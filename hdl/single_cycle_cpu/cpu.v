`include "base.v"

`define OP_NOP 0
`define OP_COMPUTE 1
`define OP_JMP 2
`define OP_ADDI 3
`define OP_SUBI 4
`define OP_HLT 5
`define OP_BLT 6
`define OP_BEQ 7
`define OP_BNEQ 8
`define OP_DEBUG_DUMPSTATE 9
`define OP_LW 10

`define JMP_ALWAYS 0
`define JMP_LT 1
`define JMP_EQ 2
`define JMP_NEQ 3

module cond_jmp_mux(input [2:0]  jmpMode,
                    input [31:0] aluResult,
                    output reg   shouldJmp);
   wire signBit = aluResult[31];

   always @ (*)
     case (jmpMode)
       `JMP_ALWAYS: shouldJmp <= 1;
       `JMP_LT: shouldJmp <= signBit;
       `JMP_EQ: shouldJmp <= aluResult == 0;
       `JMP_NEQ: shouldJmp <= aluResult != 0;
     endcase
endmodule

module decoder(input [31:0]     iReg,
               output reg [3:0] aluCmd,
               output [15:0]    imm, output aluBMuxUseImm,
               output [4:0]     selA, output [4:0] selB,
               output [4:0]     selW, output wWE,
               output [31:0]    pcImm, output reg [2:0] jmpMode, output jmpFlag,
               output           haltTriggered, output debugDump);
   wire [5:0]  opcode; // With a 5-bit opcode, we get 16-bit immediates
   wire [4:0]  rd, rs1, rs2;
   wire [10:0] rFunct;

   wire [15:0] iImm;

   assign {rFunct, rs2, rs1, rd, opcode} = iReg;
   assign iImm = iReg[31:16];

   always @ (*) begin
      case (opcode)
        `OP_COMPUTE: case (rFunct)
                       0: aluCmd <= `ALU_ADDU;
                       1: aluCmd <= `ALU_SUBU;
                       2: aluCmd <= `ALU_BAND;
                     endcase // case (rFunct)
        `OP_ADDI: aluCmd <= `ALU_ADDU;
        `OP_SUBI: aluCmd <= `ALU_SUBU;
        `OP_BLT: aluCmd <= `ALU_SUBU;
        `OP_BEQ: aluCmd <= `ALU_SUBU;
        `OP_BNEQ: aluCmd <= `ALU_SUBU;
        default: aluCmd <= 'bx;
      endcase // case (opcode)

      case (opcode)
        `OP_JMP: jmpMode <= `JMP_ALWAYS;
        `OP_BLT: jmpMode <= `JMP_LT;
        `OP_BEQ: jmpMode <= `JMP_EQ;
        `OP_BNEQ: jmpMode <= `JMP_NEQ;
        default: jmpMode <= 'bx;
      endcase
   end // always @ (*)

   wire isCJmp = opcode == `OP_BLT || opcode == `OP_BEQ || opcode == `OP_BNEQ;

   assign pcImm = {16'b0, iImm};
   assign jmpFlag = opcode == `OP_JMP || isCJmp;

   assign selA = isCJmp ? rd : rs1;
   assign selB = isCJmp ? rs1 : rs2;

   assign selW = rd;
   assign wWE = opcode == `OP_COMPUTE || opcode == `OP_ADDI || opcode == `OP_SUBI;
   assign aluBMuxUseImm = opcode == `OP_ADDI || opcode == `OP_SUBI;
   assign imm = iImm;

   assign haltTriggered = opcode == `OP_HLT;
   assign debugDump = opcode == `OP_DEBUG_DUMPSTATE;
endmodule // decoder

module scpu_rom(input [31:0]  pc,
                output [31:0] iReg);
   reg [31:0] memory[0:32767];
   initial $readmemh("output.hex", memory);

   assign iReg = memory[pc >> 2];
   assign lw = memory[lw >> 2];
endmodule // scpu_rom

module scpu(input clk, input reset, output haltTriggered, output debugDump);
   wire [31:0]      aluResult;
   wire             aluSign = aluResult[31];

   wire [2:0]       jmpMode;
   wire             muxShouldJmp;
   cond_jmp_mux jmp_mux(jmpMode, aluResult, muxShouldJmp);

   wire [31:0] pcImm, curPc;
   wire        jmpFlag;

   wire        actualJmup = jmpFlag & muxShouldJmp;
   pcu_unit pc_u(clk, reset, pcImm, actualJmup, curPc);

   wire [31:0] iReg;
   scpu_rom rom_u(curPc, iReg);

   wire [4:0]       selA, selB;
   wire [31:0]      outA, outB;
   wire [4:0]       selW;
   wire             wWE;
   register_file regs_u(clk, reset, selA, selB, outA, outB, selW, wWE, aluResult);

   wire [3:0]  aluCmd;
   wire [15:0] imm;
   wire        aluBMuxUseImm;
   decoder decoder_u(iReg,
                     aluCmd,
                     imm, aluBMuxUseImm,
                     selA, selB, selW, wWE,
                     pcImm, jmpMode, jmpFlag,
                     haltTriggered, debugDump);

   alu alu_u(outA, aluBMuxUseImm ? imm : outB, aluResult, aluCmd);
endmodule
