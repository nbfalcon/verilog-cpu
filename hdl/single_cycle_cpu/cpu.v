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

module decoder(input [31:0]     iReg,
               output reg [3:0] aluCmd,
               output [15:0]    imm, output aluBMuxUseImm,
               output [4:0]     selA, output [4:0] selB,
               output [4:0]     selW, output wWE,
               output [31:0]    pcImm, output jmpFlag,
               output           haltTriggered);
   wire [5:0]  opcode; // With a 5-bit opcode, we get 16-bit immediates
   wire [4:0]  rd, rs1, rs2;
   wire [10:0] rFunct;

   wire [15:0] iImm;

   assign {rFunct, rs2, rs1, rd, opcode} = iReg;
   assign iImm = iReg[31:16];

   always @ (*)
     case (opcode)
       `OP_NOP: aluCmd <= 0;
       `OP_COMPUTE: case (rFunct)
                      0: aluCmd <= `ALU_ADDU;
                      1: aluCmd <= `ALU_SUBU;
                      2: aluCmd <= `ALU_BAND;
                    endcase // case (rFunct)
       `OP_ADDI: aluCmd <= `ALU_ADDU;
       `OP_SUBI: aluCmd <= `ALU_SUBU;
       `OP_JMP: aluCmd <= 0;
       `OP_HLT: aluCmd <= 0;
     endcase

    assign selA = rs1;
    assign selB = rs2;
    assign selW = rd;
    assign wWE = opcode == `OP_COMPUTE || opcode == `OP_ADDI || opcode == `OP_SUBI;
    assign aluBMuxUseImm = opcode == `OP_ADDI || opcode == `OP_SUBI;
    assign imm = iImm;
    assign pcImm = {16'b0, iImm};
    assign jmpFlag = opcode == `OP_JMP;
    assign haltTriggered = opcode == `OP_HLT;
endmodule // decoder

module scpu_rom(input [31:0]  pc,
                output [31:0] iReg);
   reg [31:0] memory[0:32767];
   initial $readmemh("output.hex", memory);

   assign iReg = memory[pc >> 2];
endmodule // scpu_rom

module scpu(input clk, input reset, output haltTriggered);
   wire [31:0] pcImm, curPc;
   wire        jmpFlag;
   pcu_unit pc_u(clk, reset, pcImm, jmpFlag, curPc);

   wire [4:0]       selA, selB;
   wire [31:0]      outA, outB;
   wire [4:0]       selW;
   wire             wWE;
   wire [31:0]      inW;
   register_file regs_u(clk, reset, selA, selB, outA, outB, selW, wWE, inW);

   wire [31:0] iReg;
   scpu_rom rom_u(curPc, iReg);

   wire [3:0]  aluCmd;
   wire [15:0] imm;
   wire        aluBMuxUseImm;
   decoder decoder_u(iReg, aluCmd, imm, aluBMuxUseImm, selA, selB, selW, wWE, pcImm, jmpFlag, haltTriggered);

   alu alu_u(outA, aluBMuxUseImm ? imm : outB, inW, aluCmd);
endmodule
