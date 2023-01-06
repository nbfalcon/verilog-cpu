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
`define OP_LW_SW 10

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
endmodule // cond_jmp_mux

module decoder(input [31:0]     iReg,
               output reg [3:0] aluCmd,
               output [15:0]    imm, output aluBMuxUseImm,
               output [4:0]     selA, output [4:0] selB,
               output [4:0]     selW, output wWE,
               output [31:0]    pcImm, output reg [2:0] jmpMode, output jmpFlag,
               // For lw/sw, rd is the destination (either register to lw into, or the register with address to store to); rs1 is the source,
               // memMode is how to write/load; lw/sw share an encoding
               output           lwAEn, output wrAEn, output [1:0] memMode,
               output           haltTriggered, output debugDump);
   wire [5:0]  opcode; // With a 5-bit opcode, we get 16-bit immediates
   wire [4:0]  rd, rs1, rs2;
   wire [10:0] rFunct;

   assign {rFunct, rs2, rs1, rd, opcode} = iReg;

   wire [15:0] iImm = iReg[31:16];

   wire lswWrMode = iImm[15];
   wire [1:0] lswMemMode = iImm[1:0];

   always @ (*) begin
      case (opcode)
        `OP_COMPUTE: case (rFunct)
                       0: aluCmd <= `ALU_ADDU;
                       1: aluCmd <= `ALU_SUBU;
                       2: aluCmd <= `ALU_BAND;
                       default: aluCmd <= 'bx; // FIXME: SIGILL here
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

   assign memMode = lswMemMode;
   assign lwAEn = opcode == `OP_LW_SW && !lswWrMode;
   assign wrAEn = opcode == `OP_LW_SW && lswWrMode;

   assign haltTriggered = opcode == `OP_HLT;
   assign debugDump = opcode == `OP_DEBUG_DUMPSTATE;
endmodule // decoder

module scpu(input clk, input reset, output haltTriggered, output debugDump);
   wire [31:0]      aluResult;

   wire [2:0]       jmpMode;
   wire             muxShouldJmp;
   cond_jmp_mux jmp_mux(jmpMode, aluResult, muxShouldJmp);

   wire [31:0] pcImm, curPc;
   wire        jmpFlag;

   wire        actualJump = jmpFlag & muxShouldJmp;
   pcu_unit pc_u(clk, reset, iRegAvail, pcImm, actualJump, curPc);

   reg[4:0] saveRDForLWCycle;
   always @ (posedge clk)
     saveRDForLWCycle <= selW;

   wire [15:0] imm;

   wire [4:0]  selA, selB;
   wire [31:0] outA, outB;
   wire [4:0]  selW;
   wire        wWE;
   // For writes, rd stores the address, so we need to get it in outA
   // For writes, rs1 stores the input word
   register_file regs_u(.clk(clk), .reset(reset),
                        // rs1 is the address
                        .selA(wrAEn ? selW : selA), .selB(wrAEn ? selA : selB),
                        .outA(outA), .outB(outB),
                        .selW(port2avail ? saveRDForLWCycle : selW),
                        .wWE(port2avail || wWE),
                        .inW(port2avail ? port2o : aluResult));

   // sw will set rs1, which means port2i will get the proper write value; the saving is taken care of by the ram_unit
   wire [31:0] iReg, port2o;
   wire        iRegAvail, port2avail;
   ram_unit ram_u(.clk(clk), .reset(reset),
                  .pc(curPc), .iReg(iReg), .iRegAvail(iRegAvail),
                  // outA will contain either rs1 od rd, depending on wrAEn
                  .port2en(lwAEn || wrAEn), .wrEn(wrAEn),
                  .port2adr(outA),
                  // NOTE: port2i is only used for writes
                  .port2i(outB), .memMode(memMode),
                  .port2o(port2o), .port2avail(port2avail));

   wire [3:0]  aluCmd;
   wire        aluBMuxUseImm;
   wire        lwAEn, wrAEn;
   wire [1:0]  memMode;
   decoder decoder_u(iReg,
                     aluCmd,
                     imm, aluBMuxUseImm,
                     selA, selB, selW, wWE,
                     pcImm, jmpMode, jmpFlag,
                     lwAEn, wrAEn, memMode,
                     haltTriggered, debugDump);

   alu alu_u(outA, aluBMuxUseImm ? imm : outB, aluResult, aluCmd);
endmodule
