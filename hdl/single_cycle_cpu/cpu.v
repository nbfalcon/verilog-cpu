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
endmodule // cond_jmp_mux

module decoder(input [31:0]     iReg,
               output reg [3:0] aluCmd,
               output [15:0]    imm, output aluBMuxUseImm,
               output [4:0]     selA, output [4:0] selB,
               output [4:0]     selW, output wWE,
               output [31:0]    pcImm, output reg [2:0] jmpMode, output jmpFlag,
               output           lwAEn,
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

   assign lwAEn = opcode == `OP_LW;

   assign haltTriggered = opcode == `OP_HLT;
   assign debugDump = opcode == `OP_DEBUG_DUMPSTATE;
endmodule // decoder

// module scpu_rom(input [31:0]  pc,
//                 output [31:0] iReg);
//    reg [31:0] memory[0:32767];
//    initial $readmemh("output.hex", memory);

//    assign iReg = memory[pc >> 2];
//    assign lw = memory[lw >> 2];
// endmodule // scpu_rom

module scpu_ram_unit(input         clk, input reset,
                     input [31:0]  pc, output [31:0] iReg, output iRegAvail,

                     // Second port; iReg be 0 if using this
                     input         port2en, input [31:0] port2adr,
                     output [31:0] port2o, output port2avail);
   reg usePort2;
   reg [31:0] port2adrNextCycle;

   always @ (posedge clk) begin
      if (reset) usePort2 <= 0;
      else begin
         usePort2 <= port2en;
         port2adrNextCycle <= port2adr;
      end
   end

   reg [31:0] memory[0:32767];
   initial $readmemh("output.hex", memory);

   assign iReg = usePort2 ? 0 : memory[pc >> 2];
   assign port2o = usePort2 ? memory[port2adrNextCycle >> 2] : 0;

   assign port2avail = usePort2;
   assign iRegAvail = ~usePort2; // Currently we don't have ram that takes more than a cycle
endmodule // scpu_ram_unit

`define MEM_W 0
`define MEM_H 1
`define MEM_B 2

module mem_nibble_ex(input [31:0]      fetchedWord,
                     input [1:0]       memMode, input [1:0] byteAdr,
                     output reg [31:0] finalWord);
   always @ (*)
     case (memMode)
       `MEM_W: finalWord <= fetchedWord;
       `MEM_H: finalWord <= {16'b0, byteAdr[1] ? fetchedWord[31:16] : fetchedWord[15:0]}; // We ignore byte offset
       `MEM_B: case (byteAdr)
                 0: finalWord <= {24'b0, fetchedWord[7:0]};
                 1: finalWord <= {24'b0, fetchedWord[15:8]};
                 2: finalWord <= {24'b0, fetchedWord[23:16]};
                 3: finalWord <= {24'b0, fetchedWord[31:24]};
               endcase
     endcase
endmodule

module scpu(input clk, input reset, output haltTriggered, output debugDump);
   wire [31:0]      aluResult;

   wire [2:0]       jmpMode;
   wire             muxShouldJmp;
   cond_jmp_mux jmp_mux(jmpMode, aluResult, muxShouldJmp);

   wire [31:0] pcImm, curPc;
   wire        jmpFlag;

   wire [31:0] iReg, port2o;
   wire        lwAEn;
   wire        iRegAvail, port2avail;

   wire        actualJmup = jmpFlag & muxShouldJmp;
   pcu_unit pc_u(clk, reset, port2avail, pcImm, actualJmup, curPc);

   reg [4:0] saveRDForLWCycle;
   reg [1:0] memMode;
   reg [1:0] byteAdr;
   always @ (posedge clk) begin
      saveRDForLWCycle <= selW;
      byteAdr <= outA[1:0];
      memMode <= imm[1:0];
   end

   wire [31:0] lwNibbleOut;
   mem_nibble_ex load_nibble(port2o, memMode, byteAdr, lwNibbleOut);

   wire [15:0] imm;

   wire [4:0]       selA, selB;
   wire [31:0]      outA, outB;
   wire [4:0]       selW;
   wire             wWE;
   register_file regs_u(clk, reset, selA, selB, outA, outB,
                        port2avail ? saveRDForLWCycle : selW,
                        port2avail || wWE,
                        port2avail ? lwNibbleOut : aluResult);

   scpu_ram_unit ram_u(.clk(clk), .reset(reset),
                       .pc(curPc), .iReg(iReg), .iRegAvail(iRegAvail),
                       .port2en(lwAEn), .port2adr(outA), .port2o(port2o), .port2avail(port2avail));

   wire [3:0]  aluCmd;
   wire        aluBMuxUseImm;
   decoder decoder_u(iReg,
                     aluCmd,
                     imm, aluBMuxUseImm,
                     selA, selB, selW, wWE,
                     pcImm, jmpMode, jmpFlag,
                     lwAEn,
                     haltTriggered, debugDump);

   alu alu_u(outA, aluBMuxUseImm ? imm : outB, aluResult, aluCmd);
endmodule
