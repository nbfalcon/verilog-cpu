`include "base.v"

`define OP_NOP 0
`define OP_COMPUTE 1
`define OP_JMP 2
`define OP_ADDI 3
`define OP_SUBI 4

module decoder(input [31:0]      iReg,
               output reg [3:0]  aluCmd,
               output reg [15:0] imm, output reg aluBMuxUseImm,
               output reg [4:0]  selA, output reg [4:0] selB,
               output reg [4:0]  selW, output reg wWE,
               output reg [31:0] pcImm, output reg jmpFlag);
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
     endcase

   always @ (*) begin
      selA <= rs1;
      selB <= rs2;
      selW <= rd;

      wWE <= opcode == `OP_COMPUTE;
      aluBMuxUseImm <= opcode == `OP_ADDI || opcode == `OP_SUBI;

      pcImm <= iImm;
      jmpFlag <= opcode == `OP_JMP;
   end
endmodule // decoder

module scpu_rom(input [31:0]  pc,
                output [31:0] iReg);
   reg [31:0] memory[0:32767];
   initial $readmemh("output.hex", memory);

   assign iReg = memory[pc >> 4];
endmodule // scpu_rom

module scpu(input clk, input reset);
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
   decoder decoder_u(iReg, aluCmd, imm, aluBMuxUseImm, selA, selB, selW, wWE, pcImm, jmpFlag);

   alu alu_u(outA, aluBMuxUseImm ? imm : outB, inW, aluCmd);
endmodule
