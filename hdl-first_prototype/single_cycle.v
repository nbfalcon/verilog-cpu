`include "alu.v"

`define OP_NOP 0
`define OP_COMPUTE 1
`define OP_ADDI 2
`define OP_JUMP 3

`define OC_ADD 0
`define OC_SUB 1

module scpu_decoder(input [31:0]      iReg,
                    output reg [2:0]  aluCmd,
                    output reg [4:0]  reg1Sel,
                    output reg [4:0]  reg2Sel,
                    output reg [4:0]  regDest,
                    output reg        regDestWE,
                    output reg [14:0] imm15,
                    output reg        muxR2Imm,
                    output reg [31:0] jumpAddr,
                    output reg        doJump);
   wire [6:0] opcode;
   wire [4:0] rd, rs1, rs2;
   wire [9:0] xFunct;

   wire [14:0] iImm15;

   wire [24:0] jaPc;

   assign {xFunct, rs2, rs1, rd, opcode} = iReg; // "R-Type"
   assign iImm15 = iReg[31:17]; // "I-Type"
   assign jaPc = iReg[31:7];

   reg [2:0] aluCmdXFunct;
   always @ (*)
     case (xFunct)
       `OC_ADD: aluCmdXFunct = `ALU_CMD_ADD;
       `OC_SUB: aluCmdXFunct = `ALU_CMD_SUB;
     endcase // case (xFunct)

   always @ (*)
     case (opcode)
       `OP_NOP: aluCmd <= `ALU_CMD_NOP;
       `OP_COMPUTE: aluCmd <= aluCmdXFunct;
       `OP_ADDI: aluCmd <= `ALU_CMD_ADD;
       `OP_JUMP: aluCmd <= `ALU_CMD_NOP;
     endcase // case (opcode)

   always @ (*) begin
      reg1Sel <= rs1;
      reg2Sel <= rs2;
      regDest <= rd;

      regDestWE <= (opcode == `OP_COMPUTE || opcode == `OP_ADDI);

      imm15 <= iImm15;
      muxR2Imm <= opcode == `OP_ADDI;

      doJump <= opcode == `OP_JUMP;
      jumpAddr <= {7'b0, jaPc};
   end
endmodule

module pc_unit(input             clk, input reset,
               input [31:0]      jump, input doJump,
               output reg [31:0] pc);
  always @(posedge clk)
    if (reset) pc <= 0;
    else if (doJump) pc <= jump;
    else pc <= pc + 4;
endmodule // pc_unit

module scpu_rom(input [31:0]  pc,
                output [31:0] iReg);
   reg [31:0] memory[0:32767];
   initial $readmemh("output.hex", memory);

   assign iReg = memory[pc >> 4];
endmodule // scpu_rom

module cpu_single_cycle(input clk,
                        input reset);
   wire [31:0] jumpAddr;
   wire        doJump;
   wire [31:0] cur_pc;
   pc_unit pcu(clk, reset, jumpAddr, doJump, cur_pc);

   wire [4:0]  reg1Sel, reg2Sel, regWSel;
   wire [31:0] reg1V, reg2V, regWData;
   wire        regWWe;
   register_file regs(clk, reset, regWSel, regWData, regWWe, reg1Sel, reg2Sel, reg1V, reg2V);

   wire[31:0] iReg;
   scpu_rom rom(cur_pc, iReg);

   wire [2:0]  aluCmd;
   wire [14:0] imm15;
   wire        r2Mux;
   scpu_decoder decoder(iReg, aluCmd, reg1Sel, reg2Sel, regWSel, regWWe, imm15, r2Mux, jumpAddr, doJump);

   alu alu_u(aluCmd, reg1V, r2Mux ? imm15 : reg2V, regWData);
endmodule // cpu_single_cycle
