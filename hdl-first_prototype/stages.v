`include "alu.v"

module stage_fetch(input             clk,
                   input             reset,
                   output reg [31:0] iReg,
                   output reg [31:5] memWordAddr,
                   input [31:0]      memWordResult);
   reg [31:0] pc;

   // We need a clock, since this is sequential: update of pc
   always @(posedge clk) begin
      if (reset) pc = 32'hBFC00000;
      else begin
         memWordAddr = pc;
         iReg = memWordResult;
         pc = pc + 4;
      end
   end
endmodule // stage_fetch

`define OP_NOP 0
`define OP_COMPUTE 1

`define OC_ADD 0
`define OC_SUB 1

module stage_decode(input [31:0]     iReg,
                    output reg [2:0] aluCmd,
                    output reg [4:0] reg1Sel,
                    output reg [4:0] reg2Sel,
                    output reg [4:0] regDest);
   wire [6:0] opcode;
   wire [4:0] rd, rs1, rs2;
   wire [9:0] xFunct;

   assign {xFunct, rS2, rs1, rD, opcode} = iReg; // "R-Type"

   reg [2:0] aluCmdXFunct;
   always @ (*)
     case (xFunct)
       `OC_ADD: aluCmdXFunct = `ALU_CMD_ADD;
       `OC_SUB: aluCmdXFunct = `ALU_CMD_SUB;
     endcase // case (xFunct)

   always @ (*)
     case (opcode)
       `OP_NOP: aluCmd = `ALU_CMD_NOP;
       `OP_COMPUTE: aluCmd = aluCmdXFunct;
     endcase
endmodule

module stage_execute(input       clk,
                     input       reset,
                     input [2:0] aluCmd,
                     input [4:0] reg1Sel,
                     input [4:0] reg2Sel,
                     input [4:0] regDest);
   wire [31:0] operand1, operand2, aluResult;
   register_file regs(clk, reset, regDest, aluResult, reg1Sel, reg2Sel, operand1, operand2);

   alu myAlu(aluCmd, operand1, operand2, aluResult);
endmodule
