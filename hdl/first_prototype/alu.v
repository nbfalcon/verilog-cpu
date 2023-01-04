`define ALU_CMD_NOP 0
`define ALU_CMD_ADD 1
`define ALU_CMD_SUB 2

module alu (input [2:0]   aluCmd,
            input [31:0]  lhs,
            input [31:0]  rhs,
            output reg [31:0] result);
   always @ (*) begin
    case (aluCmd)
      `ALU_CMD_NOP: result <= 32'bx;
      `ALU_CMD_ADD: result <= lhs + rhs;
      `ALU_CMD_SUB: result <= lhs - rhs;
    endcase
   end
endmodule

module register_file(input             clk,
                     input             reset,
                     input [4:0]       regWSel, input [31:0] regWData, input regWWe,
                     input [4:0]       reg1Sel,
                     input [4:0]       reg2Sel,
                     output reg [31:0] reg1,
                     output reg [31:0] reg2);
   reg [31:0] m_register_file [1:32];
   integer    i;

   always @ (posedge clk) begin
      if (reset) begin
         for (i = 1; i <= 32; i++) m_register_file[i] <= 0;
      end else begin
         reg1 = reg1Sel == 0 ? 32'b0 : m_register_file [reg1Sel];
         reg2 = reg2Sel == 0 ? 32'b0 : m_register_file [reg2Sel];
         if (regWWe) m_register_file [regWSel] = regWData;
      end
   end
endmodule // register_file
