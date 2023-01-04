`define ALU_NOP 0
`define ALU_ADDU 1
`define ALU_SUBU 2
`define ALU_BAND 3

module alu(input [31:0]      a, input [31:0] b,
           output reg [31:0] outlo,
           input [3:0]       funct);
   always @ (*) begin
      case (funct)
        `ALU_ADDU: outlo <= a + b;
        `ALU_SUBU: outlo <= a - b;
      endcase // case (funct)
   end
endmodule // alu

module register_file(input             clk, input reset,
                     input [4:0]       selA, input [4:0] selB,
                     output reg [31:0] outA, output reg [31:0] outB,
                     input [4:0]       selW,
                     input             selWE,
                     input [31:0]      inW);
   reg [31:0] m_registers [1:32];
   integer    i;

   always @ (posedge clk) begin
     if (reset) begin
        for (i = 1; i < 32; i++) m_registers[i] = 0;
     end else begin
        outA <= selA == 0 ? 0 : m_registers[selA];
        outB <= selB == 0 ? 0 : m_registers[selB];
        if (selWE) m_registers[selW] <= inW;
     end
   end
endmodule // register_file

module pcu_unit(input             clk, input reset,
                input [31:0]      pcIn, input jmpFlag,
                output reg [31:0] pcOut);
   always @ (posedge clk) begin
      if (reset) pcOut <= 0;
      else if (jmpFlag) pcOut <= pcIn;
      else pcOut <= pcOut + 4;
   end
endmodule // pcu_unit
