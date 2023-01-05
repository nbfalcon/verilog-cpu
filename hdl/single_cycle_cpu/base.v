`define ALU_NOP 0
`define ALU_ADDU 1
`define ALU_SUBU 2
`define ALU_BAND 3

module alu(input [31:0]      a, input [31:0] b,
           output reg [31:0] outlo,
           input [3:0]       funct);
   always @ (*)
      case (funct)
        `ALU_ADDU: outlo <= a + b;
        `ALU_SUBU: outlo <= a - b;
      endcase
endmodule

module register_file(input         clk, input reset,
                     input [4:0]   selA, input [4:0] selB,
                     output [31:0] outA, output [31:0] outB,
                     input [4:0]   selW, input wWE, input [31:0] inW);
   reg [31:0] m_registers [1:31];
   integer    i;

   always @ (posedge clk)
     if (reset) begin
        for (i = 1; i < 32; i++)
          m_registers[i] <= 0;
     end else begin
        if (wWE && selW != 0) m_registers[selW] <= inW;
     end
   assign outA = selA == 0 ? 0 : m_registers[selA];
   assign outB = selB == 0 ? 0 : m_registers[selB];
endmodule // register_file

module pcu_unit(input         clk, input reset,
                input [31:0]  pcIn, input jmpFlag,
                output [31:0] pcOut);
   reg [31:0] curPc;
   always @ (posedge clk) begin
      if (reset) curPc <= 0;
      else if (jmpFlag) curPc <= pcIn;
      else curPc <= curPc + 4;
   end

   assign pcOut = curPc;
endmodule // pcu_unit
