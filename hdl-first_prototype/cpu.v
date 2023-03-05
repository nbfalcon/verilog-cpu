`include "stages.v"

module cpu(input clk);
   reg[31:0] iReg;

   stage_fetch fetch(clk, );
endmodule; // cpu
