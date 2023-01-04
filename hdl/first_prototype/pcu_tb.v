`include "single_cycle.v"

module pcu_tb();
   /* Make a reset that pulses once. */
   reg reset;
   initial begin
      #1 reset = 1;
      #2 reset = 0;
      #20 $finish;
   end

   /* Make a regular pulsing clock. */
   reg clk = 0;
   always #1 clk = !clk;

   wire [31:0] cur_pc;
   pc_unit pcu_u (clk, reset, cur_pc);

   initial $monitor("At time %t, cur_pc = %h (%0d)", $time, cur_pc, cur_pc);
endmodule // pcu_tb
