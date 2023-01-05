module pc_unit(input             clk, input reset,
               output reg [31:0] pc);
  always @(posedge clk)
    if (reset) pc <= 0;
    else pc <= pc + 4;
endmodule // pc_unit

module pcu_tb();
   /* Make a reset that pulses once. */
   reg reset;
   initial begin
      #0 reset = 1;
      #5 reset = 0;
      #100 $finish;
   end

   /* Make a regular pulsing clock. */
   reg clk = 1;
   always #5 clk = !clk;

   wire [31:0] cur_pc;
   pc_unit pcu_u (clk, reset, cur_pc);

   initial $monitor("At time %t, cur_pc = %h (%0d)", $time, cur_pc, cur_pc);
endmodule // pcu_tb
