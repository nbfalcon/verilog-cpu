`include "cpu.sv"

module cpu_tb();
   reg clk = 0;
   always #1 clk = !clk;

   reg reset;
   initial begin
      #0 reset = 1;
      #2 reset = 0;
   end

   wire haltWire, dumpState;
   scpu dut(clk, reset, haltWire, dumpState);

   // initial $monitor ("%d: %b", $time, clk);
   initial begin
      $dumpfile("cpu.vcd");
      $dumpvars;
      // $monitor("%d: hlt=%b, pc=%x, iReg=%x", $time, haltWire, dut.curPc, dut.iReg);
      // $monitor("%d: r3 = %d", $time, dut.regs.m_registers[3]);
   end

   // We need some more cycles <3
   initial #1600 begin
      $display("Killing off... Something went wrong");
      $finish; // This should not take more than 200 cycles
   end

   always @ (posedge haltWire) begin
      $display("%d: Finish by hlt", $time);
      $finish;
   end

   wire [31:0] r1 = dut.regs.m_registers[1],
               r2 = dut.regs.m_registers[2],
               r3 = dut.regs.m_registers[3];

   always @ (posedge dumpState) begin
      $display("%d: r1=%d, r2=%x, r3=%x, char '%c'", $time, r1, r2, r3, r2[7:0]);
   end
endmodule // cpu_tb
