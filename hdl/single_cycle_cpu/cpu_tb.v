`include "cpu.v"

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
      // $monitor("%d: r3 = %d", $time, dut.regs_u.m_registers[3]);
   end

   initial #800 begin
      $display("Killing off... Something went wrong");
      $finish; // This should not take more than 200 cycles
   end

   always @ (posedge haltWire) begin
      $display("%d: Finish by hlt", $time);
      $finish;
   end

   always @ (posedge dumpState) begin
      $display("%d: r1=%d, r2=%x, r3=%x, char '%c'", $time,
               dut.regs_u.m_registers[1], dut.regs_u.m_registers[2], dut.regs_u.m_registers[3], dut.regs_u.m_registers[2]);
   end
endmodule // cpu_tb
