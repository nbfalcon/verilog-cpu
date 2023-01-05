`include "cpu.v"

module cpu_tb();
   reg clk = 0;
   always #1 clk = !clk;

   reg reset;
   initial begin
      #0 reset = 1;
      #2 reset = 0;
   end

   wire haltWire;
   scpu dut(clk, reset, haltWire);

   // initial $monitor ("%d: %b", $time, clk);
   initial begin
      $dumpfile("cpu.vcd");
      $dumpvars;
      $monitor("%d: hlt=%b, pc=%x, iReg=%x", $time, haltWire, dut.curPc, dut.iReg);
      // $monitor("%d: r3 = %d", $time, dut.regs_u.m_registers[3]);
      #200 $finish; // This should not take more than 200 cycles
   end

   always @ (posedge haltWire) begin
      $display("Done @%d: r1 = %d, r2 = %d, r3 = %d", $time,
               dut.regs_u.m_registers[1], dut.regs_u.m_registers[2], dut.regs_u.m_registers[3]);
      $finish;
   end
endmodule // cpu_tb
