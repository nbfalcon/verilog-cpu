`include "single_cycle.v"

module cpu_tb();
   /* Make a reset that pulses once. */
   reg reset;
   initial begin
      #0 reset = 1;
      #2 reset = 0;
      #200 $finish;
   end

   reg clk = 0;
   always #1 clk = !clk;

   // reg[31:0] mockPc;
   // wire [31:0] iReg;
   // scpu_rom rom(clk, mockPc, iReg);
   // initial begin
   //    $monitor("mem %x: %x", mockPc, iReg);
   //    #0 mockPc = 0;
   //    #10 mockPc = 4;
   //    #20 mockPc = 8;
   //    #30 mockPc = 12;
   // end

   cpu_single_cycle the_cpu (clk, reset);

   initial $monitor("At time %t, r1 = %d, r2 = %d, r3 = %d",
                    $time,
                    the_cpu.regs.m_register_file[1], the_cpu.regs.m_register_file[2],
                    the_cpu.regs.m_register_file[3]);
endmodule
