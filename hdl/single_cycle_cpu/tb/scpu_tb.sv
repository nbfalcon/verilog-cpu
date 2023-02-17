module scpu_tb (
    input clk,
    input reset
);
  wire haltTriggered, debugDump;
  scpu dut (.*);

  //    // initial $monitor ("%d: %b", $time, clk);
  //    initial begin
  //       $dumpfile("cpu.vcd");
  //       $dumpvars;
  //       // $monitor("%d: hlt=%b, pc=%x, iReg=%x", $time, haltTriggered, dut.curPc, dut.iReg);
  //       // $monitor("%d: r3 = %d", $time, dut.regs.m_registers[3]);
  //    end

  initial begin
    $display("Hello from scpu_tb.sv\n");
  end

  //   always @(negedge clk) begin
  //     $display("Tick");
  //   end

  always @(posedge haltTriggered) begin
    $display("%d: Finish by hlt\n", $time);
    $finish;
  end

  wire [31:0] r1 = dut.regs.m_registers[1],
               r2 = dut.regs.m_registers[2],
               r3 = dut.regs.m_registers[3];

  always @(posedge debugDump) begin
    $display("%d: r1=%d, r2=%x, r3=%x, char '%c'\n", $time, r1, r2, r3, r2[7:0]);
  end
endmodule  // cpu_tb
