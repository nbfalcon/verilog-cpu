module rom_region
  import base::*;
(
    mmap_region.MEM bus
);
  cpu_word rom[16383:0];
  initial $readmemh("build/output.hex", rom);

  logic [7:0] exec_high, rw_high;
  logic [13:0] exec_lo, rw_lo;
  assign {exec_high,exec_lo} = bus.address_exec[23:2];
  assign {rw_high, rw_lo} = bus.address_rw[23:2];

  // FIXME: how does verilog actually handle this? ([23:2])
  assign bus.exec_word = rom[exec_lo];
  assign bus.read_word = rom[rw_lo];
  always_ff @(posedge bus.clk) if (bus.is_write) rom[rw_lo] <= bus.write_word;
  assign bus.word_level_io = 0;

  assign bus.fault_einval = 0;
  assign bus.fault_read = 0;
  assign bus.fault_write = 0;
  assign bus.fault_exec = 0;
  // FIXME: differentiate
  assign bus.fault_address = exec_high != 0 || rw_high != 0;
endmodule
