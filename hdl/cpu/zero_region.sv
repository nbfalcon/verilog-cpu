module zero_region
  import base::*;
(
    mmap_region.MEM bus
);
  // FIXME: how does verilog actually handle this? ([23:2])
  assign bus.exec_word = 32'b0;
  assign bus.read_word = 32'bx;
  assign bus.word_level_io = 0;

  assign bus.fault_exec = bus.request_exec;
  assign bus.fault_einval = 0;
  assign bus.fault_read = bus.rw_request && !bus.is_write;
  assign bus.fault_write = bus.is_write;
endmodule
