module dmaio_frontend
  import base::*;
(
    input clk,
    input reset
);

  logic request_exec;
  logic [23:0] exec_address;
  cpu_word exec_word;

endmodule
