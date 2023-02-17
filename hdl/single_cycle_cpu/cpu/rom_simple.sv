module rom_simple
  import base::*;
(
    input  cpu_word address,
    output cpu_word outw
);
  cpu_word memory[0:32767];
  initial $readmemh("build/output.hex", memory);

  assign outw = memory[address >> 2];
endmodule
