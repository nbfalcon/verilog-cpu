module ram_unit
  import base::*;
(
    input  cpu_word address1,
    output cpu_word outw1,

    input clk,
    input cpu_word address2,
    input port2isStore,
    input cpu_word inw2,
    output cpu_word outw2
);
  cpu_word memory[0:32767];
  initial $readmemh("build/output.hex", memory);

  assign outw1 = memory[address1>>2];
  assign outw2 = memory[address2>>2];

  always_ff @(posedge clk) if (port2isStore) memory[address2>>2] <= inw2;
endmodule
