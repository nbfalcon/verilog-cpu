module ram_unit
  import base::*;
(
    input  cpu_word address1,
    output cpu_word outw1,

    input clk,
    input mem_mode memMode2,
    input cpu_word address2,
    input port2isStore,
    input cpu_word inw2,
    output cpu_word outw2
);
  cpu_word memory[0:32767];
  initial $readmemh("build/output.hex", memory);

  assign outw1 = memory[address1>>2];

  wire cpu_word outw2W = memory[address2>>2];
  mem_nibble_ex nibble2_r(.fetchedWord(outw2W), .memMode(memMode2), .byteAdr(address2[1:0]), .finalWord(outw2));
  
  wire cpu_word inw2w;
  mem_nibble_wr nibble2_w(.oldWord(outw2W), .newWord(inw2), .memMode(memMode2), .byteAdr(address2[1:0]), .finalWord(inw2w));
  always_ff @(posedge clk) if (port2isStore) memory[address2>>2] <= inw2w;
endmodule
