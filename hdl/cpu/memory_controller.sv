module memory_controller
  import base::*;
(
    mmap_region.CONTROLLER regions[3:0],

    input  cpu_word address1,
    output cpu_word outw1,

    input clk,
    input reset,
    input mem_mode memMode2,
    input cpu_word address2,
    input port2isStore,
    input cpu_word inw2,
    output cpu_word outw2
);
  wire [5:0] address_high;
  wire [1:0] region_select, region_select_ex;
  wire [23:2] region_address;
  wire [ 1:0] byte_addr;
  assign {address_high, region_select, region_address, byte_addr} = address2;
  assign region_select_ex = address1[25:24];

  assign outw1 = gather_read_words_ex[region_select_ex];

  cpu_word read2 = gather_read_words[region_select];
  mem_nibble_ex nibble_read (
      .fetchedWord(read2),
      .memMode    (memMode2),
      .byteAdr    (byte_addr),
      .finalWord  (outw2)
  );

  cpu_word write_word;
  mem_nibble_wr nibble_write (
      .oldWord  (read2),
      .newWord  (inw2),
      .byteAdr  (byte_addr),
      .memMode  (memMode2),
      .finalWord(write_word)
  );

  genvar i;
  cpu_word gather_read_words[3:0], gather_read_words_ex[3:0];
  generate
    for (i = 0; i < 4; ++i) begin
      assign regions[i].address_exec = address1[23:2];
      assign regions[i].request_exec = region_select_ex == i;

      assign regions[i].rw_request = region_select == i;
      assign regions[i].write_word = write_word;
      assign regions[i].is_write = region_select == i && port2isStore;

      assign gather_read_words[i] = regions[i].read_word;
      assign gather_read_words_ex[i] = regions[i].exec_word;
    end
  endgenerate
endmodule
