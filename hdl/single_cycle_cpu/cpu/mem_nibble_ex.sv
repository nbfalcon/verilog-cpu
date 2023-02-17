module mem_nibble_ex
  import base::*;
(
    input cpu_word fetchedWord,
    input mem_mode memMode,
    input [1:0] byteAdr,
    output cpu_word finalWord
);
  always_comb
    case (memMode)
      MEM_W: finalWord = fetchedWord;
      MEM_H:
      finalWord = {
        16'b0, byteAdr[1] ? fetchedWord[31:16] : fetchedWord[15:0]
      };  // We ignore byte offset
      MEM_B:
      case (byteAdr)
        0: finalWord = {24'b0, fetchedWord[7:0]};
        1: finalWord = {24'b0, fetchedWord[15:8]};
        2: finalWord = {24'b0, fetchedWord[23:16]};
        3: finalWord = {24'b0, fetchedWord[31:24]};
      endcase
    endcase  // case (memMode)
endmodule  // mem_nibble_ex
