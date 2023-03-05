// The inverse of mem_nibble_ex: replace the byteAdr'th byte/halfword/word in oldWord with the
// lower bits of newWord
module mem_nibble_wr
  import base::*;
(
    input cpu_word oldWord,
    input cpu_word newWord,
    input mem_mode memMode,
    input [1:0] byteAdr,
    output cpu_word finalWord
);
  always_comb
    case (memMode)
      MEM_W: finalWord = newWord;
      MEM_H:
      finalWord = byteAdr[1]
                            ? {newWord[15:0], oldWord[15:0]}
                            : {oldWord[31:16], newWord[15:0]}; // FIXME: fault here
      MEM_B:
      case (byteAdr)
        0: finalWord = {oldWord[31:8], newWord[7:0]};
        1: finalWord = {oldWord[31:16], newWord[7:0], oldWord[7:0]};
        2: finalWord = {oldWord[31:24], newWord[7:0], oldWord[15:0]};
        3: finalWord = {newWord[7:0], oldWord[23:0]};
      endcase
    endcase
endmodule
