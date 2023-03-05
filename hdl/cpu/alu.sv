module alu
  import base::*;
(
    input  cpu_word a,
    input  cpu_word b,
    output cpu_word outlo,
    input  alu_cmd  cmd
);
  always_comb
    case (cmd)
      ALU_ADDU: outlo = a + b;
      ALU_SUBU: outlo = a - b;
    endcase
endmodule
