module cond_jmp_mux
  import base::*;
(
    input  jmp_cond jmpMode,
    input  cpu_word aluResult,
    output bit      shouldJump
);
  wire signBit = aluResult[31];

  always_comb
    case (jmpMode)
      JMP_ALWAYS: shouldJump = 1;
      JMP_LT: shouldJump = signBit;
      JMP_EQ: shouldJump = aluResult == 0;
      JMP_NEQ: shouldJump = aluResult != 0;
    endcase
endmodule  // cond_jmp_mux
