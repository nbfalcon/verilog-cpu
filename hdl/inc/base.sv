package base;
  typedef logic [15:0] cpu_half;
  typedef logic [31:0] cpu_word;

  typedef cpu_word pc_word;
  typedef logic [4:0] reg_select;

  typedef enum {
    ALU_ADDU,
    ALU_SUBU
  } alu_cmd;
  
  typedef enum {
    MEM_W,
    MEM_H,
    MEM_B
  } mem_mode;

  typedef enum {
  JMP_ALWAYS,
  JMP_LT,
  JMP_EQ,
  JMP_NEQ
} jmp_cond;

endpackage
