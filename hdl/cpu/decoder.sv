`define OP_NOP 0
`define OP_COMPUTE 1
`define OP_JMP 2
`define OP_ADDI 3
`define OP_SUBI 4
`define OP_HLT 5
`define OP_BLT 6
`define OP_BEQ 7
`define OP_BNEQ 8
`define OP_DEBUG_DUMPSTATE 9
`define OP_LW_SW 10

module decoder
  import base::*;
(
    input cpu_word iReg,

    output reg_select rs1,
    output reg_select rs2,
    output reg_select rd,
    output cpu_half   imm16,

    output alu_cmd aluCmd,
    output logic   aluUseImm,
    output logic   rdWE,

    output jmp_cond jmpMode,
    output cpu_half jmpImm,
    output logic jmpEn,

    output logic memLoad,
    output logic memStore,
    output mem_mode lsMode,

    output logic xHalt,
    output logic xDebugDump
);
  wire [ 5:0] opcode;  // With a 5-bit opcode, we get 16-bit immediates
  wire [10:0] rFunct;

  assign {rFunct, rs2, rs1, rd, opcode} = iReg;

  wire [15:0] iImm = iReg[31:16];

  wire lswWrMode = rFunct[10];
  wire [1:0] lswMemMode = rFunct[1:0];

  wire cpu_half jImm = {rFunct, rd};

  always_comb begin
    imm16 = iImm;

    case (opcode)
      `OP_COMPUTE:
      case (rFunct)
        0: aluCmd = ALU_ADDU;
        1: aluCmd = ALU_SUBU;
        default: aluCmd = 'bx;  // FIXME: SIGILL here
      endcase  // case (rFunct)
      `OP_ADDI: aluCmd = ALU_ADDU;
      `OP_SUBI: aluCmd = ALU_SUBU;
      `OP_BLT: aluCmd = ALU_SUBU;
      `OP_BEQ: aluCmd = ALU_SUBU;
      `OP_BNEQ: aluCmd = ALU_SUBU;
      // FIXME: illegal
      // default: aluCmd = x;
    endcase  // case (opcode)
    aluUseImm = opcode == `OP_ADDI || opcode == `OP_SUBI;
    rdWE = opcode == `OP_ADDI || opcode == `OP_SUBI || opcode ==
    `OP_COMPUTE
    || (opcode == `OP_LW_SW && !lswWrMode);

    case (opcode)
      `OP_JMP:  jmpMode = JMP_ALWAYS;
      `OP_BLT:  jmpMode = JMP_LT;
      `OP_BEQ:  jmpMode = JMP_EQ;
      `OP_BNEQ: jmpMode = JMP_NEQ;
      // default: jmpMode = x;
    endcase  // case (opcode)
    jmpEn  = opcode == `OP_JMP || opcode == `OP_BLT || opcode == `OP_BEQ || opcode == `OP_BNEQ;
    jmpImm = jImm;

    case (lswMemMode)
      0: lsMode = MEM_W;
      1: lsMode = MEM_H;
      2: lsMode = MEM_B;
      3: lsMode = MEM_W;  // FIXME: SIGILL here
    endcase
    memLoad = opcode == `OP_LW_SW && !lswWrMode;
    memStore = opcode == `OP_LW_SW && lswWrMode;

    xHalt = opcode == `OP_HLT;
    xDebugDump = opcode == `OP_DEBUG_DUMPSTATE;
  end  // always_comb
endmodule  // decoder
