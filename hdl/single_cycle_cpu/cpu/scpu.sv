module scpu
  import base::*;
(
    input  clk,
    input  reset,
    output logic haltTriggered,
    output logic debugDump
);
  wire muxJump;
  cond_jmp_mux cJmp(.*, .aluResult(outlo), .shouldJump(muxJump));
  
  wire pc_word pc;
  pc_unit pcu (
      .*, .pcIn(32'(jmpImm)), .shouldJump(jmpEn && muxJump), .pcOut(pc)
  );
  wire cpu_word iReg;
  rom_simple rom(.address(pc), .outw(iReg));

  // Decode
  wire reg_select rs1;
  wire reg_select rs2;
  wire reg_select rd;
  wire cpu_half imm16;

  wire alu_cmd aluCmd;
  wire logic   aluUseImm;
  wire logic   aluWE;

  wire jmp_cond jmpMode;
  wire cpu_half jmpImm;
  wire logic jmpEn;

  wire logic lsEn;
  wire mem_mode lsMode;
  wire logic isStore;

  wire logic xHalt;
  wire logic xDebugDump;
  decoder dec (.*);

  // Execute
  wire cpu_word r1, r2;
  register_file regs (
      .*,
      .rs1 (rs1),
      .rs2 (rs2),
      .rd  (rd),
      .rdWE(aluWE),
      .dw  (outlo)
  );
  wire cpu_word outlo;
  alu aluu (
      .*,
      .a  (r1),
      .b  (aluUseImm ? 32'(imm16) : r2),
      .cmd(aluCmd)
  );

  // Something something confusing interplay with verilator; debugDump is one cycle behind otherwise
  always_ff @(posedge clk) begin
    haltTriggered <= xHalt;
    debugDump <= xDebugDump;
  end
endmodule
