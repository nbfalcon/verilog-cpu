module register_file
  import base::*;
(
    input clk,
    input reset,

    input  reg_select rs1,
    input  reg_select rs2,
    output cpu_word   r1,
    output cpu_word   r2,

    input reg_select rd,
    input rdWE,
    input cpu_word dw
);
  cpu_word m_registers[1:31];
  integer  i;

  always_ff @(posedge clk)
    if (reset) begin
      for (i = 1; i < 32; i++) m_registers[i] <= 0;
    end else begin
      if (rdWE && rd != 0) m_registers[rd] <= dw;
    end
  assign r1 = rs1 == 0 ? 0 : m_registers[rs1];
  assign r2 = rs2 == 0 ? 0 : m_registers[rs2];
endmodule  // register_file
