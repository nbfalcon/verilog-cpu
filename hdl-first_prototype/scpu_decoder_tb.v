`include "single_cycle.v"

module scpu_decoder_tb();
   reg [31:0]  iReg;
   wire [2:0]  aluCmd;
   wire [4:0]  reg1Sel;
   wire [4:0]  reg2Sel;
   wire [4:0]  regDest;
   wire [14:0] imm15;
   wire        muxR2Imm;

   scpu_decoder dec(iReg, aluCmd, reg1Sel, reg2Sel, regDest, imm15, muxR2Imm);

   initial begin
      $display("scpu_decoder_tb()");
      $monitor("$t %d: alu=%d, rd=%d, r1=%d, r2=%d, imm15=%d, mux=%b", $time,
               aluCmd, regDest, reg1Sel, reg2Sel, imm15, muxR2Imm);
      # 1 iReg = 32'h00000000;  // nop
      # 2 iReg = 32'h00140082;  // addi r1, r0, $10
      # 3 iReg = 32'h000A0102;  // addi r2, r0, $5
      # 4 iReg = 32'h00041181;  // addi r3, r1, r2
   end
endmodule
