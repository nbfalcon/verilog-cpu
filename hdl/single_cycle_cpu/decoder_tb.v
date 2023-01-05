`include "cpu.v"

module decoder_tb();
   reg [31:0]  iReg;
   wire [3:0]  aluCmd;
   wire [15:0] imm;
   wire        aluBMuxUseImm;
   wire [4:0]  selA;
   wire [4:0]  selB;
   wire [4:0]  selW;
   wire        wWE;
   wire [31:0] pcImm;
   wire        jmpFlag;
   wire        haltTriggered;

   decoder dut(iReg, aluCmd, imm, aluBMuxUseImm, selA, selB, selW, wWE, pcImm, jmpFlag, haltTriggered);

   initial begin
      $monitor("iReg=%x, alu=%x, imm=%x, mux=%x, a=%x, b=%x, d=%x, we=%b, pc=%x, jmp=%b, hlt=%b",
               iReg, aluCmd, imm, aluBMuxUseImm, selA, selB, selW, wWE, pcImm, jmpFlag, haltTriggered);

      #1 iReg = 32'h000A0043;
      #2 iReg = 32'h00200002;
      #3 iReg = 32'h000A00C3;
      #4 iReg = 32'h006418C3;
      #5 iReg = 32'h000208c1;
   end
endmodule
