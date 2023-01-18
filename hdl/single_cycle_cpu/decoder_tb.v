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
   wire [2:0]  jmpMode;
   wire        jmpFlag;

   wire        haltTriggered, debugDump;

   decoder dut(iReg, aluCmd, imm, aluBMuxUseImm, selA, selB, selW, wWE,
               pcImm, jmpMode, jmpFlag,
               haltTriggered, debugDump);
   
   initial begin
      $monitor("iReg=%x, alu=%x, imm=%x, mux=%x, a=%x, b=%x, d=%x, we=%b, pc=%x, jmp=%b, hlt=%b",
               iReg, aluCmd, imm, aluBMuxUseImm, selA, selB, selW, wWE, pcImm, jmpFlag, haltTriggered);

      #1 iReg = 32'h000A0043;
      #2 iReg = 32'h00200002;
      #3 iReg = 32'h000A00C3;
      #4 iReg = 32'h006418C3;
      #5 iReg = 32'h000208c1;
      #6 iReg = 32'h00181046; // blt r1, r2, :loop
   end
endmodule
