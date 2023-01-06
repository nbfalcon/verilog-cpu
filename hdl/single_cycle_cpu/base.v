`define ALU_NOP 0
`define ALU_ADDU 1
`define ALU_SUBU 2
`define ALU_BAND 3

module alu(input [31:0]      a, input [31:0] b,
           output reg [31:0] outlo,
           input [3:0]       funct);
   always @ (*)
      case (funct)
        `ALU_ADDU: outlo <= a + b;
        `ALU_SUBU: outlo <= a - b;
      endcase
endmodule

module register_file(input         clk, input reset,
                     input [4:0]   selA, input [4:0] selB,
                     output [31:0] outA, output [31:0] outB,
                     input [4:0]   selW, input wWE, input [31:0] inW);
   reg [31:0] m_registers [1:31];
   integer    i;

   always @ (posedge clk)
     if (reset) begin
        for (i = 1; i < 32; i++)
          m_registers[i] <= 0;
     end else begin
        if (wWE && selW != 0) m_registers[selW] <= inW;
     end
   assign outA = selA == 0 ? 0 : m_registers[selA];
   assign outB = selB == 0 ? 0 : m_registers[selB];
endmodule // register_file

module pcu_unit(input         clk, input reset,
                input         cycleMask,
                input [31:0]  pcIn, input jmpFlag,
                output [31:0] pcOut);
   reg [31:0] curPc;
   always @ (posedge clk) begin
      if (reset) curPc <= 0;
      else if (cycleMask) begin
         curPc <= jmpFlag ? pcIn : curPc + 4;
      end
   end

   assign pcOut = curPc;
endmodule // pcu_unit

`define MEM_W 0
`define MEM_H 1
`define MEM_B 2

module mem_nibble_ex(input [31:0]      fetchedWord,
                     input [1:0]       memMode, input [1:0] byteAdr,
                     output reg [31:0] finalWord);
   always @ (*)
     case (memMode)
       `MEM_W: finalWord <= fetchedWord;
       `MEM_H: finalWord <= {16'b0, byteAdr[1] ? fetchedWord[31:16] : fetchedWord[15:0]}; // We ignore byte offset
       `MEM_B: case (byteAdr)
                 0: finalWord <= {24'b0, fetchedWord[7:0]};
                 1: finalWord <= {24'b0, fetchedWord[15:8]};
                 2: finalWord <= {24'b0, fetchedWord[23:16]};
                 3: finalWord <= {24'b0, fetchedWord[31:24]};
               endcase
     endcase
endmodule // mem_nibble_ex

// The inverse of mem_nibble_ex: replace the byteAdr'th byte/halfword/word in oldWord with the
// lower bits of newWord
module mem_nibble_wr(input [31:0]      oldWord, input [31:0] newWord,
                     input [1:0]       memMode, input [1:0] byteAdr,
                     output reg [31:0] finalWord);
   always @ (*)
     case (memMode)
       `MEM_W: finalWord <= newWord;
       `MEM_H: finalWord <= byteAdr[1]
                            ? {newWord[15:0], oldWord[15:0]}
                            : {oldWord[31:16], newWord[15:0]}; // FIXME: fault here
       `MEM_B: case (byteAdr)
                 0: finalWord <= {oldWord[31:8], newWord[7:0]};
                 1: finalWord <= {oldWord[31:16], newWord[7:0], oldWord[7:0]};
                 2: finalWord <= {oldWord[31:24], newWord[7:0], oldWord[15:0]};
                 3: finalWord <= {newWord[7:0], oldWord[23:0]};
               endcase
     endcase
endmodule

// Load into iReg
`define RAM_ST_LOAD_IREG 0
// Load into port2 instead of iReg; reset to IREG afterwards
`define RAM_ST_LOAD_PORT2 1
// Read for rewrite
`define RAM_ST_WR_READ 2
// Write for rewrite
`define RAM_ST_WR_REWRITE 3

module ram_unit(input             clk, input reset,
                input [31:0]      pc, output reg [31:0] iReg, output reg iRegAvail,

                     // Second port; iReg be 0 if using this
                input             port2en, wrEn,
                input [31:0]      port2adr,
                input [31:0]      port2i, input [1:0] memMode,
                output reg [31:0] port2o, output reg port2avail);
   reg [1:0] state;

   reg [31:0] storedAddr;
   reg [31:0] storedWriteWord;
   reg [1:0]  storedMemMode;

   // Intermediate value for multi-cycle writes
   reg [31:0] mcTemp;

   wire [31:0] overwriteWith;
   mem_nibble_wr rewrite_nibble(mcTemp, storedWriteWord, storedMemMode, storedAddr[1:0], overwriteWith);

   wire [31:0] loadNibble;
   mem_nibble_ex load_nibble(memory[storedAddr >> 2], storedMemMode, storedAddr[1:0], loadNibble);

   reg [31:0] memory[0:32767];
   initial $readmemh("output.hex", memory);

   // Sequential stuff
   always @ (posedge clk) begin
      if (reset) state <= `RAM_ST_LOAD_IREG;
      else begin
         // We'll need to keep the address around during multi-cycle memory operations
         if (state == `RAM_ST_LOAD_IREG) begin
            storedAddr <= port2adr;
            storedMemMode <= memMode;
            storedWriteWord <= port2i;
         end

         // Handle writes
         case (state)
           `RAM_ST_WR_READ: mcTemp <= memory[storedAddr >> 2];
           // At this point, we have read the old word at that address into
           // mcTemp, so overwriteWith is the word we need to rewrite with
           `RAM_ST_WR_REWRITE: memory[storedAddr >> 2] <= overwriteWith;
         endcase

         // State transition function
         case (state)
             `RAM_ST_LOAD_IREG: begin
                if (port2en) state <= wrEn ? `RAM_ST_WR_READ : `RAM_ST_LOAD_PORT2;
             end
             `RAM_ST_LOAD_PORT2: state <= `RAM_ST_LOAD_IREG; // Next cycle we'll be fine
             `RAM_ST_WR_READ: state <= `RAM_ST_WR_REWRITE;
             `RAM_ST_WR_REWRITE: state <= `RAM_ST_LOAD_IREG;
         endcase // case (state)
      end
   end // always @ (posedge clk)

   // Output as a combinatorial function of the state
   always @ (*)
     case (state)
       `RAM_ST_LOAD_IREG: begin
          iRegAvail <= 1;
          port2avail <= 0;

          iReg <= memory[pc >> 2];
          port2o <= 32'bx;
       end
       `RAM_ST_LOAD_PORT2: begin
          iRegAvail <= 0;
          port2avail <= 1;

          iReg <= 0; // Important: we *do* care that this is a NOP
          port2o <= loadNibble;
          // port2o <= memory[storedAddr >> 2];
       end
       `RAM_ST_WR_READ, `RAM_ST_WR_REWRITE: begin
          iRegAvail <= 0;
          port2avail <= 0;

          iReg <= 0;
          port2o <= 32'bx;
       end
     endcase
endmodule // ram_unit
