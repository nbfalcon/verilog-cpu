/* verilator lint_off MULTITOP */
typedef logic [31:0] cpu_word;
typedef cpu_word pc_word;
typedef logic [4:0]  reg_select;

typedef enum {ALU_ADDU, ALU_SUBU} alu_cmd;

module alu(input  cpu_word a, input cpu_word b,
           output cpu_word outlo,
           input  alu_cmd cmd);
   always_comb
      case (cmd)
        ALU_ADDU: outlo = a + b;
        ALU_SUBU: outlo = a - b;
      endcase
endmodule

module register_file(input  clk, input reset,

                     input  reg_select rs1, input reg_select rs2,
                     output cpu_word r1, output cpu_word r2,

                     input  reg_select rd, input rdWE, input cpu_word dw);
   cpu_word m_registers [1:31];
   integer    i;

   always_ff @ (posedge clk)
     if (reset) begin
        for (i = 1; i < 32; i++)
          m_registers[i] <= 0;
     end else begin
        if (rdWE && rd != 0) m_registers[rd] <= dw;
     end
   assign r1 = rs1 == 0 ? 0 : m_registers[rs1];
   assign r2 = rs2 == 0 ? 0 : m_registers[rs2];
endmodule // register_file

module pc_unit(input  clk, input reset,
                input  cycleMask,
                input  pc_word pcIn, input jmpFlag,
                output pc_word pcOut);
   pc_word curPc;
   always_ff @ (posedge clk) begin
      if (reset) curPc <= 0;
      else if (cycleMask) begin
         curPc <= jmpFlag ? pcIn : curPc + 4;
      end
   end

   assign pcOut = curPc;
endmodule // pc_unit

typedef enum {MEM_W, MEM_H, MEM_B} mem_mode;

module mem_nibble_ex(input  cpu_word fetchedWord,
                     input  mem_mode memMode, input [1:0] byteAdr,
                     output cpu_word finalWord);
   always_comb
     case (memMode)
       MEM_W: finalWord = fetchedWord;
       MEM_H: finalWord = {16'b0, byteAdr[1] ? fetchedWord[31:16] : fetchedWord[15:0]}; // We ignore byte offset
       MEM_B: case (byteAdr)
                0: finalWord = {24'b0, fetchedWord[7:0]};
                1: finalWord = {24'b0, fetchedWord[15:8]};
                2: finalWord = {24'b0, fetchedWord[23:16]};
                3: finalWord = {24'b0, fetchedWord[31:24]};
              endcase
     endcase // case (memMode)
endmodule // mem_nibble_ex

// The inverse of mem_nibble_ex: replace the byteAdr'th byte/halfword/word in oldWord with the
// lower bits of newWord
module mem_nibble_wr(input  cpu_word oldWord, input cpu_word newWord,
                     input  mem_mode memMode, input [1:0] byteAdr,
                     output cpu_word finalWord);
   always_comb
     case (memMode)
       MEM_W: finalWord = newWord;
       MEM_H: finalWord = byteAdr[1]
                            ? {newWord[15:0], oldWord[15:0]}
                            : {oldWord[31:16], newWord[15:0]}; // FIXME: fault here
       MEM_B: case (byteAdr)
                 0: finalWord = {oldWord[31:8], newWord[7:0]};
                 1: finalWord = {oldWord[31:16], newWord[7:0], oldWord[7:0]};
                 2: finalWord = {oldWord[31:24], newWord[7:0], oldWord[15:0]};
                 3: finalWord = {newWord[7:0], oldWord[23:0]};
               endcase
     endcase
endmodule

module ram_unit(input  clk, input reset,
                input  pc_word pc, output cpu_word iReg, output reg iRegAvail,

                // Second port; iReg be 0 if using this
                input  port2en, port2WEn,
                input  cpu_word port2adr,
                input  cpu_word port2i, input mem_mode memMode,
                output cpu_word port2o, output reg port2avail);
   enum {
         RAM_ST_LOAD_IREG, // Load into iReg
         RAM_ST_LOAD_PORT2, // Load into port2 instead of iReg; reset to IREG afterwards
         RAM_ST_WR_READ,// Read for rewrite
         RAM_ST_WR_REWRITE // Write for rewrite
   } state;

   cpu_word storedAddr;
   cpu_word storedWriteWord;
   mem_mode  storedMemMode;

   // Intermediate value for multi-cycle writes
   cpu_word mcTemp;

   wire cpu_word overwriteWith;
   mem_nibble_wr rewrite_nibble(mcTemp, storedWriteWord, storedMemMode, storedAddr[1:0], overwriteWith);

   wire cpu_word loadNibble;
   mem_nibble_ex load_nibble(memory[storedAddr >> 2], storedMemMode, storedAddr[1:0], loadNibble);

   cpu_word memory[0:32767];
   initial $readmemh("output.hex", memory);

   // Sequential stuff
   always_ff @ (posedge clk) begin
      if (reset) state <= RAM_ST_LOAD_IREG;
      else begin
         // We'll need to keep the address around during multi-cycle memory operations
         if (state == RAM_ST_LOAD_IREG) begin
            storedAddr <= port2adr;
            storedMemMode <= memMode;
            storedWriteWord <= port2i;
         end

         // Handle writes
         case (state)
           RAM_ST_WR_READ: mcTemp <= memory[storedAddr >> 2];
           // At this point, we have read the old word at that address into
           // mcTemp, so overwriteWith is the word we need to rewrite with
           RAM_ST_WR_REWRITE: memory[storedAddr >> 2] <= overwriteWith;
         endcase

         // State transition function
         case (state)
             RAM_ST_LOAD_IREG: begin
                if (port2en) state <= port2WEn ? RAM_ST_WR_READ : RAM_ST_LOAD_PORT2;
             end
             RAM_ST_LOAD_PORT2: state <= RAM_ST_LOAD_IREG; // Next cycle we'll be fine
             RAM_ST_WR_READ: state <= RAM_ST_WR_REWRITE;
             RAM_ST_WR_REWRITE: state <= RAM_ST_LOAD_IREG;
         endcase // case (state)
      end
   end // always @ (posedge clk)

   always_comb
     case (state)
       RAM_ST_LOAD_IREG: begin
          iRegAvail = 1;
          port2avail = 0;

          iReg = memory[pc >> 2];
          port2o = 32'bx;
       end
       RAM_ST_LOAD_PORT2: begin
          iRegAvail = 0;
          port2avail = 1;

          iReg = 0; // Important: we *do* care that this is a NOP
          port2o = loadNibble;
          // port2o = memory[storedAddr >> 2];
       end
       RAM_ST_WR_READ, RAM_ST_WR_REWRITE: begin
          iRegAvail = 0;
          port2avail = 0;

          iReg = 0;
          port2o = 32'bx;
       end
     endcase
endmodule // ram_unit
