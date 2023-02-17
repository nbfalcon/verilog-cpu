module multiport_ram_unit
  import base::*;
(
    input clk,
    input reset,
    input pc_word pc,
    output cpu_word iReg,
    output reg iRegAvail,

    // Second port; iReg be 0 if using this
    input port2en,
    port2WEn,
    input cpu_word port2adr,
    input cpu_word port2i,
    input mem_mode memMode,
    output cpu_word port2o,
    output reg port2avail
);
  enum {
    RAM_ST_LOAD_IREG,  // Load into iReg
    RAM_ST_LOAD_PORT2,  // Load into port2 instead of iReg; reset to IREG afterwards
    RAM_ST_WR_READ,  // Read for rewrite
    RAM_ST_WR_REWRITE  // Write for rewrite
  } state;

  cpu_word storedAddr;
  cpu_word storedWriteWord;
  mem_mode storedMemMode;

  // Intermediate value for multi-cycle writes
  cpu_word mcTemp;

  wire cpu_word overwriteWith;
  mem_nibble_wr rewrite_nibble (
      mcTemp,
      storedWriteWord,
      storedMemMode,
      storedAddr[1:0],
      overwriteWith
  );

  wire cpu_word loadNibble;
  mem_nibble_ex load_nibble (
      memory[storedAddr>>2],
      storedMemMode,
      storedAddr[1:0],
      loadNibble
  );

  cpu_word memory[0:32767];
  initial $readmemh("build/output.hex", memory);

  // Sequential stuff
  always_ff @(posedge clk) begin
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
        RAM_ST_WR_READ: mcTemp <= memory[storedAddr>>2];
        // At this point, we have read the old word at that address into
        // mcTemp, so overwriteWith is the word we need to rewrite with
        RAM_ST_WR_REWRITE: memory[storedAddr>>2] <= overwriteWith;
      endcase

      // State transition function
      case (state)
        RAM_ST_LOAD_IREG: begin
          if (port2en) state <= port2WEn ? RAM_ST_WR_READ : RAM_ST_LOAD_PORT2;
        end
        RAM_ST_LOAD_PORT2: state <= RAM_ST_LOAD_IREG;  // Next cycle we'll be fine
        RAM_ST_WR_READ: state <= RAM_ST_WR_REWRITE;
        RAM_ST_WR_REWRITE: state <= RAM_ST_LOAD_IREG;
      endcase  // case (state)
    end
  end  // always @ (posedge clk)

  always_comb
    case (state)
      RAM_ST_LOAD_IREG: begin
        iRegAvail = 1;
        port2avail = 0;

        iReg = memory[pc>>2];
        port2o = 32'bx;
      end
      RAM_ST_LOAD_PORT2: begin
        iRegAvail = 0;
        port2avail = 1;

        iReg = 0;  // Important: we *do* care that this is a NOP
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
endmodule  // ram_unit
