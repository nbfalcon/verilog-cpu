`include "base.sv"

/* verilator lint_off MULTITOP */
typedef enum {JMP_ALWAYS, JMP_LT, JMP_EQ, JMP_NEQ} jmp_cond;

module cond_jmp_mux(input        jmp_cond jmpMode,
                    input        cpu_word aluResult,
                    output bit shouldJump);
   wire signBit = aluResult[31];

   always_comb
     case (jmpMode)
       JMP_ALWAYS: shouldJump = 1;
       JMP_LT: shouldJump = signBit;
       JMP_EQ: shouldJump = aluResult == 0;
       JMP_NEQ: shouldJump = aluResult != 0;
     endcase
endmodule // cond_jmp_mux

typedef logic [15:0] cpu_half;

interface decoder_ctl(output       reg_select rs1,
                      output       reg_select rs2,
                      output       reg_select rd,
                      output logic aluUseImm,
                      output       cpu_half imm16,

                      output       alu_cmd aluCmd,
                      output logic aluWE,

                      output       jmp_cond jmpMode,
                      output       cpu_half jmpImm,
                      output logic jmpEn,

                      output       mem_mode lsMode,
                      output logic isStore,
                      output logic lsEn,

                      output logic xHalt,
                      output logic xDebugDump);
endinterface

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

module decoder(input logic [31:0] iReg,
               decoder_ctl ctl);
   wire [5:0]  opcode; // With a 5-bit opcode, we get 16-bit immediates
   wire [4:0]  rd, rs1, rs2;
   wire [10:0] rFunct;

   assign {rFunct, rs2, rs1, rd, opcode} = iReg;

   wire [15:0] iImm = iReg[31:16];

   wire lswWrMode = iImm[15];
   wire [1:0] lswMemMode = iImm[1:0];

   wire isCJmp = opcode == `OP_BLT || opcode == `OP_BEQ || opcode == `OP_BNEQ;
   wire cpu_half jImm = {rFunct, rd};

   always_comb begin
      ctl.rs1 = rs1;
      ctl.rs2 = rs2;
      ctl.rd = rd;
      ctl.imm16 = iImm;

      case (opcode)
        `OP_COMPUTE: case (rFunct)
                       0: ctl.aluCmd = ALU_ADDU;
                       1: ctl.aluCmd = ALU_SUBU;
                       default: ctl.aluCmd = 'bx; // FIXME: SIGILL here
                     endcase // case (rFunct)
        `OP_ADDI: ctl.aluCmd = ALU_ADDU;
        `OP_SUBI: ctl.aluCmd = ALU_SUBU;
        // For conditonal jumps, we don't have to enable aluWE, since that is
        // only to write to rd, which we don't want
        `OP_BLT: ctl.aluCmd = ALU_SUBU;
        `OP_BEQ: ctl.aluCmd = ALU_SUBU;
        `OP_BNEQ: ctl.aluCmd = ALU_SUBU;
        // FIXME: illegal
        // default: ctl.aluCmd = x;
      endcase // case (opcode)
      ctl.aluUseImm = opcode == `OP_ADDI || opcode == `OP_SUBI;

      case (opcode)
        `OP_JMP: ctl.jmpMode = JMP_ALWAYS;
        `OP_BLT: ctl.jmpMode = JMP_LT;
        `OP_BEQ: ctl.jmpMode = JMP_EQ;
        `OP_BNEQ: ctl.jmpMode = JMP_NEQ;
        // default: jmpMode = x;
      endcase // case (opcode)
      ctl.jmpImm = jImm;

      ctl.jmpEn = opcode == `OP_JMP || isCJmp;

      ctl.xHalt = opcode == `OP_HLT;
      ctl.xDebugDump = opcode == `OP_DEBUG_DUMPSTATE;
   end // always_comb
endmodule // decoder

// Fault coprocessor
// `define C0_FAULT_ILL 0
// `define C0_FAULT_ILLFUNCT 1
// `define C0_FAULT_STATE 2
// `define C0_FAULT_ALU 3
// `define C0_FAULT_MEMRD 4
// `define C0_FAULT_MEMWR 5
// `define C0_FAULT_MEMEX 6

// `define C0_CMD_RETURN_FROM_INTERRUPT 0
// `define C0_CMD_SETSIG 1
// `define C0_CMD_GETSIG 2
// `define C0_CMD_GET_REASON 3

// module copr0(input         clk, input reset,
//              input [31:0]  oldPc,

//              input         isCoprCommand, input [7:0] coprCommand,
//              input [4:0]   inA, input [31:0] inB, output [31:0] out,

//              input         sigIllFault,
//              output [31:0] newPc, output setPc,
//              output        haveFault);
//    reg [31:0] faultAdresses[0:`C0_FAULT_MEMEX];
//    reg [31:0] returnPc;
//    reg [4:0]  faultReason;

//    // We currently only have one fault type
//    wire       anyFault = sigIllFault;

//    always_ff @ (posedge clk) begin
//       if (sigIllFault) begin
//          faultReason <= `C0_FAULT_ILL;
//          newPc <= faultAdresses[`C0_FAULT_ILL];
//       end else if (isCoprCommand) begin
//          case (coprCommand)
//            `C0_CMD_RETURN_FROM_INTERRUPT: newPc <= returnPc;
//            `C0_CMD_SETSIG: faultAdresses[inA] <= inB;
//            `C0_CMD_GETSIG: out <= faultAdresses[inA];
//            `C0_CMD_GET_REASON: out <= faultReason;
//          endcase
//       end else newPc <= 32'bx;

//       if (anyFault) begin
//          returnPc <= oldPc;
//       end
//    end

//    assign haveFault = anyFault;
//    assign setPc = anyFault;
// endmodule // copr0

module save_rd_for_load_cycle(input     clk,
                              input bit isLoad, input bit loadResultAvail,
                              input     reg_select rdIn,
                              output    reg_select rd);
   reg_select rdSaved;

   always_ff @ (posedge clk) begin
      rdSaved <= rdIn;
   end
   // !isLoad && resultAvail -> we got the result later; now we take out rdSaved;
   assign rd = (!isLoad || loadResultAvail) ? rdIn : rdSaved;
endmodule

module scpu(input clk, input reset, output haltTriggered, output debugDump);
   // Fetch
   wire cpu_word iReg;
   wire cpu_word port2i, port2o;
   wire port2en, port2WEn;
   wire iRegAvail, port2avail;
   wire mem_mode memMode;
   ram_unit ram(.*,
                // RISCV style: sw uses r2 instead of rd to store the address
                .port2en(lsEn), .port2adr(r1), .port2WEn(isStore),
                .port2i(r2),
                .memMode(lsMode));
   wire loadResultAvail = port2avail;
   wire pc_word pc;
   pc_unit pcu(.*,
               .cycleMask(iRegAvail),
               .pcIn({16'b0, jmpImm}), .jmpFlag(shouldJump),
               .pcOut(pc));

   // Decode
   wire reg_select decRd;
   wire lsEn, isStore;
   wire mem_mode lsMode;

   wire alu_cmd aluCmd;
   wire aluWE;
   wire aluUseImm;
   wire cpu_half imm16, jmpImm;

   wire jmpEn;
   wire jmp_cond jmpMode;
   decoder_ctl ctl(.*, .rd(decRd), .xHalt(haltTriggered), .xDebugDump(debugDump));
   decoder dec(.*);

   // Execute
   wire reg_select rs1, rs2, rd;
   wire pc_word r1, r2, dw;
   register_file regs(.*,
                      .rdWE(aluWE || loadResultAvail),
                      .dw(loadResultAvail ? port2o : outlo));
   wire pc_word outlo;
   alu aluu(.*, .a(r1), .b(r2), .cmd(aluCmd));

   save_rd_for_load_cycle load_cycle(.*, .isLoad(lsEn && !isStore), .rdIn(decRd));

   wire shouldJump;
   cond_jmp_mux jmpm(.*, .aluResult(outlo));
endmodule
