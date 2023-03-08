interface mmap_region (
    input clk,
    input reset
);
  import base::*;

  logic request_exec;
  logic [23:2] address_exec;
  cpu_word exec_word;

  logic rw_request;
  logic is_write;
  logic [23:2] address_rw;
  cpu_word write_word;
  // Must be continuously assigned so that write_word can be assembled using nibbles;
  // mmap_reigon doesn't itself handle mem_mode
  cpu_word read_word;
  // Drive to high if this only supports word io; read_word can be 'bx then
  logic word_level_io;

  logic fault_exec;
  // Invalid configuration; used to expose peripherals as well
  logic fault_einval;
  logic fault_read;
  logic fault_write;
  logic fault_address;

  modport MEM(
      input clk, reset, request_exec, address_rw, address_exec, rw_request, is_write, write_word,
      output exec_word, read_word, fault_read, fault_exec, fault_einval, fault_write, fault_address, word_level_io
  );

  modport CONTROLLER(
      output request_exec, address_exec, address_rw, rw_request, is_write, write_word,
      input clk, reset, exec_word, read_word, fault_read, fault_exec, fault_einval, fault_write, fault_address, word_level_io
  );
endinterface
