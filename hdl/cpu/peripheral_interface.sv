interface peripheral_interface
(
    input clk,
    input reset
);

import base::*;

logic [11:2] address;
mem_mode io;

logic is_write;
cpu_word write_word;
cpu_word read_word;

logic fault_mem_mode;
logic fault_invalid_config;
logic fault_address;

modport MASTER (output address, io, is_write, write_word,
                input read_word, fault_mem_mode, fault_invalid_config, fault_address);
modport PERIPHERAL (input address, io, is_write, write_word,
                    output read_word, fault_mem_mode, fault_invalid_config, fault_address);

endinterface