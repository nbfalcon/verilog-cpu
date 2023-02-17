.PHONY: sim
sim: build/Vscpu_tb build/output.hex
	./build/Vscpu_tb

build/output.hex: asm/basic.S
	cd ../../tools-ng/; stack run Genas -- ../hdl/single_cycle_cpu/asm/basic.S -o ../hdl/single_cycle_cpu/build/output.hex

build/Vscpu_tb: simulator_tb/main.cc tb/*.sv inc/*.sv cpu/*.sv
	bear -- verilator --trace --cc inc/* cpu/* tb/*  --top-module scpu_tb --exe simulator_tb/main.cc -Mdir build/ --build
	mv compile_commands.json build/

gtkwave: sim
	gtkwave build/exec.vcd

.PHONY: clean
clean:
	rm -rf build/
