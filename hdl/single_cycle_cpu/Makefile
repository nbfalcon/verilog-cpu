.PHONY: sim

SIM_PROG ?= loop_count_up

sim: build/Vscpu_tb build/output.hex
	./build/Vscpu_tb

build/output.hex: asm/$(SIM_PROG).S
	cd ../../tools-ng/; stack run Genas -- ../hdl/single_cycle_cpu/asm/$(SIM_PROG).S -o ../hdl/single_cycle_cpu/build/output.hex

build/Vscpu_tb: simulator_tb/main.cc tb/*.sv inc/*.sv cpu/*.sv
	echo "Simulating $(SIM_PROG)..."
	bear -- verilator --trace --cc inc/* cpu/* tb/*  --top-module scpu_tb --exe simulator_tb/main.cc -Mdir build/ --build
	mv compile_commands.json build/

gtkwave: sim
	gtkwave build/exec.vcd

.PHONY: clean
clean:
	rm -rf build/
