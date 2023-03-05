BUILD ?= $(PWD)/build

HDL = $(PWD)
TOOLS_NG = $(PWD)/../../tools-ng

SIM_PROG ?= copy_loop

.PHONY: sim gtkwave clean asm
sim: $(BUILD)/Vscpu_tb $(BUILD)/Genas $(BUILD)/output.hex
	$(BUILD)/Vscpu_tb

gtkwave: sim
	gtkwave $(BUILD)/exec.vcd

.PHONY: $(BUILD)/output.hex
$(BUILD)/output.hex: $(HDL)/asm/$(SIM_PROG).S
	$(BUILD)/Genas $(HDL)/asm/$(SIM_PROG).S -o $(BUILD)/output.hex

$(BUILD)/Genas:
	cd $(TOOLS_NG); stack build && stack install --local-bin-path $(BUILD)

$(BUILD)/Vscpu_tb: simulator_tb/main.cc tb/*.sv inc/*.sv cpu/*.sv
	echo "Simulating $(SIM_PROG)..."
	bear -- verilator --trace --cc inc/* cpu/* tb/*  --top-module scpu_tb --exe simulator_tb/main.cc -Mdir $(BUILD)/ --build
	mv compile_commands.json $(BUILD)/

clean:
	rm -rf $(BUILD)/
