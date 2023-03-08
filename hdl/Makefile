BUILD ?= $(PWD)/build

HDL = $(PWD)
TOOLS_NG = $(PWD)/../tools-ng

SIM_PROG ?= copy_loop

.PHONY: sim gtkwave clean asm
sim: $(BUILD)/Vscpu_tb $(BUILD)/Genas $(BUILD)/output.hex
	echo "Simulating $(SIM_PROG)..."
	$(BUILD)/Vscpu_tb

gtkwave: sim
	gtkwave $(BUILD)/exec.vcd

.PHONY: $(BUILD)/output.hex
$(BUILD)/output.hex: $(HDL)/asm/$(SIM_PROG).S
	$(BUILD)/Genas $(HDL)/asm/$(SIM_PROG).S -o $(BUILD)/output.hex

.PHONY: rebuild_asm _del_asm
rebuild_asm: _del_asm $(BUILD)/Genas
_del_asm:
	rm -f $(BUILD)/Genas


$(BUILD)/Genas:
	cd $(TOOLS_NG) && stack build && stack install --local-bin-path $(BUILD)

$(BUILD)/Vscpu_tb: simulator_tb/main.cc tb/*.sv inc/*.sv cpu/*.sv
	verilator --trace --cc inc/* cpu/* tb/*  --top-module scpu_tb --exe simulator_tb/main.cc -Mdir $(BUILD)/ --build

clean:
	rm -rf $(BUILD)/
