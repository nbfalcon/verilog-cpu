#include "Vscpu_tb.h"
#include "verilated.h"
#include "verilated_vcd_c.h"
#include "verilatedos.h"
#include <iostream>

VerilatedVcdC *tfp;
Vscpu_tb *dutG = new Vscpu_tb();
vluint64_t currentCycle = 0;

void tfp_dump() { tfp->dump(currentCycle++); }

void dut_step() {
  dutG->clk ^= 1;
  dutG->eval();
  tfp_dump();
}

double sc_time_stamp() { return currentCycle; }

int main(int argc, char *argv[]) {
  Verilated::commandArgs(argc, argv);
  std::cout << "SCPU Simulator." << std::endl;

  Verilated::traceEverOn(true);
  tfp = new VerilatedVcdC();
  tfp->set_time_resolution("ns");
  tfp->set_time_unit("ns");
  dutG->trace(tfp, 99);
  tfp->open("build/exec.vcd");

  dutG->clk = 0;
  dutG->reset = 0;
  tfp_dump();

  dut_step();
  dut_step();

  dutG->reset = 1;
  dut_step();
  dut_step();

  dutG->reset = 0;
  dut_step();

  for (int i = 0; i < 6000 && !Verilated::gotFinish(); i++) {
    dut_step();
    dut_step();
  }

  tfp->close();

  delete dutG;
  delete tfp;

  return 0;
}
