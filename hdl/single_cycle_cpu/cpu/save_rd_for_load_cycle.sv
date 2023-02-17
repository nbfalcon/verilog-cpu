module save_rd_for_load_cycle
  import base::*;
(
    input             clk,
    input  bit        isLoad,
    input  bit        loadResultAvail,
    input  reg_select rdIn,
    output reg_select rd
);
  reg_select rdSaved;

  always_ff @(posedge clk) begin
    rdSaved <= rdIn;
  end
  // !isLoad && resultAvail -> we got the result later; now we take out rdSaved;
  assign rd = (!isLoad || loadResultAvail) ? rdIn : rdSaved;
endmodule
