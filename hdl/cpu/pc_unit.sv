module pc_unit
  import base::*;
(
    input clk,
    input reset,

    input pc_word pcIn,
    input shouldJump,

    output pc_word pcOut
);
  pc_word curPc;
  always_ff @(posedge clk) begin
    if (reset) curPc <= 32'h01000000;
    else if (shouldJump) curPc <= curPc + pcIn;
    else curPc <= curPc + 4;
  end
  assign pcOut = curPc;
endmodule  // pc_unit
