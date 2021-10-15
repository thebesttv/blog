module inst_fetch_tb();
   reg CLOCK;
   reg rst;
   wire [31:0] inst;            // instruction fetched

   initial begin
      CLOCK = 1'b0;
      forever #10 CLOCK = ~CLOCK;
   end

   initial begin
      rst = 1'b1;
      #195 rst = 1'b0;
      #1000 $stop;
   end

   inst_fetch inst_fetch0(.rst(rst),
                          .clk(CLOCK),
                          .inst(inst));

endmodule // inst_fetch_tb
