module pc_reg(
              input wire       clk,
              input wire       rst,
              output reg [5:0] pc,
              output reg       ce
              );
   // ce
   always @(posedge clk)
     begin
        if (rst == 1'b1)
          ce <= 1'b0;
        else
          ce <= 1'b1;
     end

   // res为0后的第一个 posedge, ce拉高, 此时 pc 还是 ce == 1'b0, 为0,
   // 即从地址0开始读取. 等第二个 posedge 时, pc 才加一
   always @(posedge clk)
     begin
        if (ce == 1'b0)
          pc <= 6'h0;
        else
          pc <= pc + 1'b1;
     end
endmodule // pc_reg
