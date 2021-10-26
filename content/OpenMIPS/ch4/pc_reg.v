`include "defines.v"
module pc_reg(
              input wire                clk,
              input wire                rst,
              output reg [`InstAddrBus] pc, // 传给 IF/ID & ROM
              output reg                ce  // 传给ROM的使能信号
              );
   // ce <- rst
   always @(posedge clk)
     begin
        if (rst == `RstEnable)
          ce <= `ChipDisable;
        else
          ce <= `ChipEnable;
     end

   // pc <- ce
   always @(posedge clk)
     begin
        if (ce == `ChipDisable)
          pc <= `ZeroWord;
        else
          pc <= pc + 4;         // 按字节寻址, 指令32位, 每个时钟周期+4
     end
endmodule // pc_reg

