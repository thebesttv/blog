`include "defines.v"
// MEM 咋也是纯的组合逻辑...
module mem(
           input wire               rst,

           input wire [`RegAddrBus] wd_i,
           input wire               wreg_i,
           input wire [`RegBus]     wdata_i,

           output reg [`RegAddrBus] wd_o,
           output reg               wreg_o,
           output reg [`RegBus]     wdata_o
           );
   always @(*)                  // 组合逻辑
     begin
        if (rst == `RstEnable)
          begin
             wd_o    <= `NOPRegAddr;
             wreg_o  <= `WriteDisable;
             wdata_o <= `ZeroWord;
          end
        else
          begin
             wd_o    <= wd_i;
             wreg_o  <= wreg_i;
             wdata_o <= wdata_i;
          end // else: !if(rst == `RstEnable)
     end // always @ (*)
endmodule // mem
