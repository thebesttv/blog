`include "defines.v"
module regfile(
               input wire               clk,
               input wire               rst,
               // write
               input wire               we,
               input wire [`RegAddrBus] waddr,
               input wire [`RegBus]     wdata,
               // read1
               input wire               re1,
               input wire [`RegAddrBus] raddr1,
               output reg [`RegBus]     rdata1,
               // read2
               input wire               re2,
               input wire [`RegAddrBus] raddr2,
               output reg [`RegBus]     rdata2
               );

   reg [`RegBus]                        regs [0:`RegNum-1];

   // write
   always @(posedge clk)
     begin
        if (rst == `RstDisable)
          if ((we == `WriteEnable) && (waddr != `ZeroRegAddr))
            regs[waddr] <= wdata;
     end

   // read1, 组合逻辑
   // always @(*)
   //   begin
   //      if (rst == `RstEnable)
   //        rdata1 <= `ZeroWord;
   //      else if (raddr1 == `ZeroRegAddr)
   //        rdata1 <= `ZeroWord;
   //      else if ((raddr1 == waddr) && (we == `WriteEnable)
   //               && (re1 == `ReadEnable))
   //        rdata1 <= wdata;
   //      else if (re1 == `ReadEnable)
   //        rdata <= regs[raddr1];
   //      else
   //        rdata <= `ZeroWord;
   //   end // always @ (*)

   always @(*)
     begin
        if (rst == `RstEnable)
          rdata1 <= `ZeroWord;
        else if (re1 == `ReadDisable)
          rdata1 <= `ZeroWord;
        else                    // ReadEnable
          begin
             if (raddr1 == `ZeroRegAddr)
               rdata1 <= `ZeroWord;
             else if ((raddr1 == waddr) && (we == `WriteEnable))
               rdata1 <= wdata;
             else
               rdata1 <= regs[raddr1];
          end // else: !if(re1 == `ReadDisable)
     end // always @ (*)

   always @(*)
     begin
        if (rst == `RstEnable)
          rdata2 <= `ZeroWord;
        else if (re2 == `ReadDisable)
          rdata2 <= `ZeroWord;
        else                    // ReadEnable
          begin
             if (raddr2 == `ZeroRegAddr)
               rdata2 <= `ZeroWord;
             else if ((raddr2 == waddr) && (we == `WriteEnable))
               rdata2 <= wdata;
             else
               rdata2 <= regs[raddr2];
          end // else: !if(re2 == `ReadDisable)
     end // always @ (*)

endmodule // regfile
