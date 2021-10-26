`include "defines.v"
module inst_rom(
                input wire                ce,
                input wire [`InstAddrBus] addr,
                output reg [`InstBus]     inst
                );
   // 按字取指
   reg [`InstBus]                         inst_mem [0:`InstMemNum-1];

   initial $readmemh("inst_rom.data", inst_mem);

   always @(*)
     begin
        if (ce == `ChipDisable)
          begin
             inst <= `ZeroWord;
          end
        else
          begin
             // 地址总大小: 128KW = 2^17W = 2^19B
             // 地址位: `InstMemNumLog2 + 2 = 19位
             // 地址位范围: [`InstMemNumLog2+2-1 : 0]
             // 由于这里按字取指, 不需要最后两位
             inst <= inst_mem[addr[`InstMemNumLog2+1 : 2]];
          end
     end // always @ (*)
endmodule // inst_rom

