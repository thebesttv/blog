`include "defines.v"
// EX是完全的组合逻辑
module ex(
          input wire               rst,

          input wire [`AluOpBus]   aluop_i,
          input wire [`AluSelBus]  alusel_i,
          input wire [`RegBus]     reg1_i,
          input wire [`RegBus]     reg2_i,
          input wire [`RegAddrBus] wd_i, // write dest addr
          input wire               wreg_i, // write enable

          output reg [`RegAddrBus] wd_o, // write dest addr
          output reg               wreg_o, // write enable
          output reg [`RegBus]     wdata_o // 多了求出的data
          );

   reg [`RegBus]                   logicout; // 保存逻辑运算结果

   // aluop_i
   always @(*)
     begin
        if (rst == `RstEnable)
          begin
             logicout <= `ZeroWord;
          end
        else
          begin
             case (aluop_i)
               `EXE_OR_OP:
                 begin
                    logicout <= reg1_i | reg2_i;
                 end
               default:
                 begin
                    logicout <= `ZeroWord;
                 end
             endcase // case (aluop_i)
          end // else: !if(rst == `RstEnable)
     end // always @ (*)

   // 根据 alusel_i 选择运算结果
   always @(*)
     begin
        wd_o <= wd_i;
        wreg_o <= wreg_i;
        case (alusel_i)
          `EXE_RES_LOGIC:
            begin
               wdata_o <= logicout;
            end
          default:
            begin
               wdata_o <= `ZeroWord;
            end
        endcase // case (alusel_i)
     end // always @ (*)
endmodule // ex

