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
   reg [`RegBus]                   shiftres; // 保存位移运算结果

   // logicout
   always @(*)
     begin
        if (rst == `RstEnable)
          begin
             logicout <= `ZeroWord;
          end
        else
          begin
             case (aluop_i)
               `EXE_AND_OP:
                 begin
                    logicout <= reg1_i & reg2_i;
                 end
               `EXE_OR_OP:
                 begin
                    logicout <= reg1_i | reg2_i;
                 end
               `EXE_NOR_OP:
                 begin
                    logicout <= ~(reg1_i | reg2_i);
                 end
               `EXE_XOR_OP:
                 begin
                    logicout <= reg1_i ^ reg2_i;
                 end
               default:
                 begin
                    logicout <= `ZeroWord;
                 end
             endcase // case (aluop_i)
          end // else: !if(rst == `RstEnable)
     end // always @ (*)

   // shiftres
   always @(*)
     begin
        if (rst == `RstEnable)
          begin
             shiftres <= `ZeroWord;
          end
        else
          begin
             case (aluop_i)
               `EXE_SLL_OP:
                 begin
                    shiftres <= reg2_i << reg1_i[4:0];
                 end
               `EXE_SRL_OP:
                 begin
                    shiftres <= reg2_i >> reg1_i[4:0];
                 end
               `EXE_SRA_OP:
                 begin
                    shiftres <= ({32{reg2_i[31]}} // 32 bits of sign bit
                                 << (6'd32 - {1'b0, reg1_i[4:0]}))
                      | (reg2_i >> reg1_i[4:0]);
                 end
               default:
                 begin
                    shiftres <= `ZeroWord;
                 end
             endcase // case (aluop_i)
          end
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
          `EXE_RES_SHIFT:
            begin
               wdata_o <= shiftres;
            end
          default:
            begin
               wdata_o <= `ZeroWord;
            end
        endcase // case (alusel_i)
     end // always @ (*)
endmodule // ex

