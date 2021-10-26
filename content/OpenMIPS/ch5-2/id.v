`include "defines.v"
module id(
          input wire                rst,
          input wire [`InstAddrBus] pc_i,
          input wire [`InstBus]     inst_i,

          // 读取 Regfile
          // reg1
          output reg                reg1_read_o, // -> regfile.re1
          output reg [`RegAddrBus]  reg1_addr_o, // -> regfile.raddr1
          input wire [`RegBus]      reg1_data_i, // <- regfile.rdata1
          // reg2
          output reg                reg2_read_o, // -> regfile.re2
          output reg [`RegAddrBus]  reg2_addr_o, // -> regfile.raddr2
          input wire [`RegBus]      reg2_data_i, // <- regfile.rdata2

          // forwarding from EX
          input wire                ex_wreg_i,
          input wire [`RegAddrBus]  ex_wd_i,
          input wire [`RegBus]      ex_wdata_i,

          // forwarding from MEM
          input wire                mem_wreg_i,
          input wire [`RegAddrBus]  mem_wd_i,
          input wire [`RegBus]      mem_wdata_i,

          // 传到下一级
          output reg [`AluOpBus]    aluop_o,
          output reg [`AluSelBus]   alusel_o,
          output reg                wreg_o, // Write Enable
          output reg [`RegAddrBus]  wd_o,   // Write Destionation
          output reg [`RegBus]      reg1_o, // 传给ALU的操作数1
          output reg [`RegBus]      reg2_o  // 传给ALU的操作数2
          );
   /*
    ori rt, rs, imm    RF[rt] = RF[rs] OR imm
    | 31  001101  26 | 25  rs  21 | 20  rt  16 | 15  immediate  0 |
    */
   wire [5:0]                       op = inst_i[31:26]; // 指令码
   wire [4:0]                       op2 = inst_i[10:6];
   wire [5:0]                       op3 = inst_i[5:0];  // 功能码
   wire [4:0]                       op4 = inst_i[20:16];
   reg [`RegBus]                    imm;       // 指令需要的立即数
   reg                              instvalid; // 指令是否有效

   always @(*) begin
      if (rst == `RstEnable)
        begin
           aluop_o <= `EXE_NOP_OP;
           alusel_o <= `EXE_RES_NOP;
           wd_o <= `NOPRegAddr;
           wreg_o <= `WriteDisable;
           instvalid <= `InstValid; // default to valid

           // Don't read registers
           reg1_read_o <= `ReadDisable;
           reg1_addr_o <= `NOPRegAddr;
           reg2_read_o <= `ReadDisable;
           reg2_addr_o <= `NOPRegAddr;
           imm <= `ZeroWord;
        end // if (rst == `RstEnable)
      else
        begin
           aluop_o <= `EXE_NOP_OP;
           alusel_o <= `EXE_RES_NOP;
           wd_o <= inst_i[15:11]; // rd
           wreg_o <= `WriteDisable;
           instvalid <= `InstInvalid; // default to Invalid

           reg1_read_o <= `ReadDisable;
           reg1_addr_o <= inst_i[25:21]; // rs
           reg2_read_o <= `ReadDisable;
           reg2_addr_o <= inst_i[20:16]; // rt
           imm <= `ZeroWord;

           case (op)
             `EXE_SPECIAL_INST: // SPECIAL 类指令
               begin
                  case (op2)
                    5'b00000:   // and, or, xor, nor, sllv, srlv, srav
                      begin
                         case (op3)
                           `EXE_AND: // and
                             begin
                                wreg_o      <= `WriteEnable;
                                aluop_o     <= `EXE_AND_OP;
                                alusel_o    <= `EXE_RES_LOGIC;
                                reg1_read_o <= `ReadEnable;
                                reg2_read_o <= `ReadEnable;
                                instvalid   <= `InstValid;
                             end
                           `EXE_OR: // or
                             begin
                                wreg_o      <= `WriteEnable;
                                aluop_o     <= `EXE_OR_OP;
                                alusel_o    <= `EXE_RES_LOGIC;
                                reg1_read_o <= `ReadEnable;
                                reg2_read_o <= `ReadEnable;
                                instvalid   <= `InstValid;
                             end
                           `EXE_XOR: // xor
                             begin
                                wreg_o      <= `WriteEnable;
                                aluop_o     <= `EXE_XOR_OP;
                                alusel_o    <= `EXE_RES_LOGIC;
                                reg1_read_o <= `ReadEnable;
                                reg2_read_o <= `ReadEnable;
                                instvalid   <= `InstValid;
                             end
                           `EXE_NOR: // nor
                             begin
                                wreg_o      <= `WriteEnable;
                                aluop_o     <= `EXE_NOR_OP;
                                alusel_o    <= `EXE_RES_LOGIC;
                                reg1_read_o <= `ReadEnable;
                                reg2_read_o <= `ReadEnable;
                                instvalid   <= `InstValid;
                             end

                           `EXE_SLLV: // sllv
                             begin
                                wreg_o      <= `WriteEnable;
                                aluop_o     <= `EXE_SLL_OP;
                                alusel_o    <= `EXE_RES_SHIFT;
                                reg1_read_o <= `ReadEnable;
                                reg2_read_o <= `ReadEnable;
                                instvalid   <= `InstValid;
                             end
                           `EXE_SRLV: // srlv
                             begin
                                wreg_o      <= `WriteEnable;
                                aluop_o     <= `EXE_SRL_OP;
                                alusel_o    <= `EXE_RES_SHIFT;
                                reg1_read_o <= `ReadEnable;
                                reg2_read_o <= `ReadEnable;
                                instvalid   <= `InstValid;
                             end
                           `EXE_SRAV: // srav
                             begin
                                wreg_o      <= `WriteEnable;
                                aluop_o     <= `EXE_SRA_OP;
                                alusel_o    <= `EXE_RES_SHIFT;
                                reg1_read_o <= `ReadEnable;
                                reg2_read_o <= `ReadEnable;
                                instvalid   <= `InstValid;
                             end
                           `EXE_SYNC: // sync: Synchronize Shared Memory
                             begin
                                wreg_o      <= `WriteDisable; // disable writes
                                aluop_o     <= `EXE_NOP_OP;
                                alusel_o    <= `EXE_RES_NOP;
                                reg1_read_o <= `ReadDisable;
                                reg2_read_o <= `ReadEnable;
                                instvalid   <= `InstValid;
                             end
                           default:
                             begin
                             end
                         endcase // case (op3)
                      end // case: 5'b00000
                    default:
                      begin
                      end
                  endcase // case (op2)
               end // case: `EXE_SPECIAL_INST

             `EXE_ANDI:
               begin
                  wreg_o    <= `WriteEnable;
                  aluop_o   <= `EXE_AND_OP;
                  alusel_o  <= `EXE_RES_LOGIC;
                  reg1_read_o <= `ReadEnable;
                  reg2_read_o <= `ReadDisable;
                  imm <= {16'h0, inst_i[15:0]}; // zero extend
                  wd_o      <= inst_i[20:16];
                  instvalid <= `InstValid;
               end // case: `EXE_ANDI
             `EXE_ORI:
               begin
                  wreg_o    <= `WriteEnable;
                  aluop_o   <= `EXE_OR_OP;
                  alusel_o  <= `EXE_RES_LOGIC;
                  reg1_read_o <= `ReadEnable;
                  reg2_read_o <= `ReadDisable;
                  imm <= {16'h0, inst_i[15:0]}; // zero extend
                  wd_o      <= inst_i[20:16]; // RF[rt] = RF[rs] OR imm
                  instvalid <= `InstValid;
               end // case: `EXE_ORI
             `EXE_XORI:
               begin
                  wreg_o    <= `WriteEnable;
                  aluop_o   <= `EXE_XOR_OP;
                  alusel_o  <= `EXE_RES_LOGIC;
                  reg1_read_o <= `ReadEnable;
                  reg2_read_o <= `ReadDisable;
                  imm <= {16'h0, inst_i[15:0]}; // zero extend
                  wd_o      <= inst_i[20:16];
                  instvalid <= `InstValid;
               end // case: `EXE_XORI
             `EXE_LUI:
               begin
                  wreg_o    <= `WriteEnable;
                  aluop_o   <= `EXE_OR_OP;
                  alusel_o  <= `EXE_RES_LOGIC;
                  reg1_read_o <= `ReadEnable; // reg1_addr_o = 0
                  reg2_read_o <= `ReadDisable;
                  imm <= {inst_i[15:0], 16'h0};
                  wd_o      <= inst_i[20:16];
                  instvalid <= `InstValid;
               end // case: `EXE_LUI
             `EXE_PREF:
               begin
                  wreg_o      <= `WriteDisable;
                  aluop_o     <= `EXE_NOP_OP;
                  alusel_o    <= `EXE_RES_NOP;
                  reg1_read_o <= `ReadDisable;
                  reg2_read_o <= `ReadDisable;
                  instvalid   <= `InstValid;
               end
             default:
               begin
               end
           endcase // case (op)

           if (inst_i[31:21] == 0)
             begin
                case (op3)
                  `EXE_SLL:
                    begin
                       wreg_o    <= `WriteEnable;
                       aluop_o   <= `EXE_SLL_OP;
                       alusel_o  <= `EXE_RES_SHIFT;
                       reg1_read_o <= `ReadDisable;
                       reg2_read_o <= `ReadEnable;
                       imm[4:0]    <= inst_i[10:6];
                       wd_o      <= inst_i[15:11];
                       instvalid <= `InstValid;
                    end // case: `EXE_SLL
                  `EXE_SRL:
                    begin
                       wreg_o    <= `WriteEnable;
                       aluop_o   <= `EXE_SRL_OP;
                       alusel_o  <= `EXE_RES_SHIFT;
                       reg1_read_o <= `ReadDisable;
                       reg2_read_o <= `ReadEnable;
                       imm[4:0]    <= inst_i[10:6];
                       wd_o      <= inst_i[15:11];
                       instvalid <= `InstValid;
                    end // case: `EXE_SRL
                  `EXE_SRA:
                    begin
                       wreg_o    <= `WriteEnable;
                       aluop_o   <= `EXE_SRA_OP;
                       alusel_o  <= `EXE_RES_SHIFT;
                       reg1_read_o <= `ReadDisable;
                       reg2_read_o <= `ReadEnable;
                       imm[4:0]    <= inst_i[10:6];
                       wd_o      <= inst_i[15:11];
                       instvalid <= `InstValid;
                    end // case: `EXE_SRA
                  default:
                    begin
                    end
                endcase // case (op3)
             end // if (inst_i[31:21] == 0)

        end // else: !if(rst == `RstEnable)
   end // always @ (*)

   // reg1_o, 传给ALU
   always @(*)
     begin
        if (rst == `RstEnable)
          reg1_o <= `ZeroWord;
        else if ((reg1_read_o == `ReadEnable)    // Read (reg1_read_o) After
                 && (ex_wreg_i == `WriteEnable)  // Write (ex_wreg_i), and
                 && (ex_wd_i == reg1_addr_o))    // same register
          reg1_o <= ex_wdata_i;
        else if ((reg1_read_o == `ReadEnable)    // Read (reg1_read_o) After
                 && (mem_wreg_i == `WriteEnable) // Write (mem_wreg_i), and
                 && (mem_wd_i == reg1_addr_o))   // samge register
          reg1_o <= mem_wdata_i;
        else if (reg1_read_o == `ReadEnable)
          reg1_o <= reg1_data_i;
        else if (reg1_read_o == `ReadDisable)
          reg1_o <= imm;
        else
          reg1_o <= `ZeroWord;
     end // always @ (*)

   // reg2_o, 传给ALU
   always @(*)
     begin
        if (rst == `RstEnable)
          reg2_o <= `ZeroWord;
        else if ((reg2_read_o == `ReadEnable)    // Read (reg2_read_o) After
                 && (ex_wreg_i == `WriteEnable)  // Write (ex_wreg_i), and
                 && (ex_wd_i == reg2_addr_o))    // same register
          reg2_o <= ex_wdata_i;
        else if ((reg2_read_o == `ReadEnable)    // Read (reg2_read_o) After
                 && (mem_wreg_i == `WriteEnable) // Write (mem_wreg_i), and
                 && (mem_wd_i == reg2_addr_o))   // samge register
          reg2_o <= mem_wdata_i;
        else if (reg2_read_o == `ReadEnable)
          reg2_o <= reg2_data_i;
        else if (reg2_read_o == `ReadDisable)
          reg2_o <= imm;
        else
          reg2_o <= `ZeroWord;
     end // always @ (*)

endmodule // id
