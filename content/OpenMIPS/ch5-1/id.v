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
   wire [5:0]                       op = inst_i[31:26];
   wire [4:0]                       op2 = inst_i[10:6];
   wire [5:0]                       op3 = inst_i[5:0];
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
           wd_o <= inst_i[15:11]; // default
           wreg_o <= `WriteDisable;
           instvalid <= `InstInvalid; // default to Invalid

           reg1_read_o <= `ReadDisable;
           reg1_addr_o <= inst_i[25:21]; // address of register 1
           reg2_read_o <= `ReadDisable;
           reg2_addr_o <= inst_i[20:16]; // address of register 2
           imm <= `ZeroWord;

           case (op)
             `EXE_ORI:
               begin
                  aluop_o <= `EXE_OR_OP;
                  alusel_o <= `EXE_RES_LOGIC;
                  wd_o <= inst_i[20:16]; // RF[rt] = RF[rs] OR imm
                  wreg_o <= `WriteEnable;
                  instvalid <= `InstValid;

                  reg1_read_o <= `ReadEnable;
                  reg2_read_o <= `ReadDisable;

                  imm <= {16'h0, inst_i[15:0]}; // zero extend
               end // case: `EXE_ORI
             default:
               begin
               end
           endcase // case (op)
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
