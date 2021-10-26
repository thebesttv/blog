`include "defines.v"
module openmips(
                input wire            clk,
                input wire            rst,

                input wire [`RegBus]  rom_data_i, // 从指令存储器取得的指令
                output wire [`RegBus] rom_addr_o, // 指令存储器中指令的地址
                output wire           rom_ce_o    // 指令存储器使能
                );
   wire [`InstAddrBus]                pc; // 和上面的输出 rom_addr_o 是一个东西

   // IF/ID -> ID
   wire [`InstAddrBus]                id_pc_i;
   wire [`InstBus]                    id_inst_i;

   // ID -> ID/EX
   wire [`AluOpBus]                   id_aluop_o;
   wire [`AluSelBus]                  id_alusel_o;
   wire [`RegBus]                     id_reg1_o; // 操作数
   wire [`RegBus]                     id_reg2_o; // 操作数
   wire                               id_wreg_o;
   wire [`RegAddrBus]                 id_wd_o;

   // ID/EX -> EX
   wire [`AluOpBus]                   ex_aluop_i;
   wire [`AluSelBus]                  ex_alusel_i;
   wire [`RegBus]                     ex_reg1_i; // 操作数
   wire [`RegBus]                     ex_reg2_i; // 操作数
   wire                               ex_wreg_i;
   wire [`RegAddrBus]                 ex_wd_i;

   // EX -> EX/MEM
   wire [`RegBus]                     ex_wdata_o;
   wire [`RegAddrBus]                 ex_wd_o;
   wire                               ex_wreg_o;

   // EX/MAM -> MEM
   wire [`RegBus]                     mem_wdata_i;
   wire [`RegAddrBus]                 mem_wd_i;
   wire                               mem_wreg_i;

   // MEM -> MEM/WB
   wire [`RegBus]                     mem_wdata_o;
   wire [`RegAddrBus]                 mem_wd_o;
   wire                               mem_wreg_o;

   // MEM/WB -> WB
   wire [`RegBus]                     wb_wdata_i;
   wire [`RegAddrBus]                 wb_wd_i;
   wire                               wb_wreg_i;

   // ID -> Regfile
   wire                               reg1_read;
   wire [`RegAddrBus]                 reg1_addr;
   wire [`RegBus]                     reg1_data;

   wire                               reg2_read;
   wire [`RegAddrBus]                 reg2_addr;
   wire [`RegBus]                     reg2_data;

   /* 下面开始实例化啦~ */
   // pc_reg
   pc_reg pc_reg0(
                  .clk(clk),
                  .rst(rst),
                  .pc(rom_addr_o),
                  .ce(rom_ce_o)
                  );
   assign pc = rom_addr_o;

   if_id if_id0(
                .clk(clk),
                .rst(rst),
                .if_pc(rom_addr_o),
                .if_inst(rom_data_i),
                .id_pc(id_pc_i),
                .id_inst(id_inst_i)
                );

   id id0(
          .rst(rst),
          .pc_i(id_pc_i),
          .inst_i(id_inst_i),

          .reg1_read_o(reg1_read),
          .reg1_addr_o(reg1_addr),
          .reg1_data_i(reg1_data),

          .reg2_read_o(reg2_read),
          .reg2_addr_o(reg2_addr),
          .reg2_data_i(reg2_data),

          .ex_wreg_i(ex_wreg_o),
          .ex_wd_i(ex_wd_o),
          .ex_wdata_i(ex_wdata_o),

          .mem_wreg_i(mem_wreg_o),
          .mem_wd_i(mem_wd_o),
          .mem_wdata_i(mem_wdata_o),

          .aluop_o(id_aluop_o),
          .alusel_o(id_alusel_o),
          .wreg_o(id_wreg_o),
          .wd_o(id_wd_o),
          .reg1_o(id_reg1_o),
          .reg2_o(id_reg2_o)
          );

   regfile regfile0(
                    .clk(clk),
                    .rst(rst),
                    .we(wb_wreg_i),
                    .waddr(wb_wd_i),
                    .wdata(wb_wdata_i),
                    .re1(reg1_read),
                    .raddr1(reg1_addr),
                    .rdata1(reg1_data),
                    .re2(reg2_read),
                    .raddr2(reg2_addr),
                    .rdata2(reg2_data)
                    );

   id_ex id_ex0(
                .clk(clk),
                .rst(rst),
                .id_aluop(id_aluop_o),
                .id_alusel(id_alusel_o),
                .id_reg1(id_reg1_o),
                .id_reg2(id_reg2_o),
                .id_wd(id_wd_o),
                .id_wreg(id_wreg_o),

                .ex_aluop(ex_aluop_i),
                .ex_alusel(ex_alusel_i),
                .ex_reg1(ex_reg1_i),
                .ex_reg2(ex_reg2_i),
                .ex_wd(ex_wd_i),
                .ex_wreg(ex_wreg_i)
                );

   ex ex0(
          .rst(rst),

          .aluop_i(ex_aluop_i),
          .alusel_i(ex_alusel_i),
          .reg1_i(ex_reg1_i),
          .reg2_i(ex_reg2_i),
          .wd_i(ex_wd_i),
          .wreg_i(ex_wreg_i),

          .wd_o(ex_wd_o),
          .wreg_o(ex_wreg_o),
          .wdata_o(ex_wdata_o)
          );

   ex_mem ex_mem0(
                  .clk(clk),
                  .rst(rst),
                  .ex_wd(ex_wd_o),
                  .ex_wreg(ex_wreg_o),
                  .ex_wdata(ex_wdata_o),
                  .mem_wd(mem_wd_i),
                  .mem_wreg(mem_wreg_i),
                  .mem_wdata(mem_wdata_i)
                  );

   mem mem0(
            .rst(rst),
            .wd_i(mem_wd_i),
            .wreg_i(mem_wreg_i),
            .wdata_i(mem_wdata_i),

            .wd_o(mem_wd_o),
            .wreg_o(mem_wreg_o),
            .wdata_o(mem_wdata_o)
            );

   mem_wb mem_wb0(
                  .clk(clk),
                  .rst(rst),

                  .mem_wd(mem_wd_o),
                  .mem_wreg(mem_wreg_o),
                  .mem_wdata(mem_wdata_o),

                  .wb_wd(wb_wd_i),
                  .wb_wreg(wb_wreg_i),
                  .wb_wdata(wb_wdata_i)
                  );

endmodule // openmips
