/******** 全局 ********/
`define RstEnable      1'b1
`define RstDisable     1'b0
`define ReadEnable     1'b1
`define ReadDisable    1'b0
`define WriteEnable    1'b1
`define WriteDisable   1'b0
`define ChipEnable     1'b1     // 芯片使能
`define ChipDisable    1'b0     // 芯片禁止
`define InstValid      1'b0     // 指令有效
`define InstInvalid    1'b1     // 指令无效
`define True_v         1'b1
`define False_v        1'b0

`define ZeroWord       32'h0
`define AluOpBus       7:0      // 译码阶段 aluop_o 的宽度
`define AluSelBus      3:0      // 译码阶段 alusel_o 的宽度

/******** 与指令有关 ********/
// opcode
`define EXE_ORI        6'b001101
`define EXE_NOP        6'b000000

// AluOp 7:0, ALU function
`define EXE_OR_OP      8'b00100101
`define EXE_NOP_OP     8'b00000000

// AluSel 3:0, 从 ALU 结果中选取
`define EXE_RES_LOGIC  3'b001   // 逻辑运算结果
`define EXE_RES_NOP    3'b000

/******** 与指令存储器ROM有关 ********/
`define InstBus        31:0     // ROM 数据总线
`define InstAddrBus    31:0     // ROM 地址总线
// `define InstMemNum     131072   // ROM实际大小为128KW
// `define InstMemNumLog2 17       // 实际地址总线宽度
`define InstMemNum     1024     // ROM实际大小为1024W
`define InstMemNumLog2 10       // 实际地址总线宽度

/******** 与通用寄存器Regfile有关 ********/
`define RegBus         31:0     // 寄存器数据
`define RegAddrBus     4:0      // 寄存器地址
`define RegWidth       32       // 寄存器宽度
`define DoubleRegBus   63:0
`define DoubleRegWidth 64
`define RegNum         32
`define RegNumLog2     5

`define NOPRegAddr     5'b00000
`define ZeroRegAddr    5'b00000
