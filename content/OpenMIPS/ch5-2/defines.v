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
// `define Stop 1'b1
// `define NoStop 1'b0
// `define InDelaySlot 1'b1
// `define NotInDelaySlot 1'b0
// `define Branch 1'b1
// `define NotBranch 1'b0
// `define InterruptAssert 1'b1
// `define InterruptNotAssert 1'b0
// `define TrapAssert 1'b1
// `define TrapNotAssert 1'b0

`define ZeroWord       32'h0
`define AluOpBus       7:0      // 译码阶段 aluop_o 的宽度
`define AluSelBus      3:0      // 译码阶段 alusel_o 的宽度

/******** 与指令有关 ********/
// opcode
// R-type (op?)
// arithmetic
`define EXE_AND        6'b100100
`define EXE_OR         6'b100101
`define EXE_XOR        6'b100110
`define EXE_NOR        6'b100111
// logic, immediate
`define EXE_SLL        6'b000000
`define EXE_SRL        6'b000010
`define EXE_SRA        6'b000011
// logic, register
`define EXE_SLLV       6'b000100
`define EXE_SRLV       6'b000110
`define EXE_SRAV       6'b000111
// sync & pref
`define EXE_SYNC       6'b001111
`define EXE_PREF       6'b110011

`define EXE_SPECIAL_INST 6'b000000 // SPECIAL 类指令的 op

// I-type (op)
`define EXE_ANDI       6'b001100
`define EXE_ORI        6'b001101
`define EXE_XORI       6'b001110
`define EXE_LUI        6'b001111

`define EXE_NOP        6'b000000

// AluOp 7:0, ALU function
`define EXE_AND_OP     8'b00100100
`define EXE_OR_OP      8'b00100101
`define EXE_XOR_OP     8'b00100110
`define EXE_NOR_OP     8'b00100111

`define EXE_LUI_OP     8'b01011100

`define EXE_SLL_OP     8'b01111100
`define EXE_SLLV_OP    8'b00000100
`define EXE_SRL_OP     8'b00000010
`define EXE_SRLV_OP    8'b00000110
`define EXE_SRA_OP     8'b00000011
`define EXE_SRAV_OP    8'b00000111

`define EXE_NOP_OP     8'b00000000

// AluSel 3:0, 从 ALU 结果中选取
`define EXE_RES_LOGIC  3'b001   // 逻辑运算结果
`define EXE_RES_SHIFT  3'b010   // 移位结果
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
