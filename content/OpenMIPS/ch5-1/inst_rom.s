        .org        0x0                 # 程序从地址 0x0 开始
        .global     _start
        .set        noat                # 允许自由使用寄存器$1

_start:
        ori         $1, $0, 0x1100
        ori         $1, $1, 0x0020
        ori         $1, $1, 0x4400
        ori         $1, $1, 0x0044
