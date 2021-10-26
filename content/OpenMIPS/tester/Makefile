CMD_CNT = 100
REGS = 1 2 3 4
MARS_OUT = mars-out
CORE_OUT = core-out

test-once: generator compare
	./generator ${CMD_CNT} ${REGS} > inst_rom.s
	java -jar Mars-registers.jar ${REGS} nc inst_rom.s > ${MARS_OUT}

	mips-linux-gnu-as -mips32 inst_rom.s -o inst_rom.o
	mips-linux-gnu-ld -T ../ram.ld inst_rom.o -o inst_rom.om
	mips-linux-gnu-objcopy -O binary inst_rom.om inst_rom.bin
	../bin2mem inst_rom.bin inst_rom.data
	-rm inst_rom.o inst_rom.om inst_rom.bin

	vvp ../ch5-2/a.out > ${CORE_OUT}

	./compare ${MARS_OUT} ${CORE_OUT} ${REGS}

generator: generator.cpp
	g++ $^ -o $@

compare: compare.cpp
	g++ $^ -o $@

.PHONY = clean

clean:
	-rm mars-out core-out
