#include <bits/stdc++.h>
using namespace std;

/*
  R-type:
  CMD rd, rs, rt: and, or, xor, nor, (last 4 bits) sllv, srlv, srav
  CMD rd, rt, sa: sll, srl, sra
  I-type:
  CMD rt, rs, imm: andi, ori, xori
  CMD rt, imm: lui
 */

const char *Rlogic[] = {
    "and", "or", "xor", "nor"
};
const char *Rshift1[] = {
    "sllv", "srlv", "srav"
};
const char *Rshift2[] = {
    "sll", "srl", "sra"
};
const char *Ilogic[] = {
    "andi", "ori", "xori"
};
const char *Isingle[] = {
    "lui"
};

int ins_cnt;
vector<int> registers;

unsigned rand_u(unsigned max) { // [0, max)
    return 1.0 * rand() / RAND_MAX * max;
}

int rand_reg() {
    return registers[rand_u(registers.size())];
}

const char *rand_name(int n, const char *v[]) {
    return v[rand_u(n)];
}

void rlogic_single() {
    printf("%s $%d, $%d, $%d", rand_name(4, Rlogic),
           rand_reg(), rand_reg(), rand_reg());
}
void rshift1_single() {
    printf("%s $%d, $%d, $%d", rand_name(3, Rshift1),
           rand_reg(), rand_reg(), rand_reg());
}
void rshift2_single() {
    printf("%s $%d, $%d, %u", rand_name(3, Rshift2),
           rand_reg(), rand_reg(), rand_u(32));
}
void ilogic_single() {
    printf("%s $%d, $%d, 0x%04x", rand_name(3, Ilogic),
           rand_reg(), rand_reg(), rand_u(1 << 16));
}
void isingle_single() {
    printf("%s $%d, 0x%04x", rand_name(1, Isingle),
           rand_reg(), rand_u(1 << 16));
}

void lui(int rt, uint16_t imm) {
    printf("lui $%d, 0x%04x\n", rt, imm);
}
void ori(int rt, int rs, uint16_t imm) {
    printf("ori $%d, $%d, 0x%04x\n", rt, rs, imm);
}

void generate_single() {
    int op = rand_u(5);
    if (op == 0) {
        rlogic_single();
    } else if (op == 1) {
        rshift1_single();
    } else if (op == 2) {
        rshift2_single();
    } else if (op == 3) {
        ilogic_single();
    } else {
        isingle_single();
    }
    putchar('\n');
}

void init() {
    for (int x : registers) lui(x, 0);
    lui(1, 0x1234);
    ori(1, 1, 0x5678);

    lui(1, 0);
}

int main(int argc, char *argv[]) {
    srand(time(NULL));
    ins_cnt = atoi(argv[1]);
    for (int i = 2; i < argc; ++i) {
        registers.push_back(atoi(argv[i]));
    }
    sort(registers.begin(), registers.end());

#ifdef DEBUG
    printf("Instructions: %d\n", ins_cnt);
    printf("Regs:");
    for (int x : registers)
        printf(" %d", x);
    putchar('\n');
#endif

    printf(".org 0x0\n.global _start\n.set noat\n_start:\n");
    init();

    for (int i = 1; i <= ins_cnt; ++i) {
#ifdef DEBUG
        printf("%3d: ", i);
#endif
        generate_single();
    }
}
