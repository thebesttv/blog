#include <bits/stdc++.h>
using namespace std;

int ins_cnt;
vector<int> registers;

unsigned rand_u(unsigned max) { // [0, max)
    return 1.0 * rand() / RAND_MAX * max;
}
unsigned rand_i(unsigned bits) {  // random immediate
    return 1.0 * rand() / RAND_MAX * (1ll << bits);
}

int rand_r() {
    return registers[rand_u(registers.size())];
}

class Operator {
public:
    explicit Operator(const string &s) {
        if (s[0] == 'r') {
            reg = true;
            bits = 0;
#ifdef DEBUG
            printf(" r");
#endif
        } else {
            reg = false;
            sscanf(s.c_str(), "i(%u)", &bits);
#ifdef DEBUG
            printf(" i(%u)", bits);
#endif
        }
    }
    string get() {
        char res[12];
        if (reg) {
            sprintf(res, "$%d", rand_r());
        } else {
            sprintf(res, "0x%X", rand_i(bits));
        }
        return string(res);
    }
private:
    bool reg;
    unsigned bits;
};

class Instruction {
public:
    explicit Instruction(const string &s) {
        istringstream ss(s);
        ss >> name;
        string t;
#ifdef DEBUG
        printf("INS: %s", name.c_str());
#endif
        while (ss >> t) {
            ops.push_back(Operator(t));
        }
#ifdef DEBUG
        putchar('\n');
#endif
    }
    string get() {
        string s = name + "\t";
        bool first = true;
        for (auto op : ops) {
            if (first) first = false;
            else s += ", ";
            s += op.get();
        }
        return s;
    }
private:
    string name;
    vector<Operator> ops;
};

vector<Instruction> ins;

void lui(int rt, uint16_t imm) {
    printf("lui\t$%d, 0x%04x\n", rt, imm);
}
void ori(int rt, int rs, uint16_t imm) {
    printf("ori\t$%d, $%d, 0x%04x\n", rt, rs, imm);
}
void mthi(int rs) {
    printf("mthi\t$%d\n", rs);
}
void mtlo(int rs) {
    printf("mtlo\t$%d\n", rs);
}

void init() {
    FILE *fp = fopen("instructions.txt", "r");
    char buf[1024];
    while (fgets(buf, sizeof(buf), fp)) {
        int n = strlen(buf); buf[--n] = 0;
        if (!n) continue;
        ins.push_back(Instruction(buf));
    }

    for (int x : registers) lui(x, 0);
    mthi(0);
    mtlo(0);

    lui(1, 0x1234);
    ori(1, 1, 0x5678);

    lui(1, 0);
}

string generate_single() {
    return ins[rand_u(ins.size())].get();
}

int main(int argc, char *argv[]) {
    srand(time(NULL));
    ins_cnt = atoi(argv[1]);
    for (int i = 2; i < argc; ++i) {
        // ignore hi lo registers
        if (isdigit(argv[i][0])) {
            registers.push_back(atoi(argv[i]));
        }
    }
    sort(registers.begin(), registers.end());

#ifdef DEBUG
    printf("Instructions: %d\n", ins_cnt);
    printf("Regs:");
    for (int x : registers)
        printf(" %d", x);
    putchar('\n');
#endif

    printf("\t.org 0x0\n\t.global _start\n\t.set noat\n_start:\n");
    init();

    for (int i = 1; i <= ins_cnt; ++i) {
#ifdef DEBUG
        printf("%3d: ", i);
#endif
        cout << generate_single() << endl;
    }
}
