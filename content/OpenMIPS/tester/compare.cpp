#include <bits/stdc++.h>
using namespace std;

FILE *fp_mars, *fp_core;
vector<int> registers;
char buf[1024];

class Regfile {
public:
    Regfile(const char *str) {
        stringstream ss(str);
        ss.unsetf(std::ios::dec);
        ss.unsetf(std::ios::hex);
        ss.unsetf(std::ios::oct);
        unsigned r; int i = 0;
        ss >> r;                // ignore PC
        while (ss >> r) {
            regs[registers[i++]] = r;
        }
    }
    void print() {
        for (int x : registers) {
            printf("  %2d: 0x%08x\n", x, regs[x]);
        }
    }

    uint32_t regs[34];
};

bool equal(const Regfile &r1, const Regfile &r2) {
    for (int x : registers) {
        if (r1.regs[x] != r2.regs[x])
            return false;
    }
    return true;
}

bool ok() {
    int n = strlen(buf);
    if (n < 8) return false;
    if (strncmp(buf, "WARNING:", strlen("WARNING:")) == 0)
        return false;
    for (int i = 0; i < n; ++i)
        if (isupper(buf[i]) || (!isalnum(buf[i]) && !isspace(buf[i])))
            return false;
    return true;
}

vector<Regfile> mars, core;

void read(const char *fname, vector<Regfile> &v) {
    FILE *fp = fopen(fname, "r");
    bool start = false;
    while (fgets(buf, sizeof(buf), fp)) {
        if (!ok()) continue;
        Regfile r(buf);
        if (!start && r.regs[1] == 0x12345678u)
            start = true;
        if (!start) continue;
        v.push_back(r);
    }
    fclose(fp);

#ifdef DEBUG
    printf("%s:\n",fname);
    int cnt = 0;
    for (const auto &r : v) {
        printf("%3d:", ++cnt);
        for (int x : registers)
            printf(" %08x", r.regs[x]);
        putchar('\n');
    }
#endif
}

int main(int argc, char *argv[]) {
    if (argc <= 3) {
        fprintf(stderr, "Usage: %s MARS_OUT CORE_OUT REG...\n", argv[0]);
        return -1;
    }

    for (int i = 3; i < argc; ++i) {
        if (isdigit(argv[i][0])) {
            registers.push_back(atoi(argv[i]));
        } else if (argv[i][0] == 'h') {
            registers.push_back(32);
        } else if (argv[i][0] == 'l') {
            registers.push_back(33);
        }
    }
    sort(registers.begin(), registers.end());

    read(argv[1], mars);        // mars
    read(argv[2], core);  // core

    if (mars.size() > core.size()) {
        printf("ERROR: mars %lu lines, while core %lu lines\n",
               mars.size(), core.size());
        return 1;
    }
    printf("mars %lu lines, core %lu lines\n",
           mars.size(), core.size());

    for (size_t i = 0; i < mars.size(); ++i) {
        if (!equal(mars[i], core[i])) {
            printf("ERROR at line %lu:\n", i + 1);
            printf("Mars:\n");
            mars[i].print();
            printf("Core:\n");
            core[i].print();
            return 1;
        }
    }

    return 0;
}
