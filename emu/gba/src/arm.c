#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "arm.h"
#include "utils.h"

u8 *mem_loc(emu *e, u32 addr) {
    if (addr >= 0x02000000 && addr <= 0x0203FFFF) {
        return &e->mem.ewram[addr - 0x02000000];
    } else if (addr >= 0x03000000 && addr <= 0x03007FFF) {
        return &e->mem.iwram[addr - 0x03000000];
    } else if (addr >= 0x08000000 && addr <= 0x09FFFFFF) {
        return &e->mem.rom[addr - 0x08000000];
    }
    panic("invalid address: %u", addr);
    return NULL;
}

u32 mem_read8(emu *e, u32 addr) {
    return *mem_loc(e, addr);
}
u32 mem_read16(emu *e, u32 addr) {
    return mem_read8(e, addr)
        | mem_read8(e, addr + 1) << 8;
}
u32 mem_read32(emu *e, u32 addr) {
    return mem_read8(e, addr)
        | mem_read8(e, addr + 1) << 8
        | mem_read8(e, addr + 2) << 16
        | mem_read8(e, addr + 3) << 24;
}

void mem_write8(emu *e, u32 addr, u8 val) {
    *mem_loc(e, addr) = val;
}
void mem_write13(emu *e, u32 addr, u16 val) {
    *mem_loc(e, addr) = val & 0xff;
    *mem_loc(e, addr + 1) = val >> 8 & 0xff;
}
void mem_write32(emu *e, u32 addr, u32 val) {
    *mem_loc(e, addr) = val & 0xff;
    *mem_loc(e, addr + 1) = val >> 8 & 0xff;
    *mem_loc(e, addr + 2) = val >> 16 & 0xff;
    *mem_loc(e, addr + 3) = val >> 24 & 0xff;
}

u32 r(emu *e, reg r) { return e->reg[r]; }
void sr(emu *e, reg r, u32 val) { e->reg[r] = val; }

cond d_cond(u32 ins) { return ins >> 28 & 0xf; }
u32 d_op(u32 ins) { return ins >> 21 & 0xf; }
reg d_rn(u32 ins) { return ins >> 16 & 0xf; }
reg d_rd(u32 ins) { return ins >> 12 & 0xf; }
reg d_rs(u32 ins) { return ins >> 8 & 0xf; }
u32 d_disc3(u32 ins) { return ins >> 25 & 0b111; }
u32 d_disc4(u32 ins) { return ins >> 24 & 0b1111; }
u32 d_disc5(u32 ins) { return ins >> 23 & 0b11111; }
u32 d_disc6(u32 ins) { return ins >> 22 & 0b111111; }
u32 d_disc8(u32 ins) { return ins >> 20 & 0xff; }
u32 d_disclo(u32 ins) { return ins >> 20 & 0b11; }
u32 d_immhi(u32 ins) { return ins >> 4 & 0xf; }
bool d_flag(u32 ins, u32 idx) { return ins >> idx & 0b1; }

void emulate(emu *e) {
    bool running = true;
    while (running) {
        u32 pc = r(e, PC);
        u32 ins = mem_read32(e, pc);
        sr(e, PC, pc + 4);
        if (d_disc8(ins) == 0b00010010 && d_immhi(ins) == 0b0001) {
            debug("branch/exchange");
        } else if (d_disc6(ins) == 0b000000 && d_immhi(ins) == 0b1001) {
            debug("multiply");
        } else if (d_disc5(ins) == 0b00001 && d_immhi(ins) == 0b1001) {
            debug("multiply long");
        } else if (d_disc5(ins) == 0b00010 && d_disclo(ins) == 0b00 && d_immhi(ins) == 0b1001) {
            debug("swap/swap byte");
        } else if (d_disc5(ins) == 0b00010 && d_disclo(ins) == 0b00) {
            debug("move from status register");
        } else if (d_disc5(ins) == 0b00110 && d_disclo(ins) == 0b10) {
            debug("move immediate to status register");
        } else if (d_disc5(ins) == 0b00010 && d_disclo(ins) == 0b10 && d_flag(ins, 4) == 0) {
            debug("move register to status register");
        } else if (d_disc4(ins) == 0b1110 && d_flag(ins, 4) == 0) {
            debug("coprocessor data processing");
        } else if (d_disc4(ins) == 0b1110 && d_flag(ins, 4) == 1) {
            debug("coprocessor register transfers");
        } else if (d_disc4(ins) == 0b1111) {
            debug("software interrupt");
        } else if (d_disc3(ins) == 0b000 && d_flag(ins, 22) == 1 && d_flag(ins, 7) == 1 && d_flag(ins, 4) == 1) {
            debug("load/store halfword/signed byte immediate");
        } else if (d_disc3(ins) == 0b000 && d_flag(ins, 22) == 0 && d_flag(ins, 7) == 1 && d_flag(ins, 4) == 1) {
            debug("load/store halfword/signed byte");
        } else if (d_disc3(ins) == 0b001) {
            debug("data processing immediate");
        } else if (d_disc3(ins) == 0b000 && d_flag(ins, 4) == 0) {
            debug("data processing immediate shift");
        } else if (d_disc3(ins) == 0b000 && d_flag(ins, 4) == 1) {
            debug("data processing register shift");
        } else if (d_disc3(ins) == 0b010) {
            debug("load/store immediate offset");
        } else if (d_disc3(ins) == 0b011 && d_flag(ins, 4) == 0) {
            debug("load/store register offset");
        } else if (d_disc3(ins) == 0b100) {
            debug("load/store multiple");
        } else if (d_disc3(ins) == 0b110) {
            debug("coprocessor load and store");
        } else if (d_disc3(ins) == 0b101) {
            debug("branch");
        } else if (d_disc3(ins) == 0b011 && d_flag(ins, 4) == 1) {
            debug("undefined");
            running = false;
        }
    }
}
