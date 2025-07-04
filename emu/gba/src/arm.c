#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "arm.h"
#include "utils.h"

// building structs
cpsr make_cpsr(u32 x) {
    cpsr ret;
    ret.n = x >> 31 & 0b1;
    ret.z = x >> 30 & 0b1;
    ret.c = x >> 29 & 0b1;
    ret.v = x >> 28 & 0b1;
    ret.dnm = x >> 8 & 0b11111111111111111111;
    ret.i = x >> 7 & 0b1;
    ret.f = x >> 6 & 0b1;
    ret.t = x >> 5 & 0b1;
    ret.mode = x & 0b11111;
    return ret;
}
u32 cpsr_to_u32(cpsr x) {
    return
        ((x.n & 0b1) << 31)
        | ((x.z & 0b1) << 30)
        | ((x.c & 0b1) << 29)
        | ((x.v & 0b1) << 28)
        | ((x.dnm & 0b11111111111111111111) << 8)
        | ((x.i & 0b1) << 7)
        | ((x.f & 0b1) << 6)
        | ((x.t & 0b1) << 5)
        | (x.mode & 0b11111)
        ;
}

// memory access
u8 *mem_loc(emu *e, u32 addr) {
    if (addr >= 0x02000000 && addr <= 0x0203FFFF) {
        return &e->mem.ewram[addr - 0x02000000];
    } else if (addr >= 0x03000000 && addr <= 0x03007FFF) {
        return &e->mem.iwram[addr - 0x03000000];
    } else if (addr >= 0x08000000 && addr <= 0x09FFFFFF) {
        return &e->mem.rom[addr - 0x08000000];
    }
    return NULL;
}
static inline u32 mem_read8(emu *e, u32 addr) {
    u8 *loc = mem_loc(e, addr);
    if (loc) return *loc;
    else {
        panic("invalid read address: 0x%08xu", addr);
        return 0xee;
    }
}
static inline u32 mem_read16(emu *e, u32 addr) {
    return mem_read8(e, addr)
        | mem_read8(e, addr + 1) << 8;
}
static inline u32 mem_read32(emu *e, u32 addr) {
    return mem_read8(e, addr)
        | mem_read8(e, addr + 1) << 8
        | mem_read8(e, addr + 2) << 16
        | mem_read8(e, addr + 3) << 24;
}
static inline void mem_write8(emu *e, u32 addr, u8 val) {
    u8 *loc = mem_loc(e, addr);
    if (loc) *loc = val;
    else panic("invalid write address: 0x%08xu", addr);
}
static inline void mem_write16(emu *e, u32 addr, u16 val) {
    mem_write8(e, addr, val & 0xff);
    mem_write8(e, addr + 1, val >> 8 & 0xff);
}
static inline void mem_write32(emu *e, u32 addr, u32 val) {
    mem_write8(e, addr, val & 0xff);
    mem_write8(e, addr + 1, val >> 8 & 0xff);
    mem_write8(e, addr + 2, val >> 16 & 0xff);
    mem_write8(e, addr + 3, val >> 24 & 0xff);
}

// utilities for implementing instructions
static inline u32 *reg_loc(emu *e, reg r) {
    switch (e->cpsr.mode) {
    case MODE_FIQ: return r >= R8 && r <= R14 ? &e->reg_fiq[r - R8] : &e->reg[r];
    case MODE_IRQ: return r >= R13 && r <= R14 ? &e->reg_irq[r - R13] : &e->reg[r];
    case MODE_SVC: return r >= R13 && r <= R14 ? &e->reg_svc[r - R13] : &e->reg[r];
    case MODE_ABT: return r >= R13 && r <= R14 ? &e->reg_abt[r - R13] : &e->reg[r];
    case MODE_UND: return r >= R13 && r <= R14 ? &e->reg_und[r - R13] : &e->reg[r];
    default: return &e->reg[r];
    }
}
static inline u32 r(emu *e, reg r) { return *reg_loc(e, r); }
static inline void sr(emu *e, reg r, u32 val) { *reg_loc(e, r) = val; }
static inline u32 ror(u32 v, u32 rot) {
    while (rot > 32) rot -= 32;
    return v >> rot | v << (32 - rot);
}
static inline bool flag(u32 ins, u32 idx) { return ins >> idx & 0b1; }
static inline u32 sext(u8 x) { return (u32) (s32) (s8) x; }
static bool cond_satisfied(emu *e, cond c) {
    switch (c) {
    case COND_EQ: return e->cpsr.z; case COND_NE: return !e->cpsr.z;
    case COND_CS: return e->cpsr.c; case COND_CC: return !e->cpsr.c;
    case COND_MI: return e->cpsr.n; case COND_PL: return !e->cpsr.n;
    case COND_VS: return e->cpsr.v; case COND_VC: return !e->cpsr.v;
    case COND_HI: return e->cpsr.c && !e->cpsr.z; case COND_LS: return !e->cpsr.c || e->cpsr.z;
    case COND_GE: return e->cpsr.n == e->cpsr.v; case COND_LT: return e->cpsr.n != e->cpsr.v;
    case COND_GT: return !e->cpsr.z && e->cpsr.n == e->cpsr.v;
    case COND_LE: return e->cpsr.z || e->cpsr.n != e->cpsr.v;
    case COND_AL: return true;
    default: return false;
    }
}
static bool shift_apply(emu *e, u32 ins, u32 sh, u32 *v, bool imm) {
    shift shift_type = ins >> 5 & 0b11;
    u32 w = *v;
    switch (shift_type) {
    case SHIFT_LSL:
        *v <<= sh;
        return
            sh == 0 ? e->cpsr.c
            : sh > 32 ? 0
            : w >> (32 - sh) & 0b1;
    case SHIFT_LSR:
        if (imm && sh == 0) sh = 32;
        *v >>= sh;
        return
            sh == 0 ? e->cpsr.c
            : sh > 32 ? 0
            : w >> (sh - 1) & 0b1;
    case SHIFT_ASR:
        if (imm && sh == 0) sh = 32;
        *v = ((s32) w) >> sh;
        return
            sh == 0 ? e->cpsr.c
            : sh >= 32 ? w >> 31 & 0b1
            : w >> (sh - 1) & 0b1;
    case SHIFT_ROR:
        if (imm && sh == 0) { // rrx
            *v = (w >> 1 | e->cpsr.c << 31);
            return w & 0b1;
        } else {
            *v = ror(w, sh);
            return
                sh == 0 ? e->cpsr.c
                : sh == 32 ? w >> 31 & 0b1
                : w >> (sh - 1) & 0b1;
        }
    }
    return false;
}
static inline bool is_negative(u32 x) { return x >> 31 & 0b1; }
static inline bool is_carry(u64 res) { return res > 0xffffffffll; }
static inline bool is_overflow_add(u32 op1, u32 op2, u32 res) {
    return is_negative(op1) == is_negative(op2) && is_negative(op1) != is_negative(res);
}
static inline bool is_overflow_sub(u32 op1, u32 op2, u32 res) {
    return is_negative(op1) != is_negative(op2) && is_negative(op1) != is_negative(res);
}

// instruction decoding helpers
static inline cond a_cond(u32 ins) { return ins >> 28 & 0xf; }
static inline dpop a_op(u32 ins) { return ins >> 21 & 0xf; }
static inline reg a_rn(u32 ins) { return ins >> 16 & 0xf; }
static inline reg a_rd(u32 ins) { return ins >> 12 & 0xf; }
static inline reg a_rs(u32 ins) { return ins >> 8 & 0xf; }
static inline reg a_rm(u32 ins) { return ins & 0xf; }
static inline u32 a_disc3(u32 ins) { return ins >> 25 & 0b111; }
static inline u32 a_disc4(u32 ins) { return ins >> 24 & 0b1111; }
static inline u32 a_disc5(u32 ins) { return ins >> 23 & 0b11111; }
static inline u32 a_disc6(u32 ins) { return ins >> 22 & 0b111111; }
static inline u32 a_disc8(u32 ins) { return ins >> 20 & 0xff; }
static inline u32 a_disclo(u32 ins) { return ins >> 20 & 0b11; }
static inline u32 a_immhi(u32 ins) { return ins >> 4 & 0xf; }

// emulation
static inline void a_dataprocessing(emu *e, u32 ins, u32 op2, u32 shiftc, bool regop) {
    bool s = flag(ins, 20);
    reg rd = a_rd(ins);
    reg rn = a_rn(ins);
    u32 op1 = r(e, rn);
    if (regop && rn == PC) op1 += 4; // PC reads as 4 higher if we're shifting a register
    u32 opcode = a_op(ins);
    u32 res = 0;
    bool n = e->cpsr.n, z = e->cpsr.z, c = e->cpsr.c, v = e->cpsr.v;
    switch (opcode) {
    case DPOP_AND: case DPOP_TST:
        res = op1 & op2;
        c = shiftc;
        break;
    case DPOP_EOR: case DPOP_TEQ:
        res = op1 ^ op2;
        c = shiftc;
        break;
    case DPOP_SUB: case DPOP_CMP:
        res = op1 - op2;
        c = !(op2 > op1);
        v = is_overflow_sub(op1, op2, res);
        break;
    case DPOP_RSB:
        res = op2 - op1;
        c = !(op1 > op2);
        v = is_overflow_sub(op2, op1, res);
        break;
    case DPOP_ADD: case DPOP_CMN:
        res = op1 + op2;
        c = is_carry((u64) op1 + (u64) op2);
        v = is_overflow_add(op1, op2, res);
        break;
    case DPOP_ADC:
        res = op1 + op2 + e->cpsr.c;
        c = is_carry((u64) op1 + (u64) op2 + (u64) e->cpsr.c);
        v = is_overflow_add(op1, op2, res);
        break;
    case DPOP_SBC:
        res = op1 - op2 + e->cpsr.c - 1;
        c = !(op2 + 1 > op1 + e->cpsr.c);
        v = is_overflow_sub(op1, op2, res);
        break;
    case DPOP_RSC:
        res = op2 - op1 + e->cpsr.c - 1;
        c = !(op1 + 1 > op2 + e->cpsr.c);
        v = is_overflow_sub(op2, op1, res);
        break;
    case DPOP_ORR:
        res = op1 | op2;
        c = shiftc;
        break;
    case DPOP_MOV:
        res = op2;
        c = shiftc;
        break;
    case DPOP_BIC:
        res = op1 & ~op2;
        c = shiftc;
        break;
    case DPOP_MVN:
        res = ~op2;
        c = shiftc;
        break;
    }
    n = is_negative(res);
    z = res == 0;
    bool cmpop = opcode >> 2 == 0b10; // if TST, TEQ, CMP, CMN
    if (!cmpop) {
        sr(e, rd, res);
        if (rd == PC) e->branched = true;
    }
    if (s) {
        if (rd == PC) {
            switch (e->cpsr.mode) {
            case MODE_FIQ: e->cpsr = e->spsr_fiq; break;
            case MODE_IRQ: e->cpsr = e->spsr_irq; break;
            case MODE_SVC: e->cpsr = e->spsr_svc; break;
            case MODE_ABT: e->cpsr = e->spsr_abt; break;
            case MODE_UND: e->cpsr = e->spsr_und; break;
            default:
                e->cpsr.n = n; e->cpsr.z = z; e->cpsr.c = c; e->cpsr.v = v;
                break;
            }
        } else {
            e->cpsr.n = n; e->cpsr.z = z; e->cpsr.c = c; e->cpsr.v = v;
        }
    }
}
static inline void a_loadstore(emu *e, u32 ins, u32 off) {
    bool p = flag(ins, 24);
    bool u = flag(ins, 23);
    bool b = flag(ins, 22);
    bool w = flag(ins, 21);
    bool l = flag(ins, 20);
    reg rn = a_rn(ins);
    reg rd = a_rd(ins);
    u32 addrpre = r(e, rn);
    u32 addrpost = u ? addrpre + off : addrpre - off;
    u32 addr = p ? addrpost : addrpre;
    u32 misalign = addr & 0b11; // align word operations, and rotate misaligned loads
    u32 rot = (4 - misalign) << 3; // misalign represents the misalignment in bytes - convert to bits
    if (!b) addr &= 0b11111111111111111111111111111100;
    if (l) if (b) sr(e, rd, mem_read8(e, addr)); else sr(e, rd, ror(mem_read32(e, addr), rot));
    else if (b) mem_write8(e, addr, r(e, rd)); else mem_write32(e, addr, r(e, rd));
    if (!p || w) sr(e, rn, addrpost);
}
static inline void a_loadstorehalfword(emu *e, u32 ins, u32 off) {
    bool p = flag(ins, 24);
    bool u = flag(ins, 23);
    bool w = flag(ins, 21);
    bool l = flag(ins, 20);
    bool s = flag(ins, 6);
    bool h = flag(ins, 5);
    reg rn = a_rn(ins);
    reg rd = a_rd(ins);
    u32 addrpre = r(e, rn);
    u32 addrpost = u ? addrpre + off : addrpre - off;
    u32 addr = p ? addrpost : addrpre;
    if (l) {
        if (h) {
            u16 v = mem_read16(e, addr);
            sr(e, rd, s ? sext(v) : v);
        } else sr(e, rd, sext(mem_read8(e, addr)));
    } else if (h) mem_write16(e, addr, r(e, rd));
    else mem_write8(e, addr, r(e, rd));
    if (!p || w) sr(e, rn, addrpost);
}

bool emulate_arm_ins(emu *e, u32 ins) {
    // in ARM, the PC points to the instruction after the next instruction to execute
    // this is "Fucked Up" but we'll deal with it
    // see: https://stackoverflow.com/questions/24091566/why-does-the-arm-pc-register-point-to-the-instruction-after-the-next-one-to-be-e
    e->branched = false;
    cond cond = a_cond(ins);
    if (!cond_satisfied(e, cond)) goto end;
    if (a_disc8(ins) == 0b00010010 && a_immhi(ins) == 0b0001) {
        // branch and exchange
        reg rn = a_rm(ins);
        u32 addr = r(e, rn);
        bool thumb = addr & 0b1;
        if (thumb) {
            e->cpsr.t = 1;
            addr &= 0b11111111111111111111111111111110;
            sr(e, PC, addr + 2);
        } else {
            e->cpsr.t = 0;
            addr &= 0b11111111111111111111111111111100;
            sr(e, PC, addr + 4);
        }
    } else if (a_disc6(ins) == 0b000000 && a_immhi(ins) == 0b1001) {
        // multiply
        bool a = flag(ins, 21);
        bool s = flag(ins, 20);
        // note that rn and rd are swapped for multiply
        reg rd = a_rn(ins);
        u32 add = r(e, a_rd(ins));
        u32 f1 = r(e, a_rs(ins));
        u32 f2 = r(e, a_rm(ins));
        u32 res = f1 * f2 + a ? add : 0;
        sr(e, rd, res);
        if (s) {
            e->cpsr.z = res == 0;
            e->cpsr.n = is_negative(res);
        }
    } else if (a_disc5(ins) == 0b00001 && a_immhi(ins) == 0b1001) {
        panic("multiply long");
        return false;
    } else if (a_disc5(ins) == 0b00010 && a_disclo(ins) == 0b00 && a_immhi(ins) == 0b1001) {
        // swap memory and register
        bool b = flag(ins, 22);
        u32 addr = r(e, a_rn(ins));
        reg rd = a_rd(ins);
        u32 rval = r(e, a_rm(ins));
        if (b) {
            u32 mval = mem_read32(e, addr);
            mem_write32(e, addr, rval);
            sr(e, rd, mval);
        } else {
            u8 mval = mem_read8(e, addr);
            mem_write8(e, addr, rval);
            sr(e, rd, mval);
        }
    } else if (a_disc5(ins) == 0b00010 && a_disclo(ins) == 0b00) {
        panic("move from status register");
        return false;
    } else if (a_disc5(ins) == 0b00110 && a_disclo(ins) == 0b10) {
        panic("move immediate to status register");
        return false;
    } else if (a_disc5(ins) == 0b00010 && a_disclo(ins) == 0b10 && flag(ins, 4) == 0) {
        panic("move register to status register");
        return false;
    } else if (a_disc4(ins) == 0b1110 && flag(ins, 4) == 0) {
        panic("coprocessor data processing");
        return false;
    } else if (a_disc4(ins) == 0b1110 && flag(ins, 4) == 1) {
        panic("coprocessor register transfers");
        return false;
    } else if (a_disc4(ins) == 0b1111) {
        panic("software interrupt");
        return false;
    } else if (a_disc3(ins) == 0b000 && flag(ins, 22) == 1 && flag(ins, 7) == 1 && flag(ins, 4) == 1) {
        // load/store 16-bit, offset in immediate
        u32 hi = ins >> 8 & 0xf;
        u32 lo = ins & 0xf;
        u32 off = hi << 4 | lo;
        a_loadstorehalfword(e, ins, off);
    } else if (a_disc3(ins) == 0b000 && flag(ins, 22) == 0 && flag(ins, 7) == 1 && flag(ins, 4) == 1) {
        // load/store 16-bit, offset in register
        u32 off = r(e, a_rm(ins));
        a_loadstorehalfword(e, ins, off);
    } else if (a_disc3(ins) == 0b001) {
        // data processing, op2 is rotated immediate
        u32 imm = ins & 0xff;
        u32 rot = (ins >> 8 & 0xf) << 1;
        u32 v = ror(imm, rot);
        bool shiftc = rot == 0 ? e->cpsr.c : imm >> (rot - 1) & 0b1;
        a_dataprocessing(e, ins, v, shiftc, false);
    } else if (a_disc3(ins) == 0b000 && flag(ins, 4) == 0) {
        // data processing, op2 is register shifted by immediate
        reg reg = a_rm(ins);
        u32 v = r(e, reg);
        if (reg == PC) v += 4; // weird quirk with PC - it's 12 ahead instead of 8 in this case
        a_dataprocessing(e, ins, v, shift_apply(e, ins, ins >> 7 & 0b1111, &v, true), true);
    } else if (a_disc3(ins) == 0b000 && flag(ins, 4) == 1) {
        // data processing, op2 is register shifted by register
        u32 v = r(e, a_rm(ins));
        u32 sh = r(e, a_rs(ins));
        a_dataprocessing(e, ins, v, shift_apply(e, ins, sh, &v, false), false);
    } else if (a_disc3(ins) == 0b010) {
        // load/store, offset in immediate
        u32 imm = ins && 0xfff;
        a_loadstore(e, ins, imm);
    } else if (a_disc3(ins) == 0b011 && flag(ins, 4) == 0) {
        // load/store, offset in register shifted by immediate
        u32 v = r(e, a_rm(ins));
        (void) shift_apply(e, ins, ins >> 7 & 0b1111, &v, true);
        a_loadstore(e, ins, v);
    } else if (a_disc3(ins) == 0b100) {
        // load/store multiple registers
        bool p = flag(ins, 24);
        bool u = flag(ins, 23);
        bool w = flag(ins, 21);
        bool l = flag(ins, 20);
        u32 reglist = ins & 0xffff;
        reg rn = a_rn(ins);
        u32 addr = r(e, rn);
        for (ptrdiff_t i = 0; i < 16; ++i) {
            if (reglist >> i & 0b1) {
                if (p) { if (u) addr += 4; else addr -= 4; }
                if (l) sr(e, i, mem_read32(e, addr));
                else mem_write32(e, addr, r(e, i));
                if (!p) { if (u) addr += 4; else addr -= 4; }
            }
        }
        if (w) sr(e, rn, addr);
    } else if (a_disc3(ins) == 0b110) {
        panic("coprocessor load and store");
        return false;
    } else if (a_disc3(ins) == 0b101) {
        // branch (optionally with link)
        bool l = flag(ins, 24);
        if (l) sr(e, LR, r(e, PC));
        u32 uoff = ins & 0xffffff;
        s32 soff = ((s32) (uoff << 8)) >> 8; // sign-extend from 24-bits to 32-bits
        soff <<= 2;
        sr(e, PC, (s32) r(e, PC) + 4 + soff);
    } else if (a_disc3(ins) == 0b011 && flag(ins, 4) == 1) {
        // undefined instructions
        debug("undefined");
        return false;
    }
end:
    u32 branch_offset = e->branched ? 8 : 4;
    sr(e, PC, r(e, PC) + branch_offset);
    return true;
}
static inline bool emulate_arm(emu *e) {
    u32 pc = r(e, PC);
    u32 ins = mem_read32(e, pc - 4);
    return emulate_arm_ins(e, ins);
}

static inline u32 t_disc3(u32 ins) { return ins >> 13 & 0b111; }
static inline u32 t_disc4(u32 ins) { return ins >> 12 & 0b1111; }
static inline u32 t_disc5(u32 ins) { return ins >> 11 & 0b11111; }
static inline u32 t_disc6(u32 ins) { return ins >> 10 & 0b111111; }
static inline u32 t_disc8(u32 ins) { return ins >> 8 & 0xff; }
static inline u32 t_rm(u32 ins) { return ins >> 6 & 0b111; }
static inline u32 t_rn(u32 ins) { return ins >> 3 & 0b111; }
static inline u32 t_rd(u32 ins) { return ins & 0b111; }

static inline void t_loadstore(emu *e, u32 ins, u32 off, bool b) {
    bool l = flag(ins, 11);
    reg rn = t_rn(ins);
    reg rd = t_rd(ins);
    u32 addr = r(e, rn);
    addr += off;
    u32 misalign = addr & 0b11;
    u32 rot = (4 - misalign) << 3;
    if (!b) addr &= 0b11111111111111111111111111111100;
    if (l) if (b) sr(e, rd, mem_read8(e, addr)); else sr(e, rd, ror(mem_read32(e, addr), rot));
    else if (b) mem_write8(e, addr, r(e, rd)); else mem_write32(e, addr, r(e, rd));
}

static inline void t_loadstorehalfword(emu *e, u32 ins, u32 off, bool h) {
    bool l = flag(ins, 11);
    reg rn = t_rn(ins);
    reg rd = t_rd(ins);
    u32 addr = r(e, rn);
    addr += off;
    if (l) {
        if (h) sr(e, rd, mem_read16(e, addr));
        else sr(e, rd, sext(mem_read8(e, addr)));
    } else mem_write16(e, addr, r(e, rd));
}

bool emulate_thumb_ins(emu *e, u16 ins) {
    // similar situation in Thumb - PC is 4 bytes ahead during instruction
    if (a_disc8(ins) == 0b11011111) {
        // software interrupt
        panic("software interrupt");
        return false;
    } else if (a_disc6(ins) == 0b010000) {
        // data-processing register
        tdpop opcode = ins >> 6 & 0xf;
        reg rs = t_rn(ins);
        reg rd = t_rd(ins);
        u32 op1 = r(e, rd);
        u32 op2 = r(e, rs);
        u32 res = 0;
        bool n = e->cpsr.n, z = e->cpsr.z, c = e->cpsr.c, v = e->cpsr.v;
        switch (opcode) {
        case TDPOP_AND: case TDPOP_TST:
            res = op1 & op2;
            break;
        case TDPOP_EOR:
            res = op1 ^ op2;
            break;
        case TDPOP_CMP:
            res = op1 - op2;
            c = !(op2 > op1);
            v = is_overflow_sub(op1, op2, res);
            break;
        case TDPOP_CMN:
            res = op1 + op2;
            c = is_carry((u64) op1 + (u64) op2);
            v = is_overflow_add(op1, op2, res);
            break;
        case TDPOP_ADC:
            res = op1 + op2 + e->cpsr.c;
            c = is_carry((u64) op1 + (u64) op2 + (u64) e->cpsr.c);
            v = is_overflow_add(op1, op2, res);
            break;
        case TDPOP_SBC:
            res = op1 - op2 + e->cpsr.c - 1;
            c = !(op2 + 1 > op1 + e->cpsr.c);
            v = is_overflow_sub(op1, op2, res);
            break;
        case TDPOP_ORR:
            res = op1 | op2;
            break;
        case TDPOP_BIC:
            res = op1 & ~op2;
            break;
        case TDPOP_MVN:
            res = ~op2;
            break;
        case TDPOP_MUL:
            res = op1 * op2;
            break;
        case TDPOP_NEG:
            res = -op2;
            break;
        case TDPOP_ROR:
            op2 &= 0xff;
            if (op2) {
                res = ror(op1, op2);
                c = op2 < 32 ? op1 >> (op2 - 1) & 0b1 : op1 >> 31 & 0b1;
            }
            break;
        case TDPOP_ASR:
            op2 &= 0xff;
            if (op2) {
                res = (s32) op1 >> (s32) op2;
                c = op2 < 32 ? op1 >> (op2 - 1) & 0b1 : op1 >> 31 & 0b1;
            }
            break;
        case TDPOP_LSR:
            op2 &= 0xff;
            if (op2) {
                res = op1 >> op2;
                c = op2 < 32 ? op1 >> (op2 - 1) & 0b1 : op1 >> 31 & 0b1;
            }
            break;
        case TDPOP_LSL:
            op2 &= 0xff;
            if (op2) {
                res = op1 << op2;
                c = op2 < 32 ? op1 >> (32 - op2) & 0b1
                    : op2 == 32 ? op1 & 0b1
                    : 0;
            }
            break;
        }
        z = res == 0;
        n = is_negative(res);
        if (opcode != TDPOP_TST && opcode != TDPOP_CMP && opcode != TDPOP_CMN) sr(e, rd, res);
        e->cpsr.n = n; e->cpsr.z = z; e->cpsr.c = c; e->cpsr.v = v;

    } else if (a_disc6(ins) == 0b010001) {
        // hi register operations / branch exchange
        thop op = ins >> 8 & 0b11;
        reg rm = t_rn(ins); if (flag(ins, 6)) rm += R8;
        reg rd = t_rd(ins); if (flag(ins, 7)) rd += R8;
        switch (op) {
        case THOP_ADD: { 
            u32 op1 = r(e, rm);
            u32 op2 = r(e, rd);
            u32 res = op1 + op2;
            sr(e, rd, res);
            break;
        }
        case THOP_CMP: {
            u32 op1 = r(e, rm);
            u32 op2 = r(e, rd);
            u32 res = op2 - op1;
            e->cpsr.n = is_negative(res);
            e->cpsr.z = res == 0;
            e->cpsr.c = !(op1 > op2);
            e->cpsr.v = is_overflow_sub(op1, op2, res);
            break;
        }
        case THOP_MOV:
            sr(e, rd, r(e, rm));
            break;
        case THOP_BX:
            u32 addr = r(e, rm);
            bool thumb = addr & 0b1;
            if (thumb) {
                e->cpsr.t = 1;
                addr &= 0b11111111111111111111111111111110;
                sr(e, PC, addr + 2);
            } else {
                e->cpsr.t = 0;
                addr &= 0b11111111111111111111111111111100;
                sr(e, PC, addr + 4);
            }
            break;
        }
    } else if (a_disc5(ins) == 0b00011) {
        // add/subtract register or immediate
        bool isimm = flag(ins, 9);
        bool sub = flag(ins, 9);
        u32 op1 = isimm ? ins >> 6 & 0b111 : r(e, t_rm(ins));
        u32 op2 = r(e, t_rn(ins));
        u32 res = sub ? op2 - op1 : op2 + op1;
        e->cpsr.n = is_negative(res);
        e->cpsr.z = res == 0;
        e->cpsr.c = sub ? !(op1 > op2) : is_carry((u64) op2 + (u64) op1);
        e->cpsr.v = sub? is_overflow_sub(op1, op2, res) : is_overflow_add(op1, op2, res);
        sr(e, t_rd(ins), res);
    } else if (a_disc5(ins) == 0b11100) {
        // unconditional branch
        u32 uoff = ins & 0b11111111111;
        uoff <<= 1;
        uoff = (((s32) uoff) << 20) >> 20;
        sr(e, PC, r(e, PC) + 2 + uoff);
    } else if (a_disc5(ins) == 0b11101) {
        // undefined instruction
        debug("undefined instruction");
        return false;
    } else if (a_disc5(ins) == 0b11110) {
        // branch with link prefix
    } else if (a_disc5(ins) == 0b11111) {
        // branch with link suffix
    } else if (a_disc5(ins) == 0b01001) {
        // load/store to/from pc offset
        reg rd = ins >> 8 & 0b111;
        u32 off = ins & 0xff;
        off *= 4;
        u32 addr = r(e, PC) + 2 + off;
        sr(e, rd, mem_read32(e, addr));
    } else if (a_disc4(ins) == 0b0101 && flag(ins, 9) == 0) {
        // load/store word or byte register
        u32 off = r(e, t_rm(ins));
        t_loadstore(e, ins, off, flag(ins, 10));
    } else if (a_disc4(ins) == 0b0101 && flag(ins, 9) == 1) {
        // load/store halfword register
        u32 off = r(e, t_rm(ins));
        t_loadstorehalfword(e, ins, off, flag(ins, 11));
    } else if (a_disc4(ins) == 0b1000) {
        // load/store halfword immediate
        u32 off = ins >> 6 & 0b11111;
        t_loadstorehalfword(e, ins, off, false);
    } else if (a_disc4(ins) == 0b1001) {
        // load/store to/from stack offset
        bool l = flag(ins, 11);
        reg rd = ins >> 8 & 0b111;
        u32 off = ins & 0xff;
        off *= 4;
        u32 addr = r(e, SP) + off;
        if (l) sr(e, rd, mem_read32(e, addr)); else mem_write32(e, addr, r(e, rd));
    } else if (a_disc4(ins) == 0b1010) {
        // add/subtract to/from SP or PC
        reg rd = ins >> 8 & 0b111;
        bool sp = flag(ins, 11);
        u32 off = ins & 0xff;
        u32 base = sp ? r(e, SP) : r(e, PC) & 0xfffffffc;
        sr(e, rd, base + off);
    } else if (a_disc4(ins) == 0b1011 && flag(ins, 10) == 0) {
        // adjust stack pointer
        bool sub = flag(ins, 7);
        u32 off = ins & 0b1111111;
        u32 base = r(e, SP);
        u32 res = sub ? base - off : base + off;
        sr(e, SP, res);
    } else if (a_disc4(ins) == 0b1011 && flag(ins, 10) == 1) {
        // push/pop register list
        bool l = flag(ins, 11);
        bool extra = flag(ins, 8);
        u32 addr = r(e, SP);
        u32 reglist = ins & 0xff;
        if (!l && extra) mem_write32(e, addr, r(e, LR));
        for (ptrdiff_t j = 0; j < 8; ++j) {
            ptrdiff_t i = l ? i : 7 - i;
            bool set = reglist >> i & 0b1;
            if (set) {
                if (l) sr(e, i, mem_read32(e, addr));
                else mem_write32(e, addr, r(e, i));
                addr = l ? addr + 4 : addr - 4;
            }
        }
        if (l && extra) sr(e, PC, mem_read32(e, addr));
    } else if (a_disc4(ins) == 0b1100) {
        // load/store multiple
        bool l = flag(ins, 11);
        reg rn = ins >> 8 & 0b111;
        u32 addr = r(e, rn);
        u32 reglist = ins & 0xff;
        for (ptrdiff_t i = 0; i < 8; ++i) {
            bool set = reglist >> i & 0b1;
            if (set) {
                if (l) sr(e, i, mem_read32(e, addr));
                else mem_write32(e, addr, r(e, i));
                addr += 4;
            }
        }
        sr(e, rn, addr);
    } else if (a_disc4(ins) == 0b1101) {
        // conditional branch
        cond c = ins >> 8 & 0xf;
        if (cond_satisfied(e, c)) {
            u32 uoff = ins & 0xff;
            uoff <<= 1;
            uoff = (((s32) uoff) << 23) >> 23;
            sr(e, PC, r(e, PC) + 2 + uoff);
        }
    } else if (a_disc3(ins) == 0b000) {
        // shift by immediate
    } else if (a_disc3(ins) == 0b001) {
        // add/subtract/compare immediate
        bool sub = flag(ins, 11);
        bool write = flag(ins, 12);
        u32 op1 = ins & 0xff;
        u32 op2 = ins >> 8 & 0b111;
        u32 res = sub ? op2 - op1 : op2 + op1;
        e->cpsr.n = is_negative(res);
        e->cpsr.z = res == 0;
        e->cpsr.c = sub ? !(op1 > op2) : is_carry((u64) op2 + (u64) op1);
        e->cpsr.v = sub? is_overflow_sub(op1, op2, res) : is_overflow_add(op1, op2, res);
        if (write) sr(e, t_rd(ins), res);
    } else if (a_disc3(ins) == 0b011) {
        // load/store word or byte immediate
        u32 off = ins >> 6 & 0b11111;
        t_loadstore(e, ins, off, flag(ins, 12));
    }
    sr(e, PC, r(e, PC) + 2);
    return true;
}
static inline bool emulate_thumb(emu *e) {
    u32 pc = r(e, PC);
    u16 ins = mem_read32(e, pc - 2);
    return emulate_thumb_ins(e, ins);
}

void emulate(emu *e, u32 fuel) {
    bool running = true;
    while (running && fuel > 0) {
        fuel -= 1;
        if (e->cpsr.t) emulate_thumb(e);
        else emulate_arm(e);
    }
}
