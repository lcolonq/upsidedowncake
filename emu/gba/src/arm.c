#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "arm.h"
#include "utils.h"
#include "mulcarry.h"

// conversions
cpsr make_cpsr(u32 x) {
    cpsr ret;
    ret.n = x >> 31 & 0b1; ret.z = x >> 30 & 0b1; ret.c = x >> 29 & 0b1; ret.v = x >> 28 & 0b1;
    ret.dnm = x >> 8 & 0b11111111111111111111;
    ret.i = x >> 7 & 0b1; ret.f = x >> 6 & 0b1; ret.t = x >> 5 & 0b1;
    ret.mode = x & 0b11111;
    return ret;
}
u32 cpsr_to_u32(cpsr x) {
    return ((x.n & 0b1) << 31) | ((x.z & 0b1) << 30) | ((x.c & 0b1) << 29) | ((x.v & 0b1) << 28)
        | ((x.dnm & 0xfffff) << 8)
        | ((x.i & 0b1) << 7) | ((x.f & 0b1) << 6) | ((x.t & 0b1) << 5)
        | (x.mode & 0b11111);
}

// utilities for implementing instructions
static inline bool is_negative(u32 x) { return x >> 31 & 0b1; }
static inline bool is_carry(u64 res) { return res > 0xffffffffll; }
static inline bool is_overflow_add(u32 op1, u32 op2, u32 res) {
    return is_negative(op1) == is_negative(op2) && is_negative(op1) != is_negative(res);
}
static inline bool is_overflow_sub(u32 op1, u32 op2, u32 res) {
    return is_negative(op1) != is_negative(op2) && is_negative(op1) != is_negative(res);
}
static inline u32 *reg_loc(emu *e, reg r) {
    if (r < 0 || r > PC) {
        panic("invalid register: %d", r);
        return &e->reg[0];
    }
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
static inline u32 sext8(u8 x) {
    debug("sext8: 0x%08x", x);
    return (u32) (s32) (s8) x;
}
static inline u32 sext16(u16 x) { return (u32) (s32) (s16) x; }
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

// memory access
static inline void mem_record_transaction(emu *e, transaction t) {
    if (!e->trans.record) return;
    if (e->trans.idx >= TRANS_MAX) {
        panic("too many transactions");
    } else {
        // debug("transaction %d: addr = 0x%08x, val = 0x%08x", e->trans.idx, t.addr, t.data);
        e->trans.data[e->trans.idx] = t;
        e->trans.idx++;
    }
}
static inline u32 mem_lookup_load(emu *e, u32 size, u32 addr) {
    for (ptrdiff_t i = 0; i < TRANS_MAX; ++i) {
        // debug("valid = %d, size = %d, addr = 0x%08x", e->trans.loads[i].valid, e->trans.loads[i].size, e->trans.loads[i].addr);
        if (e->trans.loads[i].valid
            && e->trans.loads[i].size == size
            && e->trans.loads[i].addr == addr) {
            // debug("found; data = 0x%08x", e->trans.loads[i].data);
            return e->trans.loads[i].data;
        }
    }
    debug("read from invalid load address: 0x%08x", addr);
    return 0xbeefbabe;
}
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
static inline u32 mem_read_byte(emu *e, u32 addr) {
    u8 *loc = mem_loc(e, addr);
    if (loc) return *loc;
    else {
        // panic("invalid read address: 0x%08xu", addr);
        return 0xee;
    }
}
static inline u32 mem_read8(emu *e, u32 addr) {
    if (e->trans.record) return mem_lookup_load(e, 1, addr);
    u32 res = mem_read_byte(e, addr);
    mem_record_transaction(e, (transaction){
        .kind = TRANS_READ,
        .size = 1,
        .addr = addr,
        .data = res,
    });
    return res;
}
static inline u32 mem_read16(emu *e, u32 addr) {
    u32 misalign = addr & 0b1; // align word operations, and rotate misaligned loads
    u32 rot = misalign << 3; // misalign represents the misalignment in bytes - convert to bits
    if (e->trans.record) {
        u32 x = mem_lookup_load(e, 2, addr);
        debug("x: 0x%08x, rot: %d, res: 0x%08x", x, rot, ror(x, rot));
        return ror(x, rot);
    }
    addr &= 0b11111111111111111111111111111110;
    u32 res = mem_read_byte(e, addr)
        | mem_read_byte(e, addr + 1) << 8;
    mem_record_transaction(e, (transaction){
        .kind = TRANS_READ,
        .size = 2,
        .addr = addr,
        .data = res,
    });
    return ror(res, rot);
}
static inline u32 mem_read32_raw(emu *e, u32 addr) {
    if (e->trans.record) return mem_lookup_load(e, 4, addr);
    addr &= 0b11111111111111111111111111111100;
    u32 res = mem_read_byte(e, addr)
        | mem_read_byte(e, addr + 1) << 8
        | mem_read_byte(e, addr + 2) << 16
        | mem_read_byte(e, addr + 3) << 24;
    mem_record_transaction(e, (transaction){
        .kind = TRANS_READ,
        .size = 4,
        .addr = addr,
        .data = res,
    });
    return res;
}
static inline u32 mem_read32(emu *e, u32 addr) {
    u32 misalign = addr & 0b11; // align word operations, and rotate misaligned loads
    u32 rot = misalign << 3; // misalign represents the misalignment in bytes - convert to bits
    return ror(mem_read32_raw(e, addr), rot);
}
static inline void mem_write_byte(emu *e, u32 addr, u8 val) {
    u8 *loc = mem_loc(e, addr);
    if (loc) *loc = val;
    // else panic("invalid write address: 0x%08xu", addr);
}
static inline void mem_write8(emu *e, u32 addr, u8 val) {
    mem_record_transaction(e, (transaction){
        .kind = TRANS_WRITE,
        .size = 1,
        .addr = addr,
        .data = val,
    });
    mem_write_byte(e, addr, val);
}
static inline void mem_write16(emu *e, u32 addr, u16 val) {
    mem_record_transaction(e, (transaction){
        .kind = TRANS_WRITE,
        .size = 2,
        .addr = addr,
        .data = val,
    });
    mem_write_byte(e, addr, val & 0xff);
    mem_write_byte(e, addr + 1, val >> 8 & 0xff);
}
static inline void mem_write32(emu *e, u32 addr, u32 val) {
    mem_record_transaction(e, (transaction){
        .kind = TRANS_WRITE,
        .size = 4,
        .addr = addr,
        .data = val,
    });
    mem_write_byte(e, addr, val & 0xff);
    mem_write_byte(e, addr + 1, val >> 8 & 0xff);
    mem_write_byte(e, addr + 2, val >> 16 & 0xff);
    mem_write_byte(e, addr + 3, val >> 24 & 0xff);
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
static inline u32 a_discmrs(u32 ins) { return ins & 0xfff; }
static inline u32 a_immhi(u32 ins) { return ins >> 4 & 0xf; }

// emulation
static bool shifter(emu *e, shift shift_type, u32 sh, u32 *v, bool imm) {
    u32 w = *v;
    switch (shift_type) {
    case SHIFT_LSL:
        if (sh == 0) {
            return e->cpsr.c;
        } else if (sh < 32) {
            *v <<= sh;
            return w >> (32 - sh) & 0b1;
        } else if (sh == 32) {
            *v = 0;
            return w & 0b1;
        } else {
            *v = 0;
            return 0;
        }
    case SHIFT_LSR:
        if ((imm && sh == 0) || sh == 32) {
            *v = 0;
            return is_negative(w);
        } else if (sh == 0) {
            return e->cpsr.c;
        } else if (sh > 32) {
            *v = 0;
            return 0;
        } else {
            *v >>= sh;
            return w >> (sh - 1) & 0b1;
        }
    case SHIFT_ASR:
        if (imm && sh == 0) {
            *v = is_negative(*v) ? 0xffffffff : 0x00000000;
            return is_negative(w);
        } else if (sh == 0) {
            return e->cpsr.c;
        } else if (sh < 32) {
            *v = ((s32) w) >> sh;
            return w >> (sh - 1) & 0b1;
        } else {
            *v = is_negative(*v) ? 0xffffffff : 0x00000000;
            return is_negative(w);
        }
    case SHIFT_ROR:
        if (imm && sh == 0) { // rrx
            *v = (w >> 1 | e->cpsr.c << 31);
            return w & 0b1;
        } else if (sh == 0) {
            return e->cpsr.c;
        } else {
            sh &= 0b11111;
            if (sh == 0) {
                return is_negative(w);
            } else {
                *v = ror(w, sh);
                return w >> (sh - 1) & 0b1;
            }
        }
    }
    return false;
}

static inline void dataprocessing(emu *e, bool s, reg rd, reg rn, u32 opcode, u32 op2, u32 shiftc, bool regop) {
    u32 op1 = r(e, rn);
    if (regop && rn == PC) op1 += 4; // PC reads as 4 higher if we're shifting a register
    u32 res = 0;
    bool n = e->cpsr.n, z = e->cpsr.z, c = e->cpsr.c, v = e->cpsr.v;
    switch (opcode) {
    case DPOP_AND: case DPOP_TST: res = op1 & op2; c = shiftc; break;
    case DPOP_EOR: case DPOP_TEQ: res = op1 ^ op2; c = shiftc; break;
    case DPOP_SUB: case DPOP_CMP:
        res = op1 - op2;
        c = !is_carry((u64) op1 - (u64) op2);
        v = is_overflow_sub(op1, op2, res);
        break;
    case DPOP_RSB:
        res = op2 - op1;
        c = !is_carry((u64) op2 - (u64) op1);
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
        c = !is_carry((u64) op1 - (u64) op2 + (u64) e->cpsr.c - (u64) 1);
        v = is_overflow_sub(op1, op2, res);
        break;
    case DPOP_RSC:
        res = op2 - op1 + e->cpsr.c - 1;
        c = !is_carry((u64) op2 - (u64) op1 + (u64) e->cpsr.c - (u64) 1);
        v = is_overflow_sub(op2, op1, res);
        break;
    case DPOP_ORR: res = op1 | op2; c = shiftc; break;
    case DPOP_MOV: res = op2; c = shiftc; break;
    case DPOP_BIC: res = op1 & ~op2; c = shiftc; break;
    case DPOP_MVN: res = ~op2; c = shiftc; break;
    }
    n = is_negative(res); z = res == 0;
    bool cmpop = opcode >> 2 == 0b10; // if TST, TEQ, CMP, CMN
    if (!cmpop) { sr(e, rd, res); if (rd == PC) e->branched = true; }
    if (s) {
        if (rd == PC) {
            switch (e->cpsr.mode) {
            case MODE_FIQ: e->cpsr = e->spsr_fiq; break;
            case MODE_IRQ: e->cpsr = e->spsr_irq; break;
            case MODE_SVC: e->cpsr = e->spsr_svc; break;
            case MODE_ABT: e->cpsr = e->spsr_abt; break;
            case MODE_UND: e->cpsr = e->spsr_und; break;
            default: e->cpsr.n = n; e->cpsr.z = z; e->cpsr.c = c; e->cpsr.v = v; break;
            }
        } else {
            e->cpsr.n = n; e->cpsr.z = z; e->cpsr.c = c; e->cpsr.v = v;
        }
    }
}
static inline void memop(emu *e, bool p, bool u, bool b, bool w, bool l, reg rn, reg rd, u32 off) {
    u32 val = r(e, rd); if (rd == PC) val += 4;
    u32 addrpre = r(e, rn);
    u32 addrpost = u ? addrpre + off : addrpre - off;
    u32 addr = p ? addrpost : addrpre;
    if (!p || w) {
        u32 pcoff = rn == PC ? 4 : 0;
        sr(e, rn, addrpost + pcoff);
        if (rn == PC) e->branched = true;
    }
    if (l) {
        if (b) sr(e, rd, mem_read8(e, addr));
        else sr(e, rd, mem_read32(e, addr));
        if (rd == PC) e->branched = true;
    } else {
        if (b) mem_write8(e, addr, val);
        else mem_write32(e, addr, val);
    }
}
static inline void memophalf(emu *e, bool p, bool u, bool w, bool l, bool s, bool h, reg rn, reg rd, u32 off) {
    u32 val = r(e, rd); if (rd == PC) val += 4;
    u32 addrpre = r(e, rn);
    u32 addrpost = u ? addrpre + off : addrpre - off;
    u32 addr = p ? addrpost : addrpre;
    if (!p || w) {
        u32 pcoff = rn == PC ? 4 : 0;
        sr(e, rn, addrpost + pcoff);
        if (rn == PC) e->branched = true;
    }
    if (l) {
        if (h) {
            u32 v = mem_read16(e, addr);
            if (s) { if (addr & 0b1) v = sext8(v); else v = sext16(v); }
            sr(e, rd, v);
        } else sr(e, rd, sext8(mem_read8(e, addr)));
        if (rd == PC) e->branched = true;
    } else if (h) mem_write16(e, addr, val);
    else mem_write8(e, addr, val);
}

bool emulate_arm_ins(emu *e, u32 ins) {
    // in ARM, the PC points to the instruction after the next instruction to execute
    // this is "Fucked Up" but we'll deal with it
    // see: https://stackoverflow.com/questions/24091566
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
            sr(e, PC, addr); e->branched = true;
        } else {
            e->cpsr.t = 0;
            addr &= 0b11111111111111111111111111111110;
            sr(e, PC, addr); e->branched = true;
        }
    } else if (a_disc6(ins) == 0b000000 && a_immhi(ins) == 0b1001) {
        // multiply
        bool a = flag(ins, 21);
        bool s = flag(ins, 20);
        // note that rn and rd are swapped for multiply
        reg rd = a_rn(ins);
        reg r0 = a_rd(ins); u32 add = r(e, r0); if (r0 == PC) add += 4;
        add = a ? add : 0;
        reg r1 = a_rs(ins); u32 f1 = r(e, r1); if (r1 == PC) f1 += 4;
        reg r2 = a_rm(ins); u32 f2 = r(e, r2); if (r2 == PC) f2 += 4;
        u32 res = f1 * f2 + add;
        sr(e, rd, res);
        if (rd == PC) e->branched = true;
        if (s) {
            e->cpsr.z = res == 0;
            e->cpsr.n = is_negative(res);
            e->cpsr.c = tick_multiply(f1, true) ? multiply_carry_simple(f1) : multiply_carry_lo(f2, f1, add);
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
    } else if (a_disc5(ins) == 0b00010 && a_disclo(ins) == 0b00 && a_discmrs(ins) == 0) {
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
        memophalf(e,
            flag(ins, 24), flag(ins, 23), flag(ins, 21), flag(ins, 20), flag(ins, 6), flag(ins, 5),
            a_rn(ins), a_rd(ins),
            off
        );
    } else if (a_disc3(ins) == 0b000 && flag(ins, 22) == 0 && flag(ins, 7) == 1 && flag(ins, 4) == 1) {
        // load/store 16-bit, offset in register
        u32 off = r(e, a_rm(ins));
        memophalf(e,
            flag(ins, 24), flag(ins, 23), flag(ins, 21), flag(ins, 20), flag(ins, 6), flag(ins, 5),
            a_rn(ins), a_rd(ins),
            off
        );
    } else if (a_disc3(ins) == 0b001) {
        // data processing, op2 is rotated immediate
        u32 imm = ins & 0xff;
        u32 rot = (ins >> 8 & 0xf) << 1;
        u32 v = ror(imm, rot);
        dataprocessing(e,
            flag(ins, 20), a_rd(ins), a_rn(ins), a_op(ins), v,
            rot == 0 ? e->cpsr.c : imm >> (rot - 1) & 0b1,
            false
        );
    } else if (a_disc3(ins) == 0b000 && flag(ins, 4) == 0) {
        // data processing, op2 is register shifted by immediate
        reg reg = a_rm(ins);
        u32 v = r(e, reg);
        dataprocessing(e,
            flag(ins, 20), a_rd(ins), a_rn(ins), a_op(ins), v,
            shifter(e, ins >> 5 & 0b11, ins >> 7 & 0b11111, &v, true),
            false
        );
    } else if (a_disc3(ins) == 0b000 && flag(ins, 4) == 1) {
        // data processing, op2 is register shifted by register
        reg rm = a_rm(ins);
        u32 v = r(e, rm);
        if (rm == PC) v += 4; // PC reads as 4 higher if we're shifting a register
        u32 sh = r(e, a_rs(ins)) & 0xff;
        dataprocessing(e,
            flag(ins, 20), a_rd(ins), a_rn(ins), a_op(ins), v,
            shifter(e, ins >> 5 & 0b11, sh, &v, false),
            true
        );
    } else if (a_disc3(ins) == 0b010) {
        // load/store, offset in immediate
        u32 imm = ins & 0xfff;
        memop(e,
            flag(ins, 24), flag(ins, 23), flag(ins, 22), flag(ins, 21), flag(ins, 20),
            a_rn(ins), a_rd(ins),
            imm
        );
    } else if (a_disc3(ins) == 0b011 && flag(ins, 4) == 0) {
        // load/store, offset in register shifted by immediate
        u32 v = r(e, a_rm(ins));
        (void) shifter(e, ins >> 5 & 0b11, ins >> 7 & 0b11111, &v, true);
        memop(e,
            flag(ins, 24), flag(ins, 23), flag(ins, 22), flag(ins, 21), flag(ins, 20),
            a_rn(ins), a_rd(ins),
            v
        );
    } else if (a_disc3(ins) == 0b100 && (ins & 0xffff) == 0) {
        bool p = flag(ins, 24);
        bool u = flag(ins, 23);
        bool s = flag(ins, 22);
        bool w = flag(ins, 21);
        bool l = flag(ins, 20);
        reg rn = a_rn(ins);
        u32 addr = r(e, rn);
        u32 addrstart = addr;
        bool loading_pc = l;
        ptrdiff_t regcount = 16;
        u32 addrfinal = u ? addr + regcount * 4 : addr - regcount * 4;
        if (!u) addrstart -= regcount * 4;
        if (p == u) addrstart += 4;
        mode m = e->cpsr.mode;
        if (s && !loading_pc) e->cpsr.mode = MODE_USR;
        if (l && w) {
            sr(e, rn, addrfinal);
            if (rn == PC) e->branched = true;
        }
        if (l) sr(e, PC, mem_read32_raw(e, addrstart));
        else {
            u32 val = r(e, PC);
            if (!(rn == PC && w)) val += 4;
            mem_write32(e, addrstart, val);
        }
        if (!l && w) {
            sr(e, rn, addrfinal);
            if (rn == PC) e->branched = true;
        }
        e->cpsr.mode = m;
        if (loading_pc) {
            e->branched = true;
            if (s) {
                switch (e->cpsr.mode) {
                case MODE_FIQ: e->cpsr = e->spsr_fiq; break;
                case MODE_IRQ: e->cpsr = e->spsr_irq; break;
                case MODE_SVC: e->cpsr = e->spsr_svc; break;
                case MODE_ABT: e->cpsr = e->spsr_abt; break;
                case MODE_UND: e->cpsr = e->spsr_und; break;
                default: break;
                }
            }
        }
    } else if (a_disc3(ins) == 0b100) {
        // load/store multiple registers
        bool p = flag(ins, 24);
        bool u = flag(ins, 23);
        bool s = flag(ins, 22);
        bool w = flag(ins, 21);
        bool l = flag(ins, 20);
        debug("s: %d", s);
        u32 reglist = ins & 0xffff;
        reg rn = a_rn(ins);
        u32 addr = r(e, rn);
        u32 addrstart = addr;
        bool loading_pc = l && reglist >> PC & 0b1;
        ptrdiff_t regcount = 0;
        for (ptrdiff_t i = 0; i < 16; ++i) if (reglist >> i & 0b1) regcount += 1;
        u32 addrfinal = u ? addr + regcount * 4 : addr - regcount * 4;
        if (!u) addrstart -= regcount * 4;
        if (p == u) addrstart += 4;
        mode m = e->cpsr.mode;
        if (s && !loading_pc) e->cpsr.mode = MODE_USR;
        if (l && w) {
            sr(e, rn, addrfinal);
            if (rn == PC) e->branched = true;
        }
        u32 a = addrstart;
        bool wroteback = false;
        ptrdiff_t opcount = 0;
        for (ptrdiff_t i = 0; i < 16; ++i) {
            if (reglist >> i & 0b1) {
                if (l) sr(e, i, mem_read32_raw(e, a));
                else {
                    u32 val = r(e, i);
                    if (i == PC && !(rn == PC && w)) {
                        debug("hiiii");
                        val += 4;
                    }
                    mem_write32(e, a, val);
                }
                a += 4;
                opcount += 1;
            }
            if ((opcount == 1 || i == 15) && !l && !wroteback) {
                wroteback = true;
                if (w) {
                    sr(e, rn, addrfinal);
                    if (rn == PC) e->branched = true;
                }
            }
        }
        e->cpsr.mode = m;
        if (loading_pc) {
            e->branched = true;
            if (s) {
                switch (e->cpsr.mode) {
                case MODE_FIQ: e->cpsr = e->spsr_fiq; break;
                case MODE_IRQ: e->cpsr = e->spsr_irq; break;
                case MODE_SVC: e->cpsr = e->spsr_svc; break;
                case MODE_ABT: e->cpsr = e->spsr_abt; break;
                case MODE_UND: e->cpsr = e->spsr_und; break;
                default: break;
                }
            }
        }
    } else if (a_disc3(ins) == 0b110) {
        panic("coprocessor load and store");
        return false;
    } else if (a_disc3(ins) == 0b101) {
        // branch (optionally with link)
        bool l = flag(ins, 24);
        if (l) sr(e, LR, r(e, PC) - 4);
        u32 uoff = ins & 0xffffff;
        u32 soff = ((s32) (uoff << 8)) >> 8; // sign-extend from 24-bits to 32-bits
        soff <<= 2;
        sr(e, PC, r(e, PC) + soff); e->branched = true;
    } else if (a_disc3(ins) == 0b011 && flag(ins, 4) == 1) {
        // undefined instructions
        debug("undefined");
        return false;
    }
end:
    u32 branch_offset = e->branched ? 8 : 4;
    if (e->cpsr.t) branch_offset >>= 1;
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

static inline void t_loadstorehalf(emu *e, u32 ins, u32 off, bool h) {
    bool l = flag(ins, 11);
    reg rn = t_rn(ins);
    reg rd = t_rd(ins);
    u32 addr = r(e, rn);
    addr += off;
    if (l) {
        if (h) sr(e, rd, mem_read16(e, addr));
        else sr(e, rd, sext8(mem_read8(e, addr)));
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
        // load/store half register
        u32 off = r(e, t_rm(ins));
        t_loadstorehalf(e, ins, off, flag(ins, 11));
    } else if (a_disc4(ins) == 0b1000) {
        // load/store half immediate
        u32 off = ins >> 6 & 0b11111;
        t_loadstorehalf(e, ins, off, false);
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
