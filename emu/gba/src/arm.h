#pragma once

#include <stdint.h>
#include <stddef.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

typedef enum mode {
    MODE_USR = 0b10000,
    MODE_FIQ = 0b10001,
    MODE_IRQ = 0b10010,
    MODE_SVC = 0b10011,
    MODE_ABT = 0b10111,
    MODE_UND = 0b11011,
    MODE_SYS = 0b11111,
} mode;

typedef enum cond {
    COND_EQ = 0b0000, COND_NE = 0b0001,
    COND_CS = 0b0010, COND_CC = 0b0011,
    COND_MI = 0b0100, COND_PL = 0b0101,
    COND_VS = 0b0110, COND_VC = 0b0111,
    COND_HI = 0b1000, COND_LS = 0b1001,
    COND_GE = 0b1010, COND_LT = 0b1011,
    COND_GT = 0b1100, COND_LE = 0b1101,
    COND_AL = 0b1110,
} cond;

typedef enum dpop {
    DPOP_AND = 0b0000, DPOP_EOR = 0b0001, DPOP_SUB = 0b0010, DPOP_RSB = 0b0011,
    DPOP_ADD = 0b0100, DPOP_ADC = 0b0101, DPOP_SBC = 0b0110, DPOP_RSC = 0b0111,
    DPOP_TST = 0b1000, DPOP_TEQ = 0b1001, DPOP_CMP = 0b1010, DPOP_CMN = 0b1011,
    DPOP_ORR = 0b1100, DPOP_MOV = 0b1101, DPOP_BIC = 0b1110, DPOP_MVN = 0b1111,
} dpop;

typedef enum tdpop {
    TDPOP_AND = 0b0000, TDPOP_EOR = 0b0001, TDPOP_LSL = 0b0010, TDPOP_LSR = 0b0011,
    TDPOP_ASR = 0b0100, TDPOP_ADC = 0b0101, TDPOP_SBC = 0b0110, TDPOP_ROR = 0b0111,
    TDPOP_TST = 0b1000, TDPOP_NEG = 0b1001, TDPOP_CMP = 0b1010, TDPOP_CMN = 0b1011,
    TDPOP_ORR = 0b1100, TDPOP_MUL = 0b1101, TDPOP_BIC = 0b1110, TDPOP_MVN = 0b1111,
} tdpop;

typedef enum thop {
    THOP_ADD = 0b00, THOP_CMP = 0b01, THOP_MOV = 0b10, THOP_BX = 0b11,
} thop;

typedef enum shift {
    SHIFT_LSL = 0b00, SHIFT_LSR = 0b01,
    SHIFT_ASR = 0b10, SHIFT_ROR = 0b11,
} shift;

typedef enum reg {
    R0 = 0, R1, R2, R3, R4, R5, R6, R7,
    R8, R9, R10, R11, R12, R13, R14, R15,
    REG_COUNT,
} reg;
#define PC R15
#define LR R14
#define SP R13
typedef enum reg_fiq {
    R8_FIQ = 8, R9_FIQ, R10_FIQ, R11_FIQ, R12_FIQ, R13_FIQ, R14_FIQ,
} reg_fiq;
typedef enum reg_irq { R13_IRQ = 13, R14_IRQ, } reg_irq;
typedef enum reg_svc { R13_SVC = 13, R14_SVC, } reg_svc;
typedef enum reg_abt { R13_ABT = 13, R14_ABT, } reg_abt;
typedef enum reg_und { R13_UND = 13, R14_UND, } reg_und;

typedef struct cpsr {
    bool n, z, c, v;
    bool i, f, t;
    u32 dnm;
    mode mode;
} cpsr;

typedef struct mem {
    u8 ewram[256 * 1024];
    u8 iwram[32 * 1024];
    u8 rom[32 * 1024 * 1024];
} mem;

typedef struct transaction {
    enum { TRANS_INSREAD = 0, TRANS_READ, TRANS_WRITE } kind;
    u32 addr;
    u32 size;
    u32 data;
} transaction;

typedef struct load {
    bool valid;
    u32 addr;
    u32 data;
    u32 size;
} load;

#define TRANS_MAX 32
typedef struct trans {
    bool record;
    ptrdiff_t idx;
    transaction data[TRANS_MAX];
    load loads[TRANS_MAX];
} trans;

typedef struct emu {
    u32 reg[REG_COUNT];
    u32 reg_fiq[7], reg_irq[2], reg_svc[2], reg_abt[2], reg_und[2];
    cpsr cpsr;
    cpsr spsr_fiq, spsr_irq, spsr_svc, spsr_abt, spsr_und;
    mem mem;
    bool branched;
    trans trans;
} emu;

cpsr make_cpsr(u32 x);
u32 cpsr_to_u32(cpsr x);
bool emulate_arm_ins(emu *e, u32 ins);
bool emulate_thumb_ins(emu *e, u16 ins);
void emulate(emu *e, u32 fuel);
