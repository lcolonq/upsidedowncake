#pragma once

#include <stdint.h>

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

typedef enum reg {
    R0 = 0, R1, R2, R3, R4, R5, R6, R7,
    R8, R9, R10, R11, R12, R13, R14, R15,
    REG_COUNT,
} reg;
#define PC R15
#define LR R15

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;

typedef struct mem {
    u8 ewram[256 * 1024];
    u8 iwram[32 * 1024];
    u8 rom[32 * 1024 * 1024];
} mem;

typedef struct emu {
    enum { INS_ARM, INS_THUMB } instruction_set;
    u32 reg[REG_COUNT];
    mem mem;
} emu;

void emulate(emu *);
