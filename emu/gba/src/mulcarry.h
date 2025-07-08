/*
 * Multiplication carry flag algorithm modified from NanoBoyAdvance source code, licensed as follows:
 *
 * Copyright (C) 2024 fleroviux
 *
 * Licensed under GPLv3 or any later version.
 * Refer to the included LICENSE file.
 *
 * Multiplication carry flag algorithm has been altered from its original form according to its GPL-compatible license, as follows:
 *
 * Copyright (C) 2024 zaydlang, calc84maniac
 *
 * This software is provided 'as-is', without any express or implied warranty. In no event will the authors be held liable for any damages arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose, including commercial applications, and to alter it and redistribute it freely, subject to the following restrictions:
 *
 *   1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.
 *   2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.
 *   3. This notice may not be removed or altered from any source distribution.
 */

#pragma once
#include "arm.h"
bool TickMultiply(u32 multiplier, bool is_signed) {
  u32 mask = 0xFFFFFF00;
  while (true) {
    multiplier &= mask;
    if (multiplier == 0) break;
    if (is_signed) {
      if (multiplier == mask) break;
    }
    mask <<= 8;
  }
  // Return true if full ticks used.
  return mask == 0;
}

bool MultiplyCarrySimple(u32 multiplier) {
  // Carry comes directly from final injected booth carry bit.
  // Final booth addend is negative only if upper 2 bits are 10.
  return (multiplier >> 30) == 2;
}

bool MultiplyCarryLo(u32 multiplicand, u32 multiplier, u32 accum) {
  // Set low bit of multiplicand to cause negation to invert the upper bits.
  // This bit cannot propagate to the resulting carry bit.
  multiplicand |= 1;

  // Optimized first iteration.
  u32 booth = (s32)(multiplier << 31) >> 31;
  u32 carry = multiplicand * booth;
  u32 sum = carry + accum;

  int shift = 29;
  do {
    // Process 8 multiplier bits using 4 booth iterations.
    for (int i = 0; i < 4; i++, shift -= 2) {
      // Get next booth factor (-2 to 2, shifted left by 30-shift).
      u32 next_booth = (s32)(multiplier << shift) >> shift;
      u32 factor = next_booth - booth;
      booth = next_booth;
      // Get scaled value of booth addend.
      u32 addend = multiplicand * factor;
      // Accumulate addend with carry-save add.
      accum ^= carry ^ addend;
      sum += addend;
      carry = sum - accum;
    }
  } while (booth != multiplier);

  // Carry flag comes from bit 31 of carry-save adder's final carry.
  return carry >> 31;
}

bool MultiplyCarryHi(u32 multiplicand, u32 multiplier, u32 accum_hi, bool sign_extend) {
    // Only last 3 booth iterations are relevant to output carry.
    // Reduce scale of both inputs to get upper bits of 64-bit booth addends
    // in upper bits of 32-bit values, while handling sign extension.
    if (sign_extend) {
        multiplicand = (s32)multiplicand >> 6;
        multiplier = (s32)multiplier >> 26;
    } else {
        multiplicand >>= 6;
        multiplier >>= 26;
    }
    // Set low bit of multiplicand to cause negation to invert the upper bits.
    // This bit cannot propagate to the resulting carry bit.
    multiplicand |= 1;

    // Pre-populate magic bit 61 for carry.
    u32 carry = ~accum_hi & 0x20000000;
    // Pre-populate magic bits 63-60 for accum (with carry magic pre-added).
    u32 accum = accum_hi - 0x08000000;

    // Get factors for last 3 booth iterations.
    u32 booth0 = (s32)(multiplier << 27) >> 27;
    u32 booth1 = (s32)(multiplier << 29) >> 29;
    u32 booth2 = (s32)(multiplier << 31) >> 31;
    u32 factor0 = multiplier - booth0;
    u32 factor1 = booth0 - booth1;
    u32 factor2 = booth1 - booth2;

    // Get scaled value of 3rd-last booth addend.
    u32 addend = multiplicand * factor2;
    // Finalize bits 61-60 of accum magic using its sign.
    accum -= addend & 0x10000000;
    // Get scaled value of 2nd-last booth addend.
    addend = multiplicand * factor1;
    // Finalize bits 63-62 of accum magic using its sign.
    accum -= addend & 0x40000000;

    // Get carry from carry-save add in bit 61 and propagate it to bit 62.
    u32 sum = accum + (addend & 0x20000000);
    // Subtract out carry magic to get actual accum magic.
    accum -= carry;

    // Get scaled value of last booth addend.
    addend = multiplicand * factor0;
    // Add to bit 62 and propagate carry.
    sum += addend & 0x40000000;

    // Cancel out accum magic bit 63 to get carry bit 63.
    return (sum ^ accum) >> 31;
}
