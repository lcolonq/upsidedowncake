#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>

#include <emacs-module.h>
#include <capstone/capstone.h>

#include "utils.h"
#include "arm.h"

int plugin_is_GPL_compatible;

emu EMU;

emacs_value disassemble(emacs_env *env, u8 *code) {
    csh handle;
    cs_insn *insn;
    if (cs_open(CS_ARCH_ARM, CS_MODE_ARM, &handle) != CS_ERR_OK) {
        panic("failed to open capstone handler");
        return env->intern(env, "nil");
    }
    size_t count = cs_disasm(handle, code, 4, 0, 0, &insn);
    if (count < 1) {
        panic("failed to disassemble");
        return env->intern(env, "nil");
    }
    char buf[256];
    snprintf(buf, sizeof(buf), "%s %s", insn[0].mnemonic, insn[0].op_str);
    cs_free(insn, count);
    cs_close(&handle);
    return env->make_string(env, buf, strlen(buf));
}

emacs_value c_emulate(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *) {
    ENV = env;
    EMU.reg[PC] = 0x08000000 + 4;

    if (nargs != 2) {
        panic("expected 2 arguments, got %d", nargs);
        goto error;
    }
    emacs_value vec = args[0];
    if (!env->eq(env, env->type_of(env, vec), env->intern(env, "vector"))) {
        panic("expected a vector argument");
        goto error;
    }
    emacs_value fuel = args[1];
    if (!env->eq(env, env->type_of(env, fuel), env->intern(env, "integer"))) {
        panic("expected an integer argument");
        goto error;
    }
    ptrdiff_t i = 0;
    for (; i < env->vec_size(env, vec); ++i) {
        emacs_value elem = env->vec_get(env, vec, i);
        if (!env->eq(env, env->type_of(env, elem), env->intern(env, "integer"))) {
            panic("expected an integer at index %d", i);
            goto error;
        }
        EMU.mem.rom[i] = env->extract_integer(env, elem);
    }
    EMU.mem.rom[i++] = 0b00010000;
    EMU.mem.rom[i++] = 0b00000000;
    EMU.mem.rom[i++] = 0b00000000;
    EMU.mem.rom[i++] = 0b00000110;

    emulate(&EMU, env->extract_integer(env, fuel));
    return env->intern(env, "nil");
error:
    return env->intern(env, "nil");
}

emacs_value c_emulate_test_arm(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *) {
    ENV = env;
    EMU.reg[PC] = 0x08000000 + 4;

    if (nargs != 1) {
        panic("expected 1 arguments, got %d", nargs);
        goto error;
    }
    emacs_value argvec = args[0];
    emacs_value ins = env->vec_get(env, argvec, 0);
    if (!env->eq(env, env->type_of(env, ins), env->intern(env, "integer"))) {
        panic("expected a 32-bit integer instruction as the first argument");
        goto error;
    }
    emacs_value regs = env->vec_get(env, argvec, 1);
    if (!env->eq(env, env->type_of(env, regs), env->intern(env, "vector"))) {
        panic("expected a vector of mode register vectors as the second argument");
        goto error;
        if (env->vec_size(env, regs) != 6) {
            panic("expected register vector to contain 6 sub-vectors, one for each mode");
            goto error;
        }
    }
    emacs_value regs_usr = env->vec_get(env, regs, 0);
    emacs_value regs_fiq = env->vec_get(env, regs, 1);
    emacs_value regs_irq = env->vec_get(env, regs, 2);
    emacs_value regs_svc = env->vec_get(env, regs, 3);
    emacs_value regs_abt = env->vec_get(env, regs, 4);
    emacs_value regs_und = env->vec_get(env, regs, 5);
    emacs_value cpsr = env->vec_get(env, argvec, 2);
    if (!env->eq(env, env->type_of(env, cpsr), env->intern(env, "integer"))) {
        panic("expected a CPSR as the third argument");
        goto error;
    }
    emacs_value spsrs = env->vec_get(env, argvec, 3);
    if (!env->eq(env, env->type_of(env, spsrs), env->intern(env, "vector"))) {
        panic("expected a vector of SPSRs as the fourth argument");
        if (env->vec_size(env, regs) != 5) {
            panic("expected SPSR vector to contain 5 elements");
            goto error;
        }
        goto error;
    }

    for (ptrdiff_t i = 0; i < 16; ++i) EMU.reg[i] = env->extract_integer(env, env->vec_get(env, regs_usr, i));
    for (ptrdiff_t i = 0; i < 7; ++i) EMU.reg_fiq[i] = env->extract_integer(env, env->vec_get(env, regs_fiq, i));
    for (ptrdiff_t i = 0; i < 2; ++i) EMU.reg_irq[i] = env->extract_integer(env, env->vec_get(env, regs_irq, i));
    for (ptrdiff_t i = 0; i < 2; ++i) EMU.reg_svc[i] = env->extract_integer(env, env->vec_get(env, regs_svc, i));
    for (ptrdiff_t i = 0; i < 2; ++i) EMU.reg_abt[i] = env->extract_integer(env, env->vec_get(env, regs_abt, i));
    for (ptrdiff_t i = 0; i < 2; ++i) EMU.reg_und[i] = env->extract_integer(env, env->vec_get(env, regs_und, i));
    EMU.cpsr = make_cpsr(env->extract_integer(env, cpsr));
    EMU.spsr_fiq = make_cpsr(env->extract_integer(env, env->vec_get(env, spsrs, 0)));
    EMU.spsr_irq = make_cpsr(env->extract_integer(env, env->vec_get(env, spsrs, 1)));
    EMU.spsr_svc = make_cpsr(env->extract_integer(env, env->vec_get(env, spsrs, 2)));
    EMU.spsr_abt = make_cpsr(env->extract_integer(env, env->vec_get(env, spsrs, 3)));
    EMU.spsr_und = make_cpsr(env->extract_integer(env, env->vec_get(env, spsrs, 4)));

    u32 insbytes = env->extract_integer(env, ins);
    emulate_arm_ins(&EMU, insbytes);

    for (ptrdiff_t i = 0; i < 16; ++i) env->vec_set(env, regs_usr, i, env->make_integer(env, EMU.reg[i]));
    for (ptrdiff_t i = 0; i < 7; ++i) env->vec_set(env, regs_fiq, i, env->make_integer(env, EMU.reg_fiq[i]));
    for (ptrdiff_t i = 0; i < 2; ++i) env->vec_set(env, regs_irq, i, env->make_integer(env, EMU.reg_irq[i]));
    for (ptrdiff_t i = 0; i < 2; ++i) env->vec_set(env, regs_svc, i, env->make_integer(env, EMU.reg_svc[i]));
    for (ptrdiff_t i = 0; i < 2; ++i) env->vec_set(env, regs_abt, i, env->make_integer(env, EMU.reg_abt[i]));
    for (ptrdiff_t i = 0; i < 2; ++i) env->vec_set(env, regs_und, i, env->make_integer(env, EMU.reg_und[i]));
    env->vec_set(env, argvec, 2, env->make_integer(env, cpsr_to_u32(EMU.cpsr)));
    env->vec_set(env, spsrs, 0, env->make_integer(env, cpsr_to_u32(EMU.spsr_fiq)));
    env->vec_set(env, spsrs, 1, env->make_integer(env, cpsr_to_u32(EMU.spsr_irq)));
    env->vec_set(env, spsrs, 2, env->make_integer(env, cpsr_to_u32(EMU.spsr_svc)));
    env->vec_set(env, spsrs, 3, env->make_integer(env, cpsr_to_u32(EMU.spsr_abt)));
    env->vec_set(env, spsrs, 4, env->make_integer(env, cpsr_to_u32(EMU.spsr_und)));

    return disassemble(env, (u8 *) &insbytes);
error:
    return env->intern(env, "nil");
}

int emacs_module_init(struct emacs_runtime *runtime) {
	if (runtime->size < (long int) sizeof(*runtime)) return 1;
	emacs_env *env = runtime->get_environment(runtime);
	if (env->size < (long int) sizeof(*env)) return 2;
    ENV = env;
    defun(env, "u/gba/c-emulate", "Emulate the given vector of bytes.", 2, c_emulate);
    defun(env, "u/gba/c-emulate-test-arm", "Emulate one instruction from the given state.", 1, c_emulate_test_arm);
    return 0;
}
