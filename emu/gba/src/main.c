#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>

#include <emacs-module.h>

#include "utils.h"
#include "arm.h"

int plugin_is_GPL_compatible;

emu EMU;

emacs_value c_emulate(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *) {
    ENV = env;
    EMU.reg[PC] = 0x08000000;

    if (nargs != 1) {
        panic("expected 1 argument, got %d", nargs);
        goto error;
    }
    emacs_value vec = args[0];
    if (!env->eq(env, env->type_of(env, vec), env->intern(env, "vector"))) {
        panic("expected a vector argument");
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

    emulate(&EMU);
    return env->intern(env, "nil");
error:
    return env->intern(env, "nil");
}

int emacs_module_init(struct emacs_runtime *runtime) {
	if (runtime->size < (long int) sizeof(*runtime)) return 1;
	emacs_env *env = runtime->get_environment(runtime);
	if (env->size < (long int) sizeof(*env)) return 2;
    ENV = env;
    defun(env, "u/gba/c-emulate", "Emulate the given vector of bytes.", 1, c_emulate);
    return 0;
}
