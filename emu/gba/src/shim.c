#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <dlfcn.h>

#include <emacs-module.h>

int plugin_is_GPL_compatible;

void *HANDLE = NULL;

typedef int (*func_emacs_module_init)(struct emacs_runtime *runtime);

typedef emacs_value args[];
void message(emacs_env *env, const char *msg) {
    emacs_value func = env->intern(env, "message");
    emacs_value arg = env->make_string(env, msg, strlen(msg));
    env->funcall(env, func, 1, (args){arg});
}

int emacs_module_init(struct emacs_runtime *runtime) {
	if (runtime->size < (long int) sizeof(*runtime)) return 1;
	emacs_env *env = runtime->get_environment(runtime);
	if (env->size < (long int) sizeof(*env)) return 2;
    if (HANDLE) dlclose(HANDLE);
    HANDLE = dlopen("/home/llll/src/upsidedowncake/emu/gba/udc_gba_emu.so", RTLD_LAZY);
    if (!HANDLE) {
        char buf[1024];
        snprintf(buf, sizeof(buf), "error in module shim: %s\n", dlerror());
        message(env, buf);
        return 2;
    }
    func_emacs_module_init f = dlsym(HANDLE, "emacs_module_init");
    return f(runtime);
}
