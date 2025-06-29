#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include <emacs-module.h>

#include "utils.h"

emacs_env *ENV = NULL;

typedef emacs_value args[];

void defvar(emacs_env *env, const char *nm, emacs_value x) {
	emacs_value symbol = env->intern(env, nm);
	emacs_value args[] = {symbol, x};
	env->funcall(env, env->intern(env, "set"), 2, args);
}
void defun(emacs_env *env, const char *nm, const char *docs, ptrdiff_t arity, emacs_value (*func)(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *)) {
	emacs_value func_init_window = env->make_function(env, arity, arity, func, docs, NULL);
	emacs_value symbol = env->intern(env, nm);
	emacs_value args[] = {symbol, func_init_window};
	env->funcall(env, env->intern(env, "defalias"), 2, args);
}
void message(emacs_env *env, const char *msg) {
    emacs_value func = env->intern(env, "message");
    emacs_value arg = env->make_string(env, msg, strlen(msg));
    env->funcall(env, func, 1, (args){arg});
}

void panic_(const char *format, ...) {
    if (!ENV) return;
    char buf[1024];
    va_list vargs;
    va_start(vargs, format);
    vsnprintf(buf, sizeof(buf), format, vargs);
    va_end(vargs);
    ENV->non_local_exit_signal(ENV,
        ENV->intern(ENV, "error"),
        ENV->funcall(ENV, ENV->intern(ENV, "list"), 1, (args){
            ENV->make_string(ENV, buf, strlen(buf)),
        })
    );
    fputs(buf, stderr);
}

void debug_(const char *format, ...) {
    if (!ENV) return;
    char buf[1024];
    va_list vargs;
    va_start(vargs, format);
    vsnprintf(buf, sizeof(buf), format, vargs);
    va_end(vargs);
    message(ENV, buf);
}
