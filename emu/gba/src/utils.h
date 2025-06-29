#pragma once

#include <stdint.h>
#include <emacs-module.h>

extern emacs_env *ENV;

void panic_(const char *format, ...);
void debug_(const char *format, ...);
#define STRSTR(x) #x
#define STR(x) STRSTR(x)
#define panic(format, ...) panic_("error [" __FILE__ ":" STR(__LINE__) "] " format "\n" __VA_OPT__(,) __VA_ARGS__)
#define debug(format, ...) debug_("[" __FILE__ ":" STR(__LINE__) "] " format "\n" __VA_OPT__(,) __VA_ARGS__)

void defvar(emacs_env *env, const char *nm, emacs_value x);
void defun(emacs_env *env, const char *nm, const char *docs, ptrdiff_t arity, emacs_value (*func)(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *));
void message(emacs_env *env, const char *msg);
