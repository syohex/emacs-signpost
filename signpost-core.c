/*
  Copyright (C) 2020 by Shohei YOSHIDA

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#define _DEFAULT_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <emacs-module.h>

#include <os/signpost.h>

int plugin_is_GPL_compatible;

struct signpost_data {
    os_log_t log_obj;
    os_signpost_id_t signpost_id;
};

static char *retrieve_string(emacs_env *env, emacs_value str, ptrdiff_t *size) {
    *size = 0;

    env->copy_string_contents(env, str, NULL, size);
    char *p = malloc(*size);
    if (p == NULL) {
        *size = 0;
        return NULL;
    }
    env->copy_string_contents(env, str, p, size);

    return p;
}

static void signpost_free(void *arg) {
    free(arg);
}

static emacs_value Fsignpost_log_create(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
    ptrdiff_t size;
    char *subsystem = retrieve_string(env, args[0], &size);
    if (subsystem == NULL) {
        return env->intern(env, "nil");
    }

    struct signpost_data *pd = malloc(sizeof(struct signpost_data));
    pd->log_obj = os_log_create(subsystem, OS_LOG_CATEGORY_POINTS_OF_INTEREST);
    pd->signpost_id = os_signpost_id_generate(pd->log_obj);

    free(subsystem);
    return env->make_user_ptr(env, signpost_free, pd);
}

static emacs_value Fsignpost_begin(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
    struct signpost_data *pd = env->get_user_ptr(env, args[0]);
    os_signpost_interval_begin(pd->log_obj, pd->signpost_id, "emacs-signpost");
    return env->intern(env, "t");
}

static emacs_value Fsignpost_end(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data) {
    struct signpost_data *pd = env->get_user_ptr(env, args[0]);
    os_signpost_interval_end(pd->log_obj, pd->signpost_id, "emacs-signpost");
    return env->intern(env, "t");
}

static void bind_function(emacs_env *env, const char *name, emacs_value Sfun) {
    emacs_value Qfset = env->intern(env, "fset");
    emacs_value Qsym = env->intern(env, name);
    emacs_value args[] = {Qsym, Sfun};

    env->funcall(env, Qfset, 2, args);
}

static void provide(emacs_env *env, const char *feature) {
    emacs_value Qfeat = env->intern(env, feature);
    emacs_value Qprovide = env->intern(env, "provide");
    emacs_value args[] = {Qfeat};

    env->funcall(env, Qprovide, 1, args);
}

int emacs_module_init(struct emacs_runtime *ert) {
    emacs_env *env = ert->get_environment(ert);

#define DEFUN(lsym, csym, amin, amax, doc, data) bind_function(env, lsym, env->make_function(env, amin, amax, csym, doc, data))

    DEFUN("signpost-core-log-create", Fsignpost_log_create, 1, 1, NULL, NULL);
    DEFUN("signpost-core-begin", Fsignpost_begin, 1, 1, NULL, NULL);
    DEFUN("signpost-core-end", Fsignpost_end, 1, 1, NULL, NULL);

#undef DEFUN

    provide(env, "signpost-core");
    return 0;
}
