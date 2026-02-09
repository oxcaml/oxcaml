/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* Start-up code */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/codefrag.h"
#include "caml/debugger.h"
#include "caml/runtime_events.h"
#include "caml/fiber.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/intext.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/printexc.h"
#include "caml/signals.h"
#include "caml/stack.h"
#include "caml/startup_aux.h"
#include "caml/sys.h"
#include "caml/globroots.h"
#include "caml/frame_descriptors.h"

extern int caml_parser_trace;
extern char caml_system__code_begin, caml_system__code_end;
/* The two symbols above are defined in runtime/$ARCH.S.
   They use the old `__` separator convention because the new convention
   gives `caml_system__code_begin`, which is not a valid C identifier. */

extern uintnat caml_prelinking_in_use;

/* Initialize the static data and code area limits. */

struct segment { char * begin; char * end; };

static void init_segments(void)
{
  extern struct segment caml_code_segments[];
  char * caml_code_area_start, * caml_code_area_end;
  int i;

  if (caml_prelinking_in_use) {
    /* Register each segment as a separate code fragment */
    for (i = 0; caml_code_segments[i].begin != 0; i++) {
      caml_register_code_fragment(caml_code_segments[i].begin,
                                  caml_code_segments[i].end,
                                  DIGEST_LATER, NULL);
    }
  } else {
    caml_code_area_start = caml_code_segments[0].begin;
    caml_code_area_end = caml_code_segments[0].end;
    for (i = 1; caml_code_segments[i].begin != 0; i++) {
      if (caml_code_segments[i].begin < caml_code_area_start)
        caml_code_area_start = caml_code_segments[i].begin;
      if (caml_code_segments[i].end > caml_code_area_end)
        caml_code_area_end = caml_code_segments[i].end;
    }
    /* Register the code in the table of code fragments */
    caml_register_code_fragment(caml_code_area_start,
                                caml_code_area_end,
                                DIGEST_LATER, NULL);
  }
  /* Also register the glue code written in assembly */
  caml_register_code_fragment(&caml_system__code_begin,
                              &caml_system__code_end,
                              DIGEST_IGNORE, NULL);
}

extern value caml_start_program (caml_domain_state*);
#ifdef _WIN32
extern void caml_win32_overflow_detection (void);
#endif

#ifdef _MSC_VER

/* PR 4887: avoid crash box of windows runtime on some system calls */
extern void caml_install_invalid_parameter_handler(void);

#endif

value caml_startup_common(char_os **argv, int pooling)
{
  char_os * exe_name, * proc_self_exe;
  value res;

  caml_init_os_params();

  /* Determine options */
  caml_parse_ocamlrunparam();

#ifdef DEBUG
  // Silenced in oxcaml to make it easier to run tests that
  // check program output.
  // CAML_GC_MESSAGE (ANY, "### OCaml runtime: debug mode ###\n");
#endif
  if (caml_params->cleanup_on_exit)
    pooling = 1;
  if (!caml_startup_aux(pooling))
    return Val_unit;

  caml_init_codefrag();
  caml_init_locale();
#ifdef _MSC_VER
  caml_install_invalid_parameter_handler();
#endif
  caml_init_custom_operations();
  caml_init_gc ();

  /* runtime_events's init can cause a stop-the-world pause, so it must be done
     after we've initialised the garbage collector */
  CAML_RUNTIME_EVENTS_INIT();

  init_segments();
  caml_init_signals();
  caml_init_nat_signals();
#ifdef _WIN32
  caml_win32_overflow_detection();
#endif
  caml_debugger_init (); /* force debugger.o stub to be linked */
  exe_name = argv[0];
  if (exe_name == NULL) exe_name = T("");
  proc_self_exe = caml_executable_name();
  if (proc_self_exe != NULL)
    exe_name = proc_self_exe;
  else
    exe_name = caml_search_exe_in_path(exe_name);
  caml_sys_init(exe_name, argv);
  caml_maybe_expand_stack();
  res = caml_start_program(Caml_state);
  /* ignore distinction between async and normal,
     it's an uncaught exception either way */
  Caml_state->raising_async_exn = 0;
  caml_terminate_signals();
  return res;
}

value caml_startup_exn(char_os **argv)
{
  return caml_startup_common(argv, /* pooling */ 0);
}

void caml_startup(char_os **argv)
{
  value res = caml_startup_exn(argv);
  if (Is_exception_result(res))
    caml_fatal_uncaught_exception(Extract_exception(res));
}

void caml_main(char_os **argv)
{
  caml_startup(argv);
}

value caml_startup_pooled_exn(char_os **argv)
{
  return caml_startup_common(argv, /* pooling */ 1);
}

void caml_startup_pooled(char_os **argv)
{
  value res = caml_startup_pooled_exn(argv);
  if (Is_exception_result(res))
    caml_fatal_uncaught_exception(Extract_exception(res));
}

/* Unit dependency table for manual module initialization.
   This table is emitted by the compiler when -manual-module-init is used. */

/* Initialization states.
   Values are valid OCaml values (Val_int encodings). */
enum init_state {
  INIT_STATE_NOT_INITIALIZED = 1,  /* Val_int(0) */
  INIT_STATE_INITIALIZING    = 3,  /* Val_int(1) */
  INIT_STATE_DONE            = 5,  /* Val_int(2) */
  INIT_STATE_FAILED          = 7   /* Val_int(3) */
};

struct caml_unit_deps_entry {
  const char *unit_name;       /* compilation unit name */
  void *entry_fn;              /* entry function (raw code pointer) */
  value *gc_roots;             /* pointer to gc_roots (module block) */
  intnat *frametable;          /* pointer to frametable */
  intnat num_deps;             /* number of dependencies */
  const intnat *dep_indices;   /* array of indices into caml_unit_deps_table */
  enum init_state init_state;  /* one of INIT_STATE_* */
  value raised_exn;            /* stored exception if INIT_STATE_FAILED */
};

struct caml_unit_deps_table {
  intnat num_units;
  struct caml_unit_deps_entry entries[];
};

/* Always present; empty (num_units = 0) when not using -manual-module-init */
extern struct caml_unit_deps_table caml_unit_deps_table;

/* Binary search for a unit by name. Returns NULL if not found. */
static struct caml_unit_deps_entry *
caml_unit_deps_find(const char *name)
{
  intnat lo = 0;
  intnat hi = caml_unit_deps_table.num_units - 1;

  while (lo <= hi) {
    intnat mid = lo + (hi - lo) / 2;
    int cmp = strcmp(name, caml_unit_deps_table.entries[mid].unit_name);
    if (cmp == 0) {
      return &caml_unit_deps_table.entries[mid];
    } else if (cmp < 0) {
      hi = mid - 1;
    } else {
      lo = mid + 1;
    }
  }
  return NULL;
}

/* Returns Make_exception_result(Failure msg) where msg is formatted
   with the given printf-style format and name argument. */
static value init_module_failure(const char *fmt, const char *name)
{
  size_t len = strlen(fmt) + strlen(name);
  char *msg = caml_stat_alloc(len);
  snprintf(msg, len, fmt, name);
  value exn = caml_failure_exn(msg);
  caml_stat_free(msg);
  return Make_exception_result(exn);
}

/* Initialize a module and its dependencies in topological order.
   Uses a recursive depth-first approach.
   Returns an exception result on failure, Val_unit on success. */
static value caml_init_module_rec(struct caml_unit_deps_entry *entry)
{
  CAMLparam0();
  CAMLlocal1(closure);

  /* Check current state */
  if (entry->init_state == INIT_STATE_DONE) {
    CAMLreturn(Val_unit);
  }

  if (entry->init_state == INIT_STATE_FAILED) {
    CAMLreturn(Make_exception_result(entry->raised_exn));
  }

  if (entry->init_state == INIT_STATE_INITIALIZING) {
    CAMLreturn(init_module_failure(
      "caml_init_module: cycle detected at module %s",
      entry->unit_name));
  }

  /* Mark as initializing before processing dependencies */
  entry->init_state = INIT_STATE_INITIALIZING;

  /* Initialize all dependencies first */
  for (intnat i = 0; i < entry->num_deps; i++) {
    intnat dep_idx = entry->dep_indices[i];
    struct caml_unit_deps_entry *dep =
      &caml_unit_deps_table.entries[dep_idx];
    value result = caml_init_module_rec(dep);
    if (Is_exception_result(result)) {
      entry->init_state = INIT_STATE_FAILED;
      entry->raised_exn = Extract_exception(result);
      caml_register_generational_global_root(&entry->raised_exn);
      CAMLreturn(result);
    }
  }

  /* CR mshinwell/ntrangez: consider sharing code with natdynlink */

  /* Register gc_roots and frametable before calling the entry function,
     matching the ordering used by both the normal startup path (where
     caml_globals[] and caml_frametable[] are statically available before
     caml_program runs) and natdynlink (which registers both before calling
     the entry point).

     gc_roots must be registered before the frametables because
     caml_register_dyn_globals can raise (upon duplicate registration). */
  if (entry->gc_roots != NULL) {
    void *globals[1] = { (void *)entry->gc_roots };
    caml_register_dyn_globals(globals, 1);
  }

  if (entry->frametable != NULL) {
    void *tables[1] = { (void *)entry->frametable };
    caml_register_frametables(tables, 1);
  }

  /* Create a closure wrapper for the entry function.
     The closure has: code pointer at field 0, closinfo at field 1.
     We use arity=0, delta=2 (no environment), is_last=1. */
  closure = caml_alloc_small(2, Closure_tag);
  Field(closure, 0) = (value)entry->entry_fn;
  Closinfo_val(closure) = Make_closinfo(0, 2, 1);

  /* Call the entry function (takes no arguments, but pass Val_unit) */
  value result = caml_callback_exn(closure, Val_unit);
  if (Is_exception_result(result)) {
    entry->init_state = INIT_STATE_FAILED;
    entry->raised_exn = Extract_exception(result);
    caml_register_generational_global_root(&entry->raised_exn);
    CAMLreturn(result);
  }

  /* Mark as done */
  entry->init_state = INIT_STATE_DONE;

  CAMLreturn(Val_unit);
}

/* Public API: Initialize a module by name (exception-returning variant) */
CAMLexport value caml_init_module_exn(const char *name)
{
  struct caml_unit_deps_entry *entry = caml_unit_deps_find(name);
  if (entry == NULL) {
    return init_module_failure(
      "caml_init_module: unit %s not found", name);
  }
  return caml_init_module_rec(entry);
}

/* Public API: Initialize a module by name (raises on failure) */
CAMLexport void caml_init_module(const char *name)
{
  value result = caml_init_module_exn(name);
  if (Is_exception_result(result)) {
    caml_raise(Extract_exception(result));
  }
}

/* OCaml-callable wrapper: Initialize a module by name (string -> unit) */
CAMLprim value caml_init_module_from_ocaml(value v_name)
{
  /* Copy the string to C heap since caml_init_module_exn may trigger GC */
  char *name = caml_stat_strdup(String_val(v_name));
  value result = caml_init_module_exn(name);
  caml_stat_free(name);
  if (Is_exception_result(result)) {
    caml_raise(Extract_exception(result));
  }
  return Val_unit;
}
