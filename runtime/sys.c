/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
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

/* Basic system calls */

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef _WIN32
#include <direct.h> /* for _wchdir and _wgetcwd */
#include <io.h> /* for _wopen and close */
#else
#include <sys/wait.h>
#endif
#include "caml/config.h"
#ifndef _WIN32
#include <unistd.h>
#endif
#ifdef HAS_TIMES
#include <sys/times.h>
#endif
#ifdef HAS_GETRUSAGE
#include <sys/time.h>
#include <sys/resource.h>
#endif
#ifdef HAS_GETTIMEOFDAY
#include <sys/time.h>
#endif
#if defined(HAS_GETENTROPY) && defined(__APPLE__)
#include <sys/random.h>
#endif
#include "caml/alloc.h"
#include "caml/debugger.h"
#include "caml/runtime_events.h"
#include "caml/fail.h"
#include "caml/gc_ctrl.h"
#include "caml/major_gc.h"
#include "caml/io.h"
#include "caml/mlvalues.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/fiber.h"
#include "caml/sys.h"
#include "caml/startup.h"
#include "caml/callback.h"
#include "caml/startup_aux.h"
#include "caml/major_gc.h"
#include "caml/shared_heap.h"

CAMLexport char * caml_strerror(int errnum, char * buf, size_t buflen)
{
#ifdef _WIN32
  /* Windows has a thread-safe strerror */
  return strerror(errnum);
#else
  int res = strerror_r(errnum, buf, buflen);
  /* glibc<2.13 returns -1/sets errno, >2.13 returns +ve errno.
     We assume that buffer size is large enough not to get ERANGE,
     so we assume we got EINVAL. */
  if (res != 0) {
    snprintf(buf, buflen, "Unknown error %d", errnum);
  }
  return buf;
#endif
}

#ifndef EAGAIN
#define EAGAIN (-1)
#endif
#ifndef EWOULDBLOCK
#define EWOULDBLOCK (-1)
#endif

CAMLexport void caml_sys_error(value arg)
{
  CAMLparam1 (arg);
  char * err;
  char buf[1024];
  CAMLlocal1 (str);

  err = caml_strerror(errno, buf, sizeof(buf));
  if (arg == NO_ARG) {
    str = caml_copy_string(err);
  } else {
    mlsize_t err_len = strlen(err);
    mlsize_t arg_len = caml_string_length(arg);
    str = caml_alloc_string(arg_len + 2 + err_len);
    memcpy(&Byte(str, 0), String_val(arg), arg_len);
    memcpy(&Byte(str, arg_len), ": ", 2);
    memcpy(&Byte(str, arg_len + 2), err, err_len);
  }
  caml_raise_sys_error(str);
  CAMLnoreturn;
}

CAMLexport void caml_sys_io_error(value arg)
{
  if (errno == EAGAIN || errno == EWOULDBLOCK) {
    caml_raise_sys_blocked_io();
  } else {
    caml_sys_error(arg);
  }
}

CAMLexport void caml_do_exit(int retcode)
{
  caml_domain_state* domain_state = Caml_state;
  struct gc_stats s;

  if ((atomic_load_relaxed(&caml_verb_gc) & CAML_GC_MSG_STATS) != 0) {
    caml_compute_gc_stats(&s);
    {
      /* cf caml_gc_counters */
      double minwords = s.alloc_stats.minor_words
        + (double) (domain_state->young_end - domain_state->young_ptr);
      double majwords = s.alloc_stats.major_words
        + (double) domain_state->allocated_words;
      double allocated_words = minwords + majwords
        - s.alloc_stats.promoted_words;
      intnat heap_words =
        s.heap_stats.pool_words + s.heap_stats.large_words;
      intnat top_heap_words =
        s.heap_stats.pool_max_words + s.heap_stats.large_max_words;

      if (heap_words == 0) {
        heap_words = Wsize_bsize(caml_heap_size(Caml_state->shared_heap));
      }

      if (top_heap_words == 0) {
        top_heap_words = caml_top_heap_words(Caml_state->shared_heap);
      }

      CAML_GC_MESSAGE(STATS,
          "allocated_words: %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
          (intnat)allocated_words);
      CAML_GC_MESSAGE(STATS,
          "minor_words: %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
          (intnat) minwords);
      CAML_GC_MESSAGE(STATS,
          "promoted_words: %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
          (intnat) s.alloc_stats.promoted_words);
      CAML_GC_MESSAGE(STATS,
          "major_words: %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
          (intnat) majwords);
      CAML_GC_MESSAGE(STATS,
          "minor_collections: %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
          (intnat) atomic_load(&caml_minor_collections_count));
      CAML_GC_MESSAGE(STATS,
          "major_collections: %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
          caml_major_cycles_completed);
      CAML_GC_MESSAGE(STATS,
          "forced_major_collections: %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
          (intnat)s.alloc_stats.forced_major_collections);
      CAML_GC_MESSAGE(STATS,
          "compactions: %"ARCH_INTNAT_PRINTF_FORMAT"u\n",
          atomic_load(&caml_compactions_count));
      CAML_GC_MESSAGE(STATS,
          "major_work_done: %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
          (intnat)s.alloc_stats.major_work_done);
      CAML_GC_MESSAGE(STATS, "heap_words: %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
                    heap_words);
      CAML_GC_MESSAGE(STATS, "top_heap_words: %"ARCH_INTNAT_PRINTF_FORMAT"d\n",
                      top_heap_words);
      CAML_GC_MESSAGE(STATS, "chunk_words: %"ARCH_INTNAT_PRINTF_FORMAT"u\n",
                      s.global_stats.chunk_words);
      CAML_GC_MESSAGE(STATS, "max chunk_words: %"ARCH_INTNAT_PRINTF_FORMAT"u\n",
                      s.global_stats.max_chunk_words);
    }
  }

/* Tear down runtime_events before we leave */
CAML_RUNTIME_EVENTS_DESTROY();

#ifndef NATIVE_CODE
  caml_debugger(PROGRAM_EXIT, Val_unit);
#endif
  if (caml_params->cleanup_on_exit)
    caml_shutdown();
#ifdef _WIN32
  caml_restore_win32_terminal();
#endif
  caml_terminate_signals();
  exit(retcode);
}

/* The array of command-line arguments, as an OCaml value (a
   generational global root).  Read and updated by the Sys primitives
   in stdlib/prims/sys_prims.c. */
CAMLexport value caml_main_argv;

void caml_sys_init(const char_os * exe_name, char_os **argv)
{
#ifdef _WIN32
  /* Initialises the caml_win32_* globals on Windows with the version of
     Windows which is running */
  caml_probe_win32_version();
#if WINDOWS_UNICODE
  caml_setup_win32_terminal();
#endif
#endif
  caml_init_exe_name(exe_name);
  caml_main_argv = caml_alloc_array((void *)caml_copy_string_of_os,
                               (char const **) argv);
  caml_register_generational_global_root(&caml_main_argv);
}
