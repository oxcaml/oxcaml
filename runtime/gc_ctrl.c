/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*              Damien Doligez, projet Para, INRIA Rocquencourt           */
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

#include "caml/alloc.h"
#include "caml/custom.h"
#include "caml/finalise.h"
#include "caml/gc.h"
#include "caml/gc_ctrl.h"
#include "caml/gc_stats.h"
#include "caml/major_gc.h"
#include "caml/minor_gc.h"
#include "caml/shared_heap.h"
#include "caml/misc.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"
#include "caml/runtime_events.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#include "caml/frame_descriptors.h"
#endif
#include "caml/domain.h"
#include "caml/fiber.h"
#include "caml/globroots.h"
#include "caml/signals.h"
#include "caml/startup.h"
#include "caml/fail.h"
#include <string.h>
#include "caml/callback.h"

/* TODO: move this declaration to e.g. fiber.c */
atomic_uintnat caml_max_stack_wsize;
uintnat caml_fiber_wsz;

/* GC Tweaks */
/* TODO: turn these into atomics to avoid data races */
extern uintnat caml_custom_work_max_multiplier; /* see major_gc.c */
extern uintnat caml_prelinking_in_use;    /* see startup_nat.c */
extern uintnat caml_compaction_algorithm; /* see shared_heap.c */
extern uintnat caml_compact_unmap;        /* see shared_heap.c */
extern uintnat caml_pool_min_chunk_bsz;  /* see shared_heap.c */
extern uintnat caml_percent_sweep_per_mark; /* see major_gc.c */
extern uintnat caml_gc_overhead_adjustment; /* see major_gc.c */
extern uintnat caml_nohugepage_stacks;    /* see fiber.c */
extern uintnat caml_enable_segv_handler;  /* see signals.c / signals_nat.c */

/* runtime config parameters set with caml_gc_set */
extern atomic_uintnat caml_major_heap_increment; /* percent or words; see shared_heap.c */
extern atomic_uintnat caml_percent_free; /* see major_gc.c */
extern atomic_uintnat caml_max_percent_free;     /*        see major_gc.c */
extern atomic_uintnat caml_custom_major_ratio; /* see custom.c */
extern atomic_uintnat caml_custom_minor_ratio; /* see custom.c */
extern atomic_uintnat caml_custom_minor_max_bsz; /* see custom.c */
extern uintnat caml_minor_heap_max_wsz; /* see domain.c */

#define Max(x,y) ((x) < (y) ? (y) : (x))

/* Kept in sync with the twin definitions in stdlib/prims/gc_ctrl_prims.c. */
static uintnat norm_pfree (uintnat p)
{
  return Max (p, 1);
}

static uintnat norm_custom_maj (uintnat p)
{
  return Max (p, 1);
}

static uintnat norm_custom_min (uintnat p)
{
  return Max (p, 1);
}

void caml_init_gc (void)
{
  caml_minor_heap_max_wsz =
    caml_norm_minor_heap_size(caml_params->init_minor_heap_wsz);

  caml_max_stack_wsize = caml_params->init_max_stack_wsz;
  caml_fiber_wsz = caml_get_init_stack_wsize(STACK_SIZE_FIBER);
  atomic_store_relaxed(&caml_percent_free,
                       norm_pfree (caml_params->init_percent_free));
  atomic_store_relaxed(&caml_max_percent_free,
                       norm_pfree (caml_params->init_max_percent_free));
  CAML_GC_MESSAGE(STACKS, "Initial stack limit: %"
                  ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                  Bsize_wsize(caml_params->init_max_stack_wsz) / 1024);

  atomic_store_relaxed(&caml_custom_major_ratio,
                       norm_custom_maj (caml_params->init_custom_major_ratio));
  atomic_store_relaxed(&caml_custom_minor_ratio,
                       norm_custom_min (caml_params->init_custom_minor_ratio));
  atomic_store_relaxed(&caml_custom_minor_max_bsz,
                       caml_params->init_custom_minor_max_bsz);
  atomic_store_relaxed(&caml_major_heap_increment,
                       caml_params->init_major_heap_increment);

  caml_gc_phase = Phase_sweep_and_mark_main;
  #ifdef NATIVE_CODE
  caml_init_frame_descriptors();
  #endif
  caml_init_domains(caml_params->max_domains,
                    caml_params->init_minor_heap_wsz);
  caml_init_gc_stats(caml_params->max_domains);
}

/* This primitive reports which runtime variant is running, so it must
   be compiled into the runtime libraries (once per variant) rather
   than with the Gc primitives in stdlib/prims/gc_ctrl_prims.c, which
   are built only once. */
CAMLprim value caml_runtime_variant (value unit)
{
  CAMLassert (unit == Val_unit);
#if defined (DEBUG)
  return caml_copy_string ("d");
#elif defined (CAML_INSTR)
  return caml_copy_string ("i");
#else
  return caml_copy_string ("");
#endif
}

struct gc_tweak {
  const char* name;
  uintnat* ptr; /* TODO: atomic_uintnat? */
  uintnat initial_value;
};

static struct gc_tweak gc_tweaks[] = {
  { "custom_work_max_multiplier", &caml_custom_work_max_multiplier, 0 },
  { "prelinking_in_use", &caml_prelinking_in_use, 0 },
  { "compaction", &caml_compaction_algorithm, 0 },
  { "compact_unmap", &caml_compact_unmap, 0 },
  { "pool_min_chunk_size", &caml_pool_min_chunk_bsz, 0 },
  { "main_stack_size", &caml_init_main_stack_wsz, 0 },
  { "thread_stack_size", &caml_init_thread_stack_wsz, 0 },
  { "fiber_stack_size", &caml_init_fiber_stack_wsz, 0 },
  { "percent_sweep_per_mark", &caml_percent_sweep_per_mark, 0 },
  { "gc_overhead_adjustment", &caml_gc_overhead_adjustment, 0 },
  { "nohugepage_stacks", &caml_nohugepage_stacks, 0 },
  { "enable_segv_handler", &caml_enable_segv_handler, 0 },
  { "cache_stacks_per_class", &caml_cache_stacks_per_class, 0 },
  { "tick_use_usleep", &caml_tick_use_usleep, 0 },
};

enum {N_GC_TWEAKS = sizeof(gc_tweaks)/sizeof(gc_tweaks[0])};

void caml_init_gc_tweaks(void)
{
  for (int i = 0; i < N_GC_TWEAKS; i++) {
    gc_tweaks[i].initial_value = *gc_tweaks[i].ptr;
  }
}

void caml_print_gc_tweaks(void)
{
  for (int i = 0; i < N_GC_TWEAKS; i++) {
    fprintf(stderr, "%s (initial value %ld)\n",
	gc_tweaks[i].name,
	gc_tweaks[i].initial_value);
  }
}

uintnat* caml_lookup_gc_tweak(const char* name, uintnat len)
{
  for (int i = 0; i < N_GC_TWEAKS; i++) {
    if (strlen(gc_tweaks[i].name) == len &&
        memcmp(gc_tweaks[i].name, name, len) == 0) {
      return gc_tweaks[i].ptr;
    }
  }
  return NULL;
}

CAMLprim value caml_gc_tweak_get(value name)
{
  CAMLparam1(name);
  uintnat* p = caml_lookup_gc_tweak(String_val(name),
                                    caml_string_length(name));
  if (p == NULL)
    caml_invalid_argument("Gc.Tweak: parameter not found");
  CAMLreturn (Val_long((long)*p));
}

CAMLprim value caml_gc_tweak_set(value name, value v)
{
  CAMLparam2(name, v);
  uintnat* p = caml_lookup_gc_tweak(String_val(name),
                                    caml_string_length(name));
  if (p == NULL)
    caml_invalid_argument("Gc.Tweak: parameter not found");
  *p = (uintnat)Long_val(v);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_gc_tweak_list_active(value unit)
{
  CAMLparam1(unit);
  CAMLlocal3(list, name, pair);
  for (int i = N_GC_TWEAKS - 1; i >= 0; i--) {
    if (*gc_tweaks[i].ptr != gc_tweaks[i].initial_value) {
      name = caml_copy_string(gc_tweaks[i].name);
      pair = caml_alloc_2(0, name, Val_long((long)*gc_tweaks[i].ptr));
      list = caml_alloc_2(0, pair, list);
    }
  }
  CAMLreturn(list);
}

#define F_Z "%"ARCH_INTNAT_PRINTF_FORMAT"u"

/* Return the OCAMLRUNPARAMS form of any GC tweaks. Returns NULL if
 * none are set, or if we can't allocate. */

char *format_gc_tweaks(void)
{
  size_t len = 0;
  for (size_t i = 0; i < N_GC_TWEAKS; i++) {
    uintnat val = *gc_tweaks[i].ptr;
    if (val != gc_tweaks[i].initial_value) {
      len += (2 /* ',X' */
              + strlen(gc_tweaks[i].name)+1 /* 'tweak_name=' */);
      do { /* Count digits. We're not in any great hurry. */
        val /= 10;
        ++ len;
      } while(val);
    }
  }
  if (!len) { /* no gc_tweaks */
    return NULL;
  }
  ++ len; /* trailing NUL */
  char *buf = malloc(len);
  if (!buf) {
    goto fail_alloc;
  }
  char *p = buf;

  for (size_t i = 0; i < N_GC_TWEAKS; i++) {
    uintnat val = *gc_tweaks[i].ptr;
    if (val != gc_tweaks[i].initial_value) {
      int item_len = snprintf(p, len, ",X%s="F_Z,
                              gc_tweaks[i].name, val);
      if (item_len >= len) {
         /* surprise truncation: could be a race; just stop trying. */
        goto fail_truncate;
      }
      p += item_len;
      len -= item_len;
    }
  }
  return buf;

fail_truncate:
  free(buf);
fail_alloc:
  return NULL;
}

CAMLprim value caml_runtime_parameters (value unit)
{
  CAMLassert (unit == Val_unit);
  char *tweaks = format_gc_tweaks();
  char *no_tweaks = "";
  /* keep in sync with parse_ocamlrunparam */
  value res = caml_alloc_sprintf
    ("b=%d,c="F_Z",d="F_Z",e="F_Z",H="F_Z",i="F_Z",l="F_Z
     ",m="F_Z",M="F_Z",n="F_Z",o="F_Z",O="F_Z
     ",p="F_Z",s="F_Z",t="F_Z",v="F_Z",V="F_Z
     ",W="F_Z"%s",
       /* a was OCaml 4 allocation policy */
       /* b */ (int) Caml_state->backtrace_active,
       /* c */ caml_params->cleanup_on_exit,
       /* d */ caml_params->max_domains,
       /* e */ caml_params->runtime_events_log_wsize,
       /* h was OCaml 4 init heap size */
       /* H */ caml_params->use_hugetlb_pages,
       /* i */ caml_major_heap_increment,
       /* l */ atomic_load_relaxed(&caml_max_stack_wsize),
       /* m */ atomic_load_relaxed(&caml_custom_minor_ratio),
       /* M */ caml_custom_major_ratio,
       /* n */ caml_custom_minor_max_bsz,
       /* o */ caml_percent_free,
       /* O */ caml_max_percent_free,
       /* p */ caml_params->parser_trace,
       /* R */ /* missing: see stdlib/hashtbl.mli */
       /* s */ caml_minor_heap_max_wsz,
       /* t */ caml_params->trace_level,
       /* v */ caml_verb_gc,
       /* V */ caml_params->verify_heap,
       /* w was OCaml 4 major window */
       /* W */ caml_runtime_warnings,
       /* X */ tweaks ? tweaks : no_tweaks
       );
  if (tweaks) {
    free(tweaks);
  }
  return res;
}
/* Ramp-up phase. */

static uintnat get_ramp_up_suspended_words(void) {
  return (Caml_state->current_ramp_up_allocated_words_diff
          + Caml_state->allocated_words_suspended);
}

static void set_ramp_up_suspended_words(uintnat suspended_words) {
  Caml_state->current_ramp_up_allocated_words_diff =
    suspended_words - Caml_state->allocated_words_suspended;
}

caml_result caml_gc_ramp_up(value callback, uintnat *out_suspended_words) {
    /* Calls to [caml_gc_ramp_up] could be nested, so we are careful
       to save the current setting beforehand and restore it afterwards.

       When nesting an inner ramp-up phase within an outer ramp-up
       phase, the allocations suspended during the inner phase should
       be returned as the suspended count of the inner call, and
       should not be double-counted as suspended allocations of the
       outer phase. */

    CAML_GC_MESSAGE(POLICY, "Entering a GC ramp-up phase.\n");

    intnat ramp_up_already = (Caml_state->gc_policy & CAML_GC_RAMP_UP);
    if (!ramp_up_already)
      Caml_state->gc_policy = (Caml_state->gc_policy | CAML_GC_RAMP_UP);

    /* Save the suspended words of a potential outer phase,
       and start a new ramp_up phase. */
    uintnat suspended_words_outer = get_ramp_up_suspended_words();
    if (!ramp_up_already) CAMLassert(suspended_words_outer == 0);
    set_ramp_up_suspended_words(0);

    caml_result res = caml_callback_res(callback, Val_unit);

    /* Write the suspended words of the inner phase,
       restore the suspended words of the outer phase. */
    uintnat suspended_words_inner = get_ramp_up_suspended_words();
    *out_suspended_words = suspended_words_inner;
    set_ramp_up_suspended_words(suspended_words_outer);

    CAML_GC_MESSAGE(POLICY,
      "Leaving a GC ramp-up phase; "
      "suspended words: %"ARCH_INTNAT_PRINTF_FORMAT"u\n",
      suspended_words_inner);

    if (!ramp_up_already)
      Caml_state->gc_policy = (Caml_state->gc_policy & ~CAML_GC_RAMP_UP);

    return res;
}

void caml_gc_ramp_down(uintnat ramp_up_words) {
  Caml_state->allocated_words_resumed += ramp_up_words;
}
