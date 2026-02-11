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
<<<<<<< HEAD
#include <string.h>
||||||| 23e84b8c4d
=======
#include "caml/callback.h"
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

atomic_uintnat caml_max_stack_wsize;
uintnat caml_fiber_wsz;

<<<<<<< HEAD
extern uintnat caml_major_heap_increment; /* percent or words; see shared_heap.c */
extern uintnat caml_percent_free;         /*        see major_gc.c */
extern uintnat caml_max_percent_free;     /*        see major_gc.c */
extern uintnat caml_custom_major_ratio;   /* see custom.c */
extern uintnat caml_custom_minor_ratio;   /* see custom.c */
extern uintnat caml_custom_minor_max_bsz; /* see custom.c */
extern uintnat caml_minor_heap_max_wsz;   /* see domain.c */
extern uintnat caml_custom_work_max_multiplier; /* see major_gc.c */
extern uintnat caml_prelinking_in_use;    /* see startup_nat.c */
extern uintnat caml_compaction_algorithm; /* see shared_heap.c */
extern uintnat caml_compact_unmap;        /* see shared_heap.c */
extern uintnat caml_pool_min_chunk_bsz;  /* see shared_heap.c */
extern uintnat caml_percent_sweep_per_mark; /* see major_gc.c */
extern uintnat caml_gc_pacing_policy;       /* see major_gc.c */
extern uintnat caml_gc_overhead_adjustment; /* see major_gc.c */
extern uintnat caml_nohugepage_stacks;    /* see fiber.c */
extern uintnat caml_enable_segv_handler;  /* see signals.c / signals_nat.c */
||||||| 23e84b8c4d
extern uintnat caml_major_heap_increment; /* percent or words; see major_gc.c */
extern uintnat caml_percent_free;         /*        see major_gc.c */
extern uintnat caml_percent_max;          /*        see compact.c */
extern uintnat caml_allocation_policy;    /*        see freelist.c */
extern uintnat caml_custom_major_ratio;   /* see custom.c */
extern uintnat caml_custom_minor_ratio;   /* see custom.c */
extern uintnat caml_custom_minor_max_bsz; /* see custom.c */
extern uintnat caml_minor_heap_max_wsz;   /* see domain.c */
=======
extern _Atomic uintnat caml_percent_free; /* see major_gc.c */
extern _Atomic uintnat caml_custom_major_ratio; /* see custom.c */
extern _Atomic uintnat caml_custom_minor_ratio; /* see custom.c */
extern _Atomic uintnat caml_custom_minor_max_bsz; /* see custom.c */
extern uintnat caml_minor_heap_max_wsz; /* see domain.c */
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

CAMLprim value caml_gc_quick_stat(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  /* get a copy of these before allocating anything... */
  intnat majcoll, mincoll, compactions;
  struct gc_stats s;
  caml_compute_gc_stats(&s);
  majcoll = caml_major_cycles_completed;
  mincoll = atomic_load(&caml_minor_collections_count);
  compactions = atomic_load(&caml_compactions_count);

  res = caml_alloc_tuple (17);
  Store_field (res, 0, caml_copy_double ((double)s.alloc_stats.minor_words));
  Store_field (res, 1, caml_copy_double ((double)s.alloc_stats.promoted_words));
  Store_field (res, 2, caml_copy_double ((double)s.alloc_stats.major_words));
  Store_field (res, 3, Val_long (mincoll));
  Store_field (res, 4, Val_long (majcoll));
  Store_field (res, 5, Val_long (
    s.global_stats.chunk_words + s.heap_stats.large_words));
  Store_field (res, 6, Val_long (s.global_stats.chunks));
  Store_field (res, 7, Val_long (
    s.heap_stats.pool_live_words + s.heap_stats.large_words));
  Store_field (res, 8, Val_long (
    s.heap_stats.pool_live_blocks + s.heap_stats.large_blocks));
  Store_field (res, 9, Val_long (
    s.global_stats.chunk_words - s.heap_stats.pool_live_words
    - s.heap_stats.pool_frag_words));
  Store_field (res, 10, Val_long (0)); /* free_blocks */
  Store_field (res, 11, Val_long (0)); /* largest_free */
  Store_field (res, 12, Val_long (s.heap_stats.pool_frag_words));
  Store_field (res, 13, Val_long (compactions));
  Store_field (res, 14, Val_long (
    s.heap_stats.pool_max_words + s.heap_stats.large_max_words));
  Store_field (res, 15, Val_long (0)); /* stack_size */
  Store_field (res, 16, Val_long (s.alloc_stats.forced_major_collections));
  CAMLreturn (res);
}

double caml_gc_minor_words_unboxed (void)
{
  return (double)caml_minor_words_allocated();
}

CAMLprim value caml_gc_minor_words(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLreturn(caml_copy_double(caml_gc_minor_words_unboxed()));
}

CAMLprim value caml_gc_counters(value v)
{
<<<<<<< HEAD
  CAMLparam0 ();   /* v is ignored */
||||||| 23e84b8c4d
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);
=======
  CAMLparam0 (); /* v is ignored */
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  CAMLlocal4 (minwords_, prowords_, majwords_, res);

  /* get a copy of these before allocating anything... */
  double minwords = caml_gc_minor_words_unboxed();
  double prowords = (double)Caml_state->stat_promoted_words;
  double majwords = Caml_state->stat_major_words +
                    (double) Caml_state->allocated_words;

  minwords_ = caml_copy_double(minwords);
  prowords_ = caml_copy_double(prowords);
  majwords_ = caml_copy_double(majwords);
  res = caml_alloc_3(0, minwords_, prowords_, majwords_);
  CAMLreturn(res);
}

CAMLprim value caml_gc_get(value v)
{
  CAMLparam0 ();   /* v is ignored */
  CAMLlocal1 (res);

  res = caml_alloc_tuple (11);
  Store_field (res, 0, Val_long (Caml_state->minor_heap_wsz));          /* s */
<<<<<<< HEAD
  Store_field (res, 1, Val_long (caml_major_heap_increment));           /* i */
  Store_field (res, 2, Val_long (caml_percent_free));                   /* o */
||||||| 23e84b8c4d
  Store_field (res, 2, Val_long (caml_percent_free));                   /* o */
=======
  Store_field (res, 2,
    Val_long (atomic_load_relaxed(&caml_percent_free)));                /* o */
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  Store_field (res, 3, Val_long (atomic_load_relaxed(&caml_verb_gc)));  /* v */
  Store_field (res, 4, Val_long (caml_max_percent_free));
  Store_field (res, 5, Val_long (caml_max_stack_wsize));                /* l */
<<<<<<< HEAD
  Store_field (res, 6, Val_long (0));
  Store_field (res, 7, Val_long (0));
  Store_field (res, 8, Val_long (caml_custom_major_ratio));             /* M */
  Store_field (res, 9, Val_long (caml_custom_minor_ratio));             /* m */
  Store_field (res, 10, Val_long (caml_custom_minor_max_bsz));          /* n */
||||||| 23e84b8c4d
  Store_field (res, 8, Val_long (caml_custom_major_ratio));             /* M */
  Store_field (res, 9, Val_long (caml_custom_minor_ratio));             /* m */
  Store_field (res, 10, Val_long (caml_custom_minor_max_bsz));          /* n */
=======
  Store_field (res, 8,
    Val_long (atomic_load_relaxed(&caml_custom_major_ratio)));          /* M */
  Store_field (res, 9,
    Val_long (atomic_load_relaxed(&caml_custom_minor_ratio)));          /* m */
  Store_field (res, 10,
    Val_long (atomic_load_relaxed(&caml_custom_minor_max_bsz)));        /* n */
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  CAMLreturn (res);
}

#define Max(x,y) ((x) < (y) ? (y) : (x))

static uintnat norm_pfree (uintnat p)
{
  return Max (p, 1);
}

static uintnat norm_pmax (uintnat p)
{
  return p;
}

static uintnat norm_custom_maj (uintnat p)
{
  return Max (p, 1);
}

static uintnat norm_custom_min (uintnat p)
{
  return Max (p, 1);
}

CAMLprim value caml_gc_set(value v)
{
  uintnat newminwsz = caml_norm_minor_heap_size (Long_val (Field (v, 0)));
  uintnat newheapincr = Long_val (Field (v, 1));
  uintnat newpf = norm_pfree (Long_val (Field (v, 2)));
  uintnat new_verb_gc = Long_val (Field (v, 3));
  uintnat newpm = norm_pmax (Long_val (Field (v, 4)));
  uintnat new_max_stack_size = Long_val (Field (v, 5));
  /* ignore fields 6 (allocation policy) and 7 (window size) */
  uintnat new_custom_maj = norm_custom_maj (Long_val (Field (v, 8)));
  uintnat new_custom_min = norm_custom_min (Long_val (Field (v, 9)));
  uintnat new_custom_sz = Long_val (Field (v, 10));

  CAML_EV_BEGIN(EV_EXPLICIT_GC_SET);

  if (newheapincr != caml_major_heap_increment) {
    caml_major_heap_increment = newheapincr;
    if (newheapincr > 1000) {
      CAML_GC_MESSAGE(PARAMS, "New heap increment size: %"
                      ARCH_INTNAT_PRINTF_FORMAT "uk words\n",
                      caml_major_heap_increment/1024);
    } else {
      CAML_GC_MESSAGE(PARAMS, "New heap increment size: %"
                      ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                      caml_major_heap_increment);
    }
  }

  caml_change_max_stack_size (new_max_stack_size);

<<<<<<< HEAD
  if (newpf != caml_percent_free){
    caml_percent_free = newpf;
    CAML_GC_MESSAGE(PARAMS,
                    "New space overhead: %" ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                    caml_percent_free);
  }

  if (newpm != caml_max_percent_free) {
    caml_max_percent_free = newpm;
    CAML_GC_MESSAGE(PARAMS, "New max space overhead: %"
                    ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_max_percent_free);
||||||| 23e84b8c4d
  if (newpf != caml_percent_free){
    caml_percent_free = newpf;
    caml_gc_message (0x20, "New space overhead: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_free);
=======
  if (newpf != atomic_load_relaxed(&caml_percent_free)){
    atomic_store_relaxed(&caml_percent_free, newpf);
    CAML_GC_MESSAGE(PARAMS,
                    "New space overhead: %" ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                    caml_percent_free);
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  }

  atomic_store_relaxed(&caml_verb_gc, new_verb_gc);

  /* These fields were added in 4.08.0. */
  if (Wosize_val (v) >= 11){
<<<<<<< HEAD
    if (new_custom_maj != caml_custom_major_ratio){
      caml_custom_major_ratio = new_custom_maj;
||||||| 23e84b8c4d
    if (new_custom_maj != caml_custom_major_ratio){
      caml_custom_major_ratio = new_custom_maj;
      caml_gc_message (0x20, "New custom major ratio: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_major_ratio);
=======
    if (new_custom_maj != atomic_load_relaxed(&caml_custom_major_ratio)){
      atomic_store_relaxed(&caml_custom_major_ratio, new_custom_maj);
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
      CAML_GC_MESSAGE(PARAMS, "New custom major ratio: %"
                      ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                      caml_custom_major_ratio);
    }
<<<<<<< HEAD
    if (new_custom_min != caml_custom_minor_ratio){
      caml_custom_minor_ratio = new_custom_min;
||||||| 23e84b8c4d
    if (new_custom_min != caml_custom_minor_ratio){
      caml_custom_minor_ratio = new_custom_min;
      caml_gc_message (0x20, "New custom minor ratio: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_minor_ratio);
=======
    if (new_custom_min != atomic_load_relaxed(&caml_custom_minor_ratio)){
      atomic_store_relaxed(&caml_custom_minor_ratio, new_custom_min);
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
      CAML_GC_MESSAGE(PARAMS, "New custom minor ratio: %"
                      ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                      caml_custom_minor_ratio);
    }
<<<<<<< HEAD
    if (new_custom_sz != caml_custom_minor_max_bsz){
      caml_custom_minor_max_bsz = new_custom_sz;
||||||| 23e84b8c4d
    if (new_custom_sz != caml_custom_minor_max_bsz){
      caml_custom_minor_max_bsz = new_custom_sz;
      caml_gc_message (0x20, "New custom minor size limit: %"
                       ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                       caml_custom_minor_max_bsz);
=======
    if (new_custom_sz != atomic_load_relaxed(&caml_custom_minor_max_bsz)){
      atomic_store_relaxed(&caml_custom_minor_max_bsz, new_custom_sz);
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
      CAML_GC_MESSAGE(PARAMS, "New custom minor size limit: %"
                      ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                      caml_custom_minor_max_bsz);
    }
  }

  /* Minor heap size comes last because it will trigger a minor collection
     (thus invalidating [v]) and it can raise [Out_of_memory]. */
  if (newminwsz != Caml_state->minor_heap_wsz) {
    CAML_GC_MESSAGE(PARAMS, "New minor heap size: %"
                    ARCH_INTNAT_PRINTF_FORMAT "uk words\n", newminwsz / 1024);
  }

  if (newminwsz > caml_minor_heap_max_wsz) {
    CAML_GC_MESSAGE(PARAMS, "New minor heap max: %"
                    ARCH_INTNAT_PRINTF_FORMAT "uk words\n", newminwsz / 1024);
    caml_update_minor_heap_max(newminwsz);
  }
  CAMLassert(newminwsz <= caml_minor_heap_max_wsz);
  if (newminwsz != Caml_state->minor_heap_wsz) {
    /* FIXME: when (newminwsz > caml_minor_heap_max_wsz) and
       (newminwsz != Caml_state->minor_heap_wsz) are both true,
       the current domain reallocates its own minor heap twice. */
    caml_set_minor_heap_size (newminwsz);
  }

  CAML_EV_END(EV_EXPLICIT_GC_SET);
  return Val_unit;
}

CAMLprim value caml_gc_minor(value v)
{
  Caml_check_caml_state();
  CAML_EV_BEGIN(EV_EXPLICIT_GC_MINOR);
  CAMLassert (v == Val_unit);
  caml_minor_collection ();
  caml_result result = caml_process_pending_actions_res();
  CAML_EV_END(EV_EXPLICIT_GC_MINOR);
<<<<<<< HEAD
  return caml_raise_async_if_exception(exn, "");
||||||| 23e84b8c4d
  return caml_raise_if_exception(exn);
=======
  return caml_get_value_or_raise(result);
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
}

<<<<<<< HEAD
static value gc_major_exn(int compaction)
||||||| 23e84b8c4d
static value gc_major_exn(int force_compaction)
=======
static caml_result gc_major_res(int force_compaction)
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
{
  CAML_EV_BEGIN(EV_EXPLICIT_GC_MAJOR);
  CAML_GC_MESSAGE(MAJOR, "Major GC cycle requested\n");
  caml_empty_minor_heaps_once();
<<<<<<< HEAD
  caml_finish_major_cycle(compaction);
  caml_reset_major_pacing();
  value exn = caml_process_pending_actions_exn();
||||||| 23e84b8c4d
  caml_finish_major_cycle(force_compaction);
  value exn = caml_process_pending_actions_exn();
=======
  caml_finish_major_cycle(force_compaction);
  caml_reset_major_pacing();
  caml_result result = caml_process_pending_actions_res();
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  CAML_EV_END(EV_EXPLICIT_GC_MAJOR);
  return result;
}

CAMLprim value caml_gc_major(value v)
{
  Caml_check_caml_state();
  CAMLassert (v == Val_unit);
<<<<<<< HEAD
  return caml_raise_async_if_exception(
    gc_major_exn (Compaction_auto),
    "");
||||||| 23e84b8c4d
  return caml_raise_if_exception(gc_major_exn(0));
=======
  return caml_get_value_or_raise(gc_major_res(0));
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
}

static caml_result gc_full_major_res(void)
{
  CAML_EV_BEGIN(EV_EXPLICIT_GC_FULL_MAJOR);
  CAML_GC_MESSAGE(MAJOR, "Full Major GC requested\n");
  /* In general, it can require up to 3 GC cycles for a
     currently-unreachable object to be collected. */
<<<<<<< HEAD
  for (i = 0; i < 3; i++) {
    caml_finish_major_cycle(i == 2 ? Compaction_auto : Compaction_none);
    caml_reset_major_pacing();
    exn = caml_process_pending_actions_exn();
    if (Is_exception_result(exn)) break;
||||||| 23e84b8c4d
  for (i = 0; i < 3; i++) {
    caml_empty_minor_heaps_once();
    caml_finish_major_cycle(0);
    exn = caml_process_pending_actions_exn();
    if (Is_exception_result(exn)) break;
=======
  for (int i = 0; i < 3; i++) {
    caml_finish_major_cycle(0);
    caml_reset_major_pacing();
    caml_result res = caml_process_pending_actions_res();
    if (caml_result_is_exception(res)) return res;
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  }
  ++ Caml_state->stat_forced_major_collections;
  CAML_EV_END(EV_EXPLICIT_GC_FULL_MAJOR);
  return Result_unit;
}

CAMLprim value caml_gc_full_major(value v)
{
  Caml_check_caml_state();
  CAMLassert (v == Val_unit);
<<<<<<< HEAD
  return caml_raise_async_if_exception(gc_full_major_exn (), "");
||||||| 23e84b8c4d
  return caml_raise_if_exception(gc_full_major_exn());
=======
  return caml_get_value_or_raise(gc_full_major_res());
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
}

CAMLprim value caml_gc_major_slice (value v)
{
  CAML_EV_BEGIN(EV_EXPLICIT_GC_MAJOR_SLICE);
  CAMLassert (Is_long (v));
  caml_major_collection_slice(Long_val(v));
  caml_result result = caml_process_pending_actions_res();
  CAML_EV_END(EV_EXPLICIT_GC_MAJOR_SLICE);
<<<<<<< HEAD
  return caml_raise_async_if_exception(exn, "");
||||||| 23e84b8c4d
  return caml_raise_if_exception(exn);
=======
  return caml_get_value_or_raise(result);
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
}

CAMLprim value caml_gc_compaction(value v)
{
  Caml_check_caml_state();
  CAML_EV_BEGIN(EV_EXPLICIT_GC_COMPACT);
  CAMLassert (v == Val_unit);
<<<<<<< HEAD
  value exn = Val_unit;
  int i;
  /* We do a full major before this compaction. See [caml_full_major_exn] for
     why this needs three iterations. */
  for (i = 0; i < 3; i++) {
    caml_finish_major_cycle(i == 2 ? Compaction_forced : Compaction_none);
    caml_reset_major_pacing();
    exn = caml_process_pending_actions_exn();
    if (Is_exception_result(exn)) break;
||||||| 23e84b8c4d
  value exn = gc_major_exn(1);
=======
  caml_result result = Result_unit;
  /* We do a full major before this compaction. See [caml_full_major_res] for
     why this needs three iterations. */
  for (int i = 0; i < 3; i++) {
    caml_finish_major_cycle(i == 2);
    caml_reset_major_pacing();
    result = caml_process_pending_actions_res();
    if (caml_result_is_exception(result)) break;
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
  }
  ++ Caml_state->stat_forced_major_collections;
  CAML_EV_END(EV_EXPLICIT_GC_COMPACT);
<<<<<<< HEAD
  return caml_raise_async_if_exception(exn, "");
||||||| 23e84b8c4d
  return caml_raise_if_exception(exn);
=======
  return caml_get_value_or_raise(result);
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
}

CAMLprim value caml_gc_stat(value v)
{
  caml_result result;
  CAML_EV_BEGIN(EV_EXPLICIT_GC_STAT);
  result = gc_full_major_res();
  if (caml_result_is_exception(result)) goto out;
  result = Result_value(caml_gc_quick_stat(Val_unit));
 out:
  CAML_EV_END(EV_EXPLICIT_GC_STAT);
<<<<<<< HEAD
  return caml_raise_async_if_exception(res, "");
||||||| 23e84b8c4d
  return caml_raise_if_exception(res);
=======
  return caml_get_value_or_raise(result);
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
}

CAMLprim value caml_get_minor_free (value v)
{
  return Val_int
    ((uintnat)Caml_state->young_ptr - (uintnat)Caml_state->young_start);
}

void caml_init_gc (void)
{
  caml_minor_heap_max_wsz =
    caml_norm_minor_heap_size(caml_params->init_minor_heap_wsz);

  caml_max_stack_wsize = caml_params->init_max_stack_wsz;
<<<<<<< HEAD
  caml_fiber_wsz = caml_get_init_stack_wsize(STACK_SIZE_FIBER);
  caml_percent_free = norm_pfree (caml_params->init_percent_free);
  caml_max_percent_free = norm_pmax (caml_params->init_max_percent_free);
  CAML_GC_MESSAGE(STACKS, "Initial stack limit: %"
                  ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                  Bsize_wsize(caml_params->init_max_stack_wsz) / 1024);
||||||| 23e84b8c4d
  caml_fiber_wsz = (Stack_threshold * 2) / sizeof(value);
  caml_percent_free = norm_pfree (caml_params->init_percent_free);
  caml_gc_log ("Initial stack limit: %"
               ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
               caml_max_stack_wsize / 1024 * sizeof (value));
=======
  caml_fiber_wsz = (Stack_threshold * 2) / sizeof(value);
  atomic_store_relaxed(&caml_percent_free,
                       norm_pfree (caml_params->init_percent_free));
  caml_gc_log ("Initial stack limit: %"
               ARCH_INTNAT_PRINTF_FORMAT "uk bytes",
               caml_params->init_max_stack_wsz / 1024 * sizeof (value));
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

<<<<<<< HEAD
  caml_custom_major_ratio =
      norm_custom_maj (caml_params->init_custom_major_ratio);
  caml_custom_minor_ratio =
      norm_custom_min (caml_params->init_custom_minor_ratio);
  caml_custom_minor_max_bsz = caml_params->init_custom_minor_max_bsz;
  caml_major_heap_increment = caml_params->init_major_heap_increment;
||||||| 23e84b8c4d
  caml_custom_major_ratio =
      norm_custom_maj (caml_params->init_custom_major_ratio);
  caml_custom_minor_ratio =
      norm_custom_min (caml_params->init_custom_minor_ratio);
  caml_custom_minor_max_bsz = caml_params->init_custom_minor_max_bsz;
=======
  atomic_store_relaxed(&caml_custom_major_ratio,
                       norm_custom_maj (caml_params->init_custom_major_ratio));
  atomic_store_relaxed(&caml_custom_minor_ratio,
                       norm_custom_min (caml_params->init_custom_minor_ratio));
  atomic_store_relaxed(&caml_custom_minor_max_bsz,
                       caml_params->init_custom_minor_max_bsz);
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

  caml_gc_phase = Phase_sweep_and_mark_main;
  #ifdef NATIVE_CODE
  caml_init_frame_descriptors();
  #endif
  caml_init_domains(caml_params->max_domains,
                    caml_params->init_minor_heap_wsz);
  caml_init_gc_stats(caml_params->max_domains);
<<<<<<< HEAD
||||||| 23e84b8c4d
  caml_init_domains(caml_params->init_minor_heap_wsz);
/*
  caml_major_heap_increment = major_incr;
  caml_percent_free = norm_pfree (percent_fr);
  caml_percent_max = norm_pmax (percent_m);
  caml_init_major_heap (major_heap_size);
  caml_gc_message (0x20, "Initial minor heap size: %luk bytes\n",
                   Caml_state->minor_heap_size / 1024);
  caml_gc_message (0x20, "Initial major heap size: %luk bytes\n",
                   major_heap_size / 1024);
  caml_gc_message (0x20, "Initial space overhead: %"
                   ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_free);
  caml_gc_message (0x20, "Initial max overhead: %"
                   ARCH_INTNAT_PRINTF_FORMAT "u%%\n", caml_percent_max);
  if (caml_major_heap_increment > 1000){
    caml_gc_message (0x20, "Initial heap increment: %"
                     ARCH_INTNAT_PRINTF_FORMAT "uk words\n",
                     caml_major_heap_increment / 1024);
  }else{
    caml_gc_message (0x20, "Initial heap increment: %"
                     ARCH_INTNAT_PRINTF_FORMAT "u%%\n",
                     caml_major_heap_increment);
  }
  caml_gc_message (0x20, "Initial allocation policy: %d\n",
                   caml_allocation_policy);
*/
=======

>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
}

/* FIXME After the startup_aux.c unification, move these functions there. */

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

<<<<<<< HEAD
extern int caml_parser_trace;
||||||| 23e84b8c4d
extern int caml_parser_trace;

CAMLprim value caml_runtime_parameters (value unit)
{
#define F_Z ARCH_INTNAT_PRINTF_FORMAT
#define F_S ARCH_SIZET_PRINTF_FORMAT

  CAMLassert (unit == Val_unit);
  /* TODO KC */
  return caml_alloc_sprintf ("caml_runtime_parameters not implemented: %d", 0);
}

=======
CAMLprim value caml_runtime_parameters (value unit)
{
#define F_Z ARCH_INTNAT_PRINTF_FORMAT
#define F_S ARCH_SIZET_PRINTF_FORMAT

  CAMLassert (unit == Val_unit);
  return caml_alloc_sprintf
      ("b=%d,c=%"F_Z"u,e=%"F_Z"u,l=%"F_Z"u,M=%"F_Z"u,m=%"F_Z"u,n=%"F_Z"u,"
       "o=%"F_Z"u,p=%d,s=%"F_S"u,t=%"F_Z"u,v=%"F_Z"u,V=%"F_Z"u,W=%"F_Z"u",
       /* b */ (int) Caml_state->backtrace_active,
       /* c */ caml_params->cleanup_on_exit,
       /* e */ caml_params->runtime_events_log_wsize,
       /* l */ caml_max_stack_wsize,
       /* M */ caml_custom_major_ratio,
       /* m */ caml_custom_minor_ratio,
       /* n */ caml_custom_minor_max_bsz,
       /* o */ caml_percent_free,
       /* p */ Caml_state->parser_trace,
       /* R */ /* missing */
       /* s */ Caml_state->minor_heap_wsz,
       /* t */ caml_params->trace_level,
       /* v */ caml_verb_gc,
       /* V */ caml_params->verify_heap,
       /* W */ caml_runtime_warnings
       );
#undef F_Z
#undef F_S
}
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a

/* Control runtime warnings */

CAMLprim value caml_ml_enable_runtime_warnings(value vbool)
{
  caml_runtime_warnings = Bool_val(vbool);
  return Val_unit;
}

CAMLprim value caml_ml_runtime_warnings_enabled(value unit)
{
  CAMLassert (unit == Val_unit);
  return Val_bool(caml_runtime_warnings);
}

<<<<<<< HEAD
struct gc_tweak {
  const char* name;
  uintnat* ptr;
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
  { "gc_pacing_policy", &caml_gc_pacing_policy, 0 },
  { "gc_overhead_adjustment", &caml_gc_overhead_adjustment, 0 },
  { "nohugepage_stacks", &caml_nohugepage_stacks, 0 },
  { "enable_segv_handler", &caml_enable_segv_handler, 0 },
  { "cache_stacks_per_class", &caml_cache_stacks_per_class, 0 },
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
  /* keep in sync with runtime4 and with parse_ocamlrunparam */
  value res = caml_alloc_sprintf
    ("b=%d,c="F_Z",d="F_Z",e="F_Z",H="F_Z",i="F_Z",l="F_Z
     ",m="F_Z",M="F_Z",n="F_Z",o="F_Z",O="F_Z
     ",p="F_Z",s="F_Z",t="F_Z",v="F_Z",V="F_Z
     ",W="F_Z"%s",
       /* a is runtime 4 allocation policy */
       /* b */ (int) Caml_state->backtrace_active,
       /* c */ caml_params->cleanup_on_exit,
       /* d */ caml_params->max_domains,
       /* e */ caml_params->runtime_events_log_wsize,
       /* h is runtime 4 init heap size */
       /* H */ caml_params->use_hugetlb_pages,
       /* i */ caml_major_heap_increment,
       /* l */ caml_max_stack_wsize,
       /* m */ caml_custom_minor_ratio,
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
       /* w is runtime 4 major window */
       /* W */ caml_runtime_warnings,
       /* X */ tweaks ? tweaks : no_tweaks
       );
  if (tweaks) {
    free(tweaks);
  }
  return res;
||||||| 23e84b8c4d
=======

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

    CAML_GC_MESSAGE(SLICESIZE, "Entering a GC ramp-up phase.\n");

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

    CAML_GC_MESSAGE(SLICESIZE,
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

CAMLprim value caml_ml_gc_ramp_up(value callback) {
  CAMLparam1(callback);
  CAMLlocal1(v);
  uintnat deferred_words;
  caml_result res = caml_gc_ramp_up(callback, &deferred_words);
  if (caml_result_is_exception(res)) {
    // We will re-raise the exception below; before that,
    // we ramp_down to avoid discarding the deferred work.
    caml_gc_ramp_down(deferred_words);
  }
  v = caml_get_value_or_raise(res);
  CAMLreturn (caml_alloc_2(0, v, Val_long(deferred_words)));
}

CAMLprim value caml_ml_gc_ramp_down(value work) {
  uintnat resumed_words = Long_val(work);
  CAML_GC_MESSAGE(SLICESIZE,
    "GC ramp-down; resumed words: %"ARCH_INTNAT_PRINTF_FORMAT"u\n",
    resumed_words);
  caml_gc_ramp_down(resumed_words);
  return Val_unit;
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
}
