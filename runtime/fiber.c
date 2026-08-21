/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Tom Kelly, OCaml Labs Consultancy                    */
/*                Stephen Dolan, University of Cambridge                  */
/*                                                                        */
/*   Copyright 2021 Indian Institute of Technology, Madras                */
/*   Copyright 2021 OCaml Labs Consultancy                                */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include "caml/config.h"
#include <string.h>
#include <stdbool.h>
#include <stdio.h>
#ifdef HAS_UNISTD
#include <unistd.h>
#endif
#include <assert.h>
#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/codefrag.h"
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/dynamic.h"
#include "caml/gc_ctrl.h"
#include "caml/platform.h"
#include "caml/minor_gc.h"
#include "caml/misc.h"
#include "caml/major_gc.h"
#include "caml/memory.h"
#include "caml/obj.h"
#include "caml/runtime_events.h"
#include "caml/startup_aux.h"
#include "caml/shared_heap.h"
#ifdef NATIVE_CODE
#include "caml/stack.h"
#include "caml/frame_descriptors.h"
#endif
#if defined(USE_MMAP_MAP_STACK) || !defined(STACK_CHECKS_ENABLED)
#include <sys/mman.h>
#endif
#ifdef __linux__
/* for gettid */
#include <sys/types.h>
#include <sys/syscall.h>
#endif

#ifdef DEBUG
#define fiber_debug_log(...) caml_gc_log(__VA_ARGS__)
#else
#define fiber_debug_log(...)
#endif

static_assert(sizeof(struct stack_info) == Stack_ctx_words * sizeof(value), "");

static _Atomic int64_t fiber_id_global = 0;
static CAMLthread_local int64_t fiber_id_local = 0;

#define NUM_STACK_SIZE_CLASSES 5
#define MAX_STACK_CACHE_LIMIT  Max_domains_max

/* Parameters settable with OCAMLRUNPARAM */
uintnat caml_init_main_stack_wsz = 0;   /* -Xmain_stack_size= */
uintnat caml_init_thread_stack_wsz = 0; /* -Xthread_stack_size= */
uintnat caml_init_fiber_stack_wsz = 0;  /* -Xfiber_stack_size= */

uintnat caml_nohugepage_stacks = 1;

uintnat caml_cache_stacks_per_class = /* -Xcache_stacks_per_class */
#if defined(USE_MMAP_MAP_STACK) || defined(STACK_GUARD_PAGES)
  1
#else
  128
#endif
  ;

/* Soft upper bound on the number of guarded stacks in existence (see the
   design comment below); only meaningful with STACK_GUARD_PAGES. */
uintnat caml_max_guarded_stacks = 1024; /* -Xmax_guarded_stacks */

uintnat caml_get_init_stack_wsize (int context)
{
  uintnat init_stack_wsize = 0;

#ifdef STACK_GUARD_PAGES
  switch(context) {
  case STACK_SIZE_MAIN:   init_stack_wsize = caml_init_main_stack_wsz; break;
  case STACK_SIZE_THREAD: init_stack_wsize = caml_init_thread_stack_wsz; break;
  case STACK_SIZE_FIBER:  init_stack_wsize = caml_init_fiber_stack_wsz; break;
  default: caml_fatal_error("caml_get_init_stack_wsize: invalid context");
  }
#else
  switch(context) {
  case STACK_SIZE_MAIN:
  case STACK_SIZE_THREAD: init_stack_wsize = Wsize_bsize(Stack_init_bsize); break;
  case STACK_SIZE_FIBER:  init_stack_wsize = Wsize_bsize(Stack_threshold * 2); break;
  default: caml_fatal_error("caml_get_init_stack_wsize: invalid context");
  }
#endif

  uintnat stack_wsize = 0;
  if (init_stack_wsize < caml_max_stack_wsize)
    stack_wsize = init_stack_wsize;
  else
    stack_wsize = caml_max_stack_wsize;

  /* If we are requesting a large stack (more than a hugepage), then
     we'd like the total allocation size to be a multiple of the huge
     page size. However, the stack guard pages, headers, etc. have
     some overhead, so we want the requested stack size to be a bit
     less than a multiple of the hugepage size */
  if (caml_plat_hugepagesize > 0
      && stack_wsize > Wsize_bsize(caml_plat_hugepagesize)) {
    /* round down to multiple of hugepage size */
    stack_wsize &= ~(Wsize_bsize(caml_plat_hugepagesize) - 1);
    /* 3 pages is enough to cover the overhead */
    stack_wsize -= 3 * Wsize_bsize(caml_plat_pagesize);
  }

  return stack_wsize;
}

void caml_change_max_stack_size (uintnat new_max_wsize)
{
  struct stack_info *current_stack = Caml_state->current_stack;
  asize_t wsize = Stack_high(current_stack) - (value*)current_stack->sp
                 + Stack_threshold / sizeof (value);

  if (new_max_wsize < wsize) new_max_wsize = wsize;
  if (new_max_wsize != caml_max_stack_wsize){
    CAML_GC_MESSAGE(STACKS,
                    "Changing stack limit to %"
                    ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                    Bsize_wsize(new_max_wsize) / 1024);
  }
  caml_max_stack_wsize = new_max_wsize;
}

/* Round up to a power of 2 */
static uintnat round_up_p2(uintnat x, uintnat p2)
{
  CAMLassert (Is_power_of_2(p2));
  return (x + p2 - 1) & ~(p2 - 1);
}

/* Allocate a stack with at least the specified number of words.
   The [handler] field of the result is initialised (so Stack_high(...)) is
   well-defined), but other fields are uninitialised */
Caml_inline struct stack_info* alloc_for_stack (mlsize_t wosize, int64_t id)
{
  /* Ensure 16-byte alignment of the [struct stack_handler*]. */
  const int stack_alignment = 16;

  /* Ensure we have room to offset Stack_high. */
  wosize += Stack_padding_word;

#ifdef USE_MMAP_MAP_STACK
  size_t len = sizeof(struct stack_info) +
               sizeof(value) * wosize +
               8 + /* For 16-byte aligning handler */
               sizeof(struct stack_handler);
  struct stack_info* si;
  si = mmap(NULL, len, PROT_WRITE | PROT_READ,
             MAP_ANONYMOUS | MAP_PRIVATE | MAP_STACK, -1, 0);
  if (si == MAP_FAILED)
    return NULL;

  si->size = len;

  si->handler =
    (struct stack_handler*)
    round_up_p2((uintnat)si + sizeof(struct stack_info)
      + sizeof(value) * wosize, stack_alignment);

  return si;
#elif defined(STACK_GUARD_PAGES)
  /* (We use the following strategy only in native code, because bytecode
   * has its own way of dealing with stack checks.)
   *
   * We want to detect a stack overflow by triggering a segfault when a
   * given part of the memory is accessed; in order to do so, we protect
   * a page near the end of the stack to make it unreadable/unwritable.
   * A signal handler for segfault will be installed, that will check if
   * the invalid address is in the range we protect, and will raise a stack
   * overflow exception accordingly.
   */
  size_t page_size = caml_plat_pagesize;
  size_t len = Bsize_wsize(wosize);
  uintnat trailer_size = round_up_p2(sizeof(struct stack_handler),
    stack_alignment);
  len += trailer_size;

  // We need two more pages for stack_info and guard
  CAMLassert(sizeof(struct stack_info) <= page_size);
  len += 2 * page_size;
  len = caml_mem_round_up_mapping_size(len);

  // Stack layout (higher addresses are at the top):
  //
  // --------------------
  // struct stack_handler
  // -------------------- <- [stack->handler], 16-aligned
  // pad word (amd64-no-fp)
  // -------------------- <- Stack_high
  // the stack itself
  // -------------------- <- page-aligned
  // guard page
  // -------------------- <- page-aligned
  // padding to one page
  // struct stack_info
  // -------------------- <- [stack], page/hugepage-aligned (by caml_mem_map)
  struct stack_info* stack;
  /* These mappings should never use HugeTLB pages, due to the guard page */
  stack = caml_mem_map(len, CAML_MAP_NO_HUGETLB, NULL);
  if (stack == NULL) {
    return NULL;
  }
#ifdef __linux__
  /* On Linux, (optionally) disable *any* hugepage usage for stacks.
     (Huge pages are not as beneficial for stacks, because you use the same few
     kb over and over again, but can have a significant RAM cost) */
  if (caml_nohugepage_stacks) madvise(stack, len, MADV_NOHUGEPAGE);
#endif
  // mmap is always expected to return a page-aligned value.
  CAMLassert((uintnat)stack % page_size == 0);

  if (mprotect(Protected_stack_page(stack), page_size, PROT_NONE)) {
    caml_mem_unmap(stack, len);
    return NULL;
  }

#ifdef DEBUG /* Avoid unnecessary syscalls in release builds */
#ifdef __linux__
  /* On Linux, give names to the various mappings */
  caml_mem_name_map(stack, page_size,
                    "stack info (original fiber id %ld, tid %ld)",
                    id, (long)syscall(SYS_gettid));

  caml_mem_name_map(Protected_stack_page(stack), page_size,
                    "guard page for stack (original fiber id %ld, tid %ld)",
                    id, (long)syscall(SYS_gettid));

  caml_mem_name_map(Stack_base(stack), len - 2*page_size,
                    "stack (original fiber id %ld, tid %ld)",
                    id, (long)syscall(SYS_gettid));
#endif /* __linux__ */
#endif /* DEBUG */

  // Assert that the guard page does not impinge on the actual stack area.
  CAMLassert((char*) stack + len - (trailer_size + Bsize_wsize(wosize))
    >= Protected_stack_page(stack) + page_size);

  stack->size = len;
  stack->handler = (struct stack_handler*)((char*)stack + len - trailer_size);
  CAMLassert(((uintnat) stack->handler) % stack_alignment == 0);

  return stack;
#else
  size_t len = sizeof(struct stack_info)+
               sizeof(value) * wosize +
               8 + /* For 16-byte aligning handler */
               sizeof(struct stack_handler);
  struct stack_info* stack = caml_stat_alloc_noexc(len);
  if (stack == NULL) return NULL;
  stack->size = len;
  stack->handler =
    (struct stack_handler*)
    round_up_p2((uintnat)stack + sizeof(struct stack_info) +
      sizeof(value) * wosize, stack_alignment);
  return stack;
#endif /* USE_MMAP_MAP_STACK, STACK_GUARD_PAGES */
}

/* Returns the index into the [Caml_state->stack_cache] array if this size is
 * pooled. If unpooled, it is [-1].
 *
 * Stacks may be unpooled if either the stack size is not 2**N multiple of
 * [caml_fiber_wsz] or the stack is bigger than pooled sizes. */
Caml_inline int stack_cache_bucket (mlsize_t wosize) {
  mlsize_t size_bucket_wsz = caml_fiber_wsz;
  int bucket=0;

  while (bucket < NUM_STACK_SIZE_CLASSES) {
    if (wosize == size_bucket_wsz)
      return bucket;
    ++bucket;
    size_bucket_wsz += size_bucket_wsz;
  }
  return -1;
}

#ifdef STACK_GUARD_PAGES

/**** Guarded and idle stacks (STACK_GUARD_PAGES only) ****

   Without stack checks, every stack in use is "guarded": a fixed-size
   mapping with a guard page, which is expensive to create and consumes
   kernel mappings. To bound the number of mappings, only stacks that can
   run need to be guarded:

   - Running stacks (the current stack of some thread and its ancestors)
     and the stacks of continuations on the minor heap are guarded.
   - When a continuation is promoted to the major heap, its stacks become
     "idle": their data is copied into malloced buffers and their guarded
     stacks are recycled; [idled_from] records each vanished guarded
     stack's [Stack_high]. Stack-internal absolute pointers (the exception
     chain and, with frame pointers, the frame-pointer chain) are left
     unrelocated: nothing follows them while the continuation is
     suspended.
   - When such a continuation is resumed, [caml_continuation_use_noexc]
     wakes each stack: copies it onto a fresh guarded stack and relocates
     those chains by the distance the data has moved.

   Guarded stacks are cached on three levels. Freed ones go to the
   freeing domain's cache ([Caml_state->stack_caches], one lock-free
   list per size class, pushed by any domain, popped by the owner). At
   the start of each minor collection every domain demotes its local
   caches to the global cache: anything still there has gone a whole
   minor cycle unused. During compaction the global cache's mappings
   are returned to the OS. [caml_max_guarded_stacks] is a soft bound
   on the total number of guarded stacks: allocating beyond it
   requests a minor collection (but still succeeds), and cached ones
   beyond it are parked on an "extra" cache whose use also requests a
   minor collection. The global and extra caches are shared by all
   domains, so they are multiple-consumer: lock-free popping would
   suffer from ABA, so they are protected by [stack_cache_global_lock]
   instead (they are off the fast path).

   This relies on two invariants. A captured stack chain has no live C
   frames -- callbacks mask the stack's handlers (see callback.c), so
   effects cannot cross the C boundary -- and therefore no [c_stack_link]
   points into it, it contains no C-entry chunk (so no asynchronous-
   exception trap frame, and [Caml_state->async_exn_handler] never
   points into it), and nothing re-enters it except resumption. */

/* This code has only ever run on amd64: configure forces stack checks
   on everywhere else. Ports are possible. ARM64 would require:
   - a reload of [Cont_last_fiber] after the call to
     [caml_continuation_use_noexc] in arm64.S's DO_RESUME_SWITCH, as in
     amd64.S (waking moves the stacks, invalidating the earlier load);
   - re-deriving the suspended-context layout at [Stack_sp] assumed by
     the frame-pointer chain walk in [stack_wake] (arm64's
     SWITCH_OCAML_STACKS saves the x29/x30 pair unconditionally);
   - checking the handler-relative offset of the oldest saved frame
     pointer in [continuation_wake_stacks] against arm64.S's
     UPDATE_BASE_POINTER (the two agree today, but by parallel
     construction, not by sharing). */
#if !defined(TARGET_amd64)
#error "Guarded-stack idling only on AMD64: see comment here."
#endif

/* The global and extra caches; both protected by [stack_cache_global_lock].
   [len] fields are only accessed under the lock. */
static struct stack_cache stack_cache_global[NUM_STACK_SIZE_CLASSES];
static struct stack_cache stack_cache_extra[NUM_STACK_SIZE_CLASSES];
static caml_plat_mutex stack_cache_global_lock = CAML_PLAT_MUTEX_INITIALIZER;

/* Monotonic counters, reported via CAML_GC_MESSAGE (STACKS category). */
static atomic_uintnat stacks_demoted = 0;
static atomic_uintnat stacks_idled = 0;
static atomic_uintnat stacks_idled_bytes = 0;
static atomic_uintnat stacks_woken = 0;
static atomic_uintnat stacks_woken_bytes = 0;
static atomic_uintnat stack_cache_hits_local = 0;
static atomic_uintnat stack_cache_hits_global = 0;
static atomic_uintnat stack_cache_hits_extra = 0;

/* A cached stack's [sp] is dead so we use it to record the major
   cycle at which the stack was demoted, so they can be freed when
   they get old. */
#define Stack_cache_stamp(stk) (*(uintnat*)&(stk)->sp)

/* Ask for a minor collection at the next safe point, to reduce the
   population of guarded stacks. */
static void minor_gc_request(void)
{
  Caml_state->requested_minor_gc = 1;
  caml_interrupt_self();
}

/* Pop a stack of size class [bucket] from cache [c], or return NULL.
   Must be called with [stack_cache_global_lock] held. */
static struct stack_info* cache_pop_with_lock(struct stack_cache* c)
{
  struct stack_info* stk = atomic_load_relaxed(&c->head);
  if (stk == NULL) return NULL;
  atomic_store_relaxed(&c->head, Stack_cache_next(stk));
  atomic_store_relaxed(&c->len, atomic_load_relaxed(&c->len) - 1);
  return stk;
}

/* Push [stk] onto cache [c].
   Must be called with [stack_cache_global_lock] held. */
static void cache_push_with_lock(struct stack_cache* c,
                                 struct stack_info* stk)
{
  Stack_cache_next(stk) = atomic_load_relaxed(&c->head);
  atomic_store_relaxed(&c->head, stk);
  atomic_store_relaxed(&c->len, atomic_load_relaxed(&c->len) + 1);
}

#endif /* STACK_GUARD_PAGES */

/* Stack counters, reported at process exit */
static atomic_uintnat stack_count = 0;
static atomic_uintnat stack_count_peak = 0;
static atomic_uintnat stacks_created = 0;
static atomic_uintnat stacks_freed = 0;

/* Allocate a stack that the local cache could not supply: from the global
   caches when possible, otherwise a fresh one. Returns NULL on allocation
   failure. The result's [cache_bucket] is set. */
static struct stack_info*
stack_alloc_uncached(mlsize_t wosize, int cache_bucket, int64_t id)
{
  struct stack_info* stack;
#ifdef STACK_GUARD_PAGES
  if (cache_bucket != -1) {
    caml_plat_lock_blocking(&stack_cache_global_lock);
    stack = cache_pop_with_lock(&stack_cache_global[cache_bucket]);
    if (stack != NULL) {
      (void)caml_atomic_counter_incr(&stack_cache_hits_global);
    } else {
      stack = cache_pop_with_lock(&stack_cache_extra[cache_bucket]);
      if (stack != NULL) {
        /* Dipping into the over-bound reserve. */
        (void)caml_atomic_counter_incr(&stack_cache_hits_extra);
        minor_gc_request();
      }
    }
    caml_plat_unlock(&stack_cache_global_lock);
    if (stack != NULL) return stack;
  }
  if (atomic_load_relaxed(&stack_count) >= caml_max_guarded_stacks) {
    /* The bound is soft: GC at the next safe point, but do allocate. */
    minor_gc_request();
  }
#endif
  stack = alloc_for_stack(wosize, id);
  if (stack == NULL) return NULL;
  (void)caml_atomic_counter_incr(&stacks_created);
  uintnat count = caml_atomic_counter_incr(&stack_count);
  uintnat peak = atomic_load_relaxed(&stack_count_peak);
  while (count > peak &&
         !atomic_compare_exchange_weak(&stack_count_peak, &peak, count)) {
  }
  CAML_EV_COUNTER(EV_C_STACK_CREATED, stack->size);
  stack->cache_bucket = cache_bucket;
  return stack;
}

static struct stack_info*
alloc_size_class_stack_noexc(mlsize_t wosize, int cache_bucket, value hval,
                             value hexn, value heff, value htick, int64_t id)
{
  struct stack_info* stack;
  struct stack_cache* caches = Caml_state->stack_caches;

  static_assert(sizeof(struct stack_info) % sizeof(value) == 0, "");
  static_assert(sizeof(struct stack_handler) % sizeof(value) == 0, "");

  CAMLassert(caches != NULL);

  if (cache_bucket != -1) {
    struct stack_cache* cache = &caches[cache_bucket];
    bool alloc = false;
    do {
      stack = cache->head;
      if(stack) {
        // Other domains may push to the cache, but not pop, so it's safe
        // to read the cache link.
        struct stack_info* top = Stack_cache_next(stack);
        alloc = atomic_compare_exchange_weak(&cache->head, &stack, top);
        if(alloc) {
          cache->len -= 1;
#ifdef STACK_GUARD_PAGES
          (void)caml_atomic_counter_incr(&stack_cache_hits_local);
#endif
        }
      } else {
        stack = stack_alloc_uncached(wosize, cache_bucket, id);
        if(stack == NULL) {
          return NULL;
        }
        alloc = true;
      }
    } while(!alloc);

    CAMLassert(stack->cache_bucket == stack_cache_bucket(wosize));
  } else {
    /* couldn't get a cached stack, so have to create one */
    stack = stack_alloc_uncached(wosize, cache_bucket, id);
    if (stack == NULL) {
      return NULL;
    }
  }

  struct stack_handler* hand = stack->handler;
  hand->handle_value = hval;
  hand->handle_exn = hexn;
  hand->handle_effect = heff;
  hand->handle_tick = htick;
  hand->parent = NULL;
  stack->sp = Stack_high(stack);
  stack->exception_ptr = NULL;
  stack->idled_from = NULL;
  stack->id = id;
  stack->domain_idx = Caml_state->id;
  stack->local_arenas = NULL;
  stack->local_sp = 0;
  stack->local_top = NULL;
  stack->local_limit = 0;
  caml_dynamic_table_init(&stack->dyn);
#ifdef DEBUG
  stack->magic = 42;
#endif
  /* Due to stack alignment performed above, the actual stack size may be
   * larger than requested. */
  CAMLassert(Stack_high(stack) - Stack_base(stack) >= wosize);
  return stack;

}

/* allocate a stack with at least "wosize" usable words of stack */
struct stack_info*
caml_alloc_stack_noexc(mlsize_t wosize, value hval, value hexn, value heff, int64_t id)
{
  int cache_bucket = stack_cache_bucket (wosize);
  return alloc_size_class_stack_noexc(wosize, cache_bucket, hval, hexn, heff,
                                      /*htick=*/Val_null, id);
}

static int64_t new_fiber_id(void)
{
  enum { Fiber_id_chunk = 1024 };
  if (fiber_id_local % Fiber_id_chunk == 0)
    fiber_id_local = atomic_fetch_add(&fiber_id_global, Fiber_id_chunk);
  return fiber_id_local++;
}

#ifdef NATIVE_CODE

value caml_alloc_stack (value hval, value hexn, value heff) {
  const int64_t id = new_fiber_id();
  struct stack_info *stack =
      alloc_size_class_stack_noexc(caml_fiber_wsz, 0 /* first bucket */, hval,
                                   hexn, heff, /*htick=*/Val_null, id);

  if (!stack)
#if defined(USE_MMAP_MAP_STACK) || defined(STACK_GUARD_PAGES)
    caml_raise_out_of_fibers();
#else
    caml_raise_out_of_memory();
#endif

  fiber_debug_log ("Allocate stack=%p of %" ARCH_INTNAT_PRINTF_FORMAT
                     "u words", stack, caml_fiber_wsz);

  return Val_ptr(stack);
}

value caml_alloc_stack_preemptible(value hval, value hexn, value heff,
                                        value htick) {
  const int64_t id = new_fiber_id();
  struct stack_info* stack =
    alloc_size_class_stack_noexc(caml_fiber_wsz, 0 /* first bucket */,
                                 hval, hexn, heff, htick, id);

  if (!stack)
#if defined(USE_MMAP_MAP_STACK) || defined(STACK_GUARD_PAGES)
    caml_raise_out_of_fibers();
#else
    caml_raise_out_of_memory();
#endif

  fiber_debug_log ("Allocate stack=%p of %" ARCH_INTNAT_PRINTF_FORMAT
                     "u words", stack, caml_fiber_wsz);

  return Val_ptr(stack);
}

void caml_get_stack_sp_pc (struct stack_info* stack,
                           char** sp /* out */, uintnat* pc /* out */)
{
  char* p = (char*)stack->sp;
  p = First_frame(p);
  *pc = Saved_return_address(p); /* ret addr */
  *sp = p;                       /* pointer to first frame */
}


/* Returns the arena number of a block,
   or -1 if it is not in any local arena */
static int get_local_ix(caml_local_arenas* loc, value v)
{
  int i;
  CAMLassert(Is_block(v));
  /* Search local arenas, starting from the largest (last) */
  for (i = 0; i < loc->count; i++) {
    struct caml_local_arena arena = loc->arenas[i];
    if (arena.base <= (char*)v && (char*)v < arena.base + arena.length)
      return i;
  }
  return -1;
}


/* If it visits an unmarked local block,
      returns the index of the containing arena
   Otherwise returns -1.
   Temporarily marks local blocks with colors.GARBAGE
    (which is not otherwise the color of reachable blocks) */
static int visit(scanning_action f, void* fdata,
                 struct caml_local_arenas* locals,
                 struct global_heap_state colors,
                 value* p)
{
  value v = *p, vblock = v;
  header_t hd;
  int ix;
  if (!Is_block(v))
    return -1;

  if (Is_young(v)) {
    f(fdata, v, p);
    return -1;
  }

  /* major or local or external */

  hd = Hd_val(vblock);
  if (Tag_hd(hd) == Infix_tag) {
    vblock -= Infix_offset_val(v);
    hd = Hd_val(vblock);
  }

  if (Color_hd(hd) == colors.GARBAGE) {
    /* Local, marked */
    return -1;
  } else if (Color_hd(hd) == NOT_MARKABLE) {
    /* Local (unmarked) or external */

    if (locals == NULL)
      /* external */
      return -1;

    ix = get_local_ix(locals, vblock);

    if (ix != -1) {
      /* Mark this unmarked local */
      *Hp_val(vblock) = With_status_hd(hd, colors.GARBAGE);
    }

    return ix;
  } else {
    /* Major heap */
    f(fdata, v, p);
    return -1;
  }
}

static void scan_local_allocations(scanning_action f, void* fdata,
                                   caml_local_arenas* loc, uintnat local_sp)
{
  int arena_ix;
  intnat sp;
  struct caml_local_arena arena;
  /* does not change during scanning */
  struct global_heap_state colors = caml_global_heap_state;

  if (loc == NULL) return;
  CAMLassert(loc->count > 0);
  sp = local_sp;
  arena_ix = loc->count - 1;
  arena = loc->arenas[arena_ix];
#ifdef DEBUG
  { header_t* hp;
    for (hp = (header_t*)arena.base;
         hp < (header_t*)(arena.base + arena.length + sp);
         hp++) {
      *hp = Debug_free_local;
    }
  }
#endif

  while (sp < 0) {
    header_t* hp = (header_t*)(arena.base + arena.length + sp), hd = *hp;
    intnat i;

    if (hd == Local_uninit_hd) {
      CAMLassert(arena_ix > 0);
      arena = loc->arenas[--arena_ix];
#ifdef DEBUG
      for (hp = (header_t*)arena.base;
           hp < (header_t*)(arena.base + arena.length + sp);
           hp++) {
        *hp = Debug_free_local;
      }
#endif
      continue;
    }
    CAMLassert(Color_hd(hd) == NOT_MARKABLE ||
               Color_hd(hd) == colors.GARBAGE);
    if (Color_hd(hd) == NOT_MARKABLE) {
      /* Local allocation, not marked */
#ifdef DEBUG
      /* We don't check the reserved bits here because this is OK even for mixed
         blocks. */
      for (i = 0; i < Wosize_hd(hd); i++)
        Field(Val_hp(hp), i) = Debug_free_local;
#endif
      sp += Bhsize_hd(hd);
      continue;
    }
    /* reset mark */
    hd = With_status_hd(hd, NOT_MARKABLE);
    *hp = hd;
    CAMLassert(Tag_hd(hd) != Infix_tag);  /* start of object, no infix */
    CAMLassert(Tag_hd(hd) != Cont_tag);   /* no local continuations */
    if (!Scannable_hd(hd)) {
      sp += Bhsize_hd(hd);
      continue;
    }
    i = 0;
    if (Tag_hd(hd) == Closure_tag)
      i = Start_env_closinfo(Closinfo_val(Val_hp(hp)));

    mlsize_t scannable_wosize = Scannable_wosize_hd(hd);

    for (; i < scannable_wosize; i++) {
      value *p = Op_val(Val_hp(hp)) + i;
      int marked_ix = visit(f, fdata, loc, colors, p);
      if (marked_ix != -1) {
        struct caml_local_arena a = loc->arenas[marked_ix];
        intnat newsp = (char*)*p - (a.base + a.length);
        if (sp <= newsp) {
          /* forwards pointer, common case */
          CAMLassert(marked_ix <= arena_ix);
        } else {
          /* If backwards pointers are ever supported (e.g. local recursive
             values), then this should reset sp and iterate to a fixpoint */
          CAMLassert(marked_ix >= arena_ix);
          caml_fatal_error("backwards local pointer");
        }
      }
    }
    sp += Bhsize_hd(hd);
  }
}


Caml_inline void scan_stack_frames(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* gc_regs,
  struct caml_local_arenas* locals)
{
  char * sp;
  uintnat retaddr;
  value * regs;
  frame_descr * d;
  value *root;
  caml_frame_descrs *fds = caml_get_frame_descrs();
  /* does not change during marking */
  struct global_heap_state colors = caml_global_heap_state;

  sp = (char*)stack->sp;
  regs = gc_regs;

next_chunk:
  if (sp == (char*)Stack_high(stack)) return;
  sp = First_frame(sp);
  retaddr = Saved_return_address(sp);

  while(1) {
    d = caml_find_frame_descr(fds, retaddr);
    CAMLassert(d);
    if (!frame_return_to_C(d)) {
      /* Scan the roots in this frame */
      if (frame_is_short(d)) {
        /* Short descriptor: live registers come from the hot-register
         * bitmap, live stack slots from the frame's slot bitmap. */
        struct frame_descr_decoded dec;
        caml_decode_frame_descr(d, &dec);
        if (dec.has_allocs) {
          unsigned char bitmap = dec.short_reg_bitmap;
          for (int i = 0; bitmap; i++, bitmap >>= 1) {
            if (bitmap & 1) {
              root = regs + caml_frame_hot_regs[i];
              visit (f, fdata, locals, colors, root);
            }
          }
        }
        /* Live stack slots: a bitmap of the frame. */
        for (uint32_t byte = 0; byte < dec.short_live_bytes; byte++) {
          unsigned char bits = dec.short_live[byte];
          for (int i = 0; bits != 0; i++, bits >>= 1) {
            if (bits & 1) {
              root = (value *)(sp + ((uintnat)byte * 8 + i) * sizeof(value));
              visit (f, fdata, locals, colors, root);
            }
          }
        }
      } else if (frame_is_long(d)) {
        const unsigned char *p = d + Frame_long_live_ofs;
        uint32_t n = caml_read_unaligned_uint32(d + Frame_long_num_live_ofs);
        for (; n > 0; n--, p += sizeof(uint32_t)) {
          uint32_t ofs = caml_read_unaligned_uint32(p);
          if (ofs & 1) {
            root = regs + (ofs >> 1);
          } else {
            root = (value *)(sp + ofs);
          }
          visit (f, fdata, locals, colors, root);
        }
      } else {
        const unsigned char *p = d + Frame_live_ofs;
        uint16_t n = caml_read_unaligned_uint16(d + Frame_num_live_ofs);
        for (; n > 0; n--, p += sizeof(uint16_t)) {
          uint16_t ofs = caml_read_unaligned_uint16(p);
          if (ofs & 1) {
            root = regs + (ofs >> 1);
          } else {
            root = (value *)(sp + ofs);
          }
          visit (f, fdata, locals, colors, root);
        }
      }
      /* Move to next frame */
      sp += frame_size(d);
      retaddr = Saved_return_address(sp);
      /* XXX KC: disabled already scanned optimization. */
    } else {
      /* This marks the top of an ML stack chunk. Move sp to the previous
       * stack chunk.  */
      regs = Saved_gc_regs(sp); /* update gc_regs */
      sp += Stack_header_size;  /* skip trap frame, gc_regs, DWARF pointer */
      goto next_chunk;
    }
  }
}

void caml_scan_stack(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* gc_regs)
{
  while (stack != NULL) {
    caml_local_arenas* locals = caml_refresh_locals(stack);

    scan_stack_frames(f, fflags, fdata, stack, gc_regs, locals);

    /* Scan dynamic bindings */
    caml_dynamic_table_scan_roots(&stack->dyn, f, fflags, fdata);

    f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
    f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
    f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));
    f(fdata, Stack_handle_tick(stack), &Stack_handle_tick(stack));

    scan_local_allocations(f, fdata, locals, stack->local_sp);

    stack = Stack_parent(stack);
  }
}

void caml_ensure_gc_regs(void)
{
  CAMLnoalloc;
  if (Caml_state->gc_regs_buckets == NULL) {
    /* Ensure there is at least one gc_regs bucket available before
       running any OCaml code. See fiber.h for documentation. */
    value* bucket = caml_stat_alloc(sizeof(value) * Wosize_gc_regs);
    bucket[0] = 0; /* no next bucket */
    Caml_state->gc_regs_buckets = bucket;
  }
}

void caml_maybe_expand_stack (void)
{
  struct stack_info* stk = Caml_state->current_stack;
  uintnat stack_available =
    (value*)stk->sp - Stack_base(stk);
  uintnat stack_needed =
    Stack_threshold / sizeof(value)
    /* for words pushed by caml_start_program */
    + 8 + Stack_padding_word;

  if (stack_available < stack_needed) {
    if (!caml_try_realloc_stack (stack_needed)) {
      caml_raise_stack_overflow();
    }
  }

  caml_ensure_gc_regs();
}

#else /* End NATIVE_CODE, begin BYTE_CODE */

value caml_global_data = Val_unit;

CAMLprim value caml_alloc_stack(value hval, value hexn, value heff)
{
  value* sp;
  const int64_t id = new_fiber_id();
  struct stack_info *stack =
      alloc_size_class_stack_noexc(caml_fiber_wsz, 0 /* first bucket */, hval,
                                   hexn, heff, /*htick=*/Val_null, id);

  if (!stack)
#if defined(USE_MMAP_MAP_STACK) || defined(STACK_GUARD_PAGES)
    caml_raise_out_of_fibers();
#else
    caml_raise_out_of_memory();
#endif

  sp = Stack_high(stack);
  sp -= 1;
  sp[0] = Val_long(1);

  stack->sp = sp;

  return Val_ptr(stack);
}

CAMLprim value caml_alloc_stack_preemptible(value hval, value hexn,
                                            value heff, value htick)
{
  value* sp;
  const int64_t id = new_fiber_id();
  struct stack_info* stack =
    alloc_size_class_stack_noexc(caml_fiber_wsz, 0 /* first bucket */,
                                 hval, hexn, heff, htick, id);

  if (!stack)
#if defined(USE_MMAP_MAP_STACK) || defined(STACK_GUARD_PAGES)
    caml_raise_out_of_fibers();
#else
    caml_raise_out_of_memory();
#endif

  sp = Stack_high(stack);
  sp -= 1;
  sp[0] = Val_long(1);

  stack->sp = sp;

  return Val_ptr(stack);
}

CAMLprim value caml_ensure_stack_capacity(value required_space)
{
  asize_t req = Long_val(required_space);
  if (Caml_state->current_stack->sp - req <
      Stack_base(Caml_state->current_stack))
    if (!caml_try_realloc_stack(req))
      caml_raise_stack_overflow();
  return Val_unit;
}

/*
  Root scanning.

  Used by the GC to find roots on the stacks of running or runnable fibers.
*/

/* Code pointers are stored on the bytecode stack as naked pointers.
   We must avoid passing them to the scanning action,
   unless we know that it is a no-op outside young values
   (so it will safely ignore code pointers). */
 Caml_inline int is_scannable(scanning_action_flags flags, value v) {
  return
      (flags & SCANNING_ONLY_YOUNG_VALUES)
      || (Is_block(v) && caml_find_code_fragment_by_pc((char *) v) == NULL);
}

void caml_scan_stack(
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct stack_info* stack, value* v_gc_regs)
{
  value *low, *high;

  while (stack != NULL) {
    CAMLassert(stack->magic == 42);

    high = Stack_high(stack);
    low = stack->sp;
    for (value *sp = low; sp < high; sp++) {
      value v = *sp;
      if (is_scannable(fflags, v)) {
        f(fdata, v, sp);
      }
    }

    /* Scan dynamic bindings */
    caml_dynamic_table_scan_roots(&stack->dyn, f, fflags, fdata);

    if (is_scannable(fflags, Stack_handle_value(stack)))
      f(fdata, Stack_handle_value(stack), &Stack_handle_value(stack));
    if (is_scannable(fflags, Stack_handle_exception(stack)))
      f(fdata, Stack_handle_exception(stack), &Stack_handle_exception(stack));
    if (is_scannable(fflags, Stack_handle_effect(stack)))
      f(fdata, Stack_handle_effect(stack), &Stack_handle_effect(stack));
    if (is_scannable(fflags, Stack_handle_tick(stack)))
      f(fdata, Stack_handle_tick(stack), &Stack_handle_tick(stack));

    stack = Stack_parent(stack);
  }
}

#endif /* end BYTE_CODE */

CAMLexport void caml_do_local_roots (
  scanning_action f, scanning_action_flags fflags, void* fdata,
  struct caml__roots_block *local_roots,
  struct stack_info *current_stack,
  value * v_gc_regs,
  dynamic_cache_t dynamic_bindings,
  struct c_stack_link* c_stack)
{
#ifdef NATIVE_CODE
  caml_local_arenas* locals = caml_refresh_locals(current_stack);
#endif

  caml_dynamic_cache_scan_roots(dynamic_bindings, f, fflags, fdata);
  for (struct caml__roots_block *lr = local_roots; lr != NULL; lr = lr->next) {
#ifdef NATIVE_CODE
    /* c_stack marks the boundary between C stack segments. Distinct C stack
       segments may have distinct ML fiber stacks, so when we change stack
       segment we need to find the appropriate local arenas. */
    while (c_stack != NULL && (uintnat)c_stack < (uintnat)lr) {
      c_stack = c_stack->prev;
      if (c_stack != NULL) locals = caml_refresh_locals(c_stack->stack);
    }
#endif
    for (int i = 0; i < lr->ntables; i++){
      for (int j = 0; j < lr->nitems; j++){
        value *sp = &(lr->tables[i][j]);
        if (*sp != 0) {
#ifdef NATIVE_CODE
          visit (f, fdata, locals, caml_global_heap_state, sp);
#else
          f (fdata, *sp, sp);
#endif
        }
      }
    }
  }
  caml_scan_stack(f, fflags, fdata, current_stack, v_gc_regs);
#ifndef NATIVE_CODE
  CAMLassert(current_stack->local_arenas == NULL);
#endif
}


/*
  Stack management.

  Used by the interpreter to allocate stack space.
*/

#ifdef NATIVE_CODE
/* Update absolute exception pointers for new stack*/
void caml_rewrite_exception_stack(struct stack_info *old_stack,
                                  value** exn_ptr, value** async_exn_ptr,
                                  struct stack_info *new_stack)
{
  fiber_debug_log("Old [%p, %p]", Stack_base(old_stack), Stack_high(old_stack));
  fiber_debug_log("New [%p, %p]", Stack_base(new_stack), Stack_high(new_stack));
  if(exn_ptr) {
    CAMLassert(async_exn_ptr != NULL);

    fiber_debug_log ("*exn_ptr=%p", *exn_ptr);
    fiber_debug_log ("*async_exn_ptr=%p", *async_exn_ptr);

    while (Stack_base(old_stack) < *exn_ptr &&
           *exn_ptr <= Stack_high(old_stack)) {
      int must_update_async_exn_ptr = *exn_ptr == *async_exn_ptr;
#ifdef DEBUG
      value* old_val = *exn_ptr;
#endif
      *exn_ptr = Stack_high(new_stack) - (Stack_high(old_stack) - *exn_ptr);

      if (must_update_async_exn_ptr) *async_exn_ptr = *exn_ptr;
      fiber_debug_log ("must_update_async_exn_ptr=%d",
        must_update_async_exn_ptr);

      fiber_debug_log ("Rewriting %p to %p", old_val, *exn_ptr);

      CAMLassert(Stack_base(new_stack) < *exn_ptr);
      CAMLassert((value*)*exn_ptr <= Stack_high(new_stack));

      exn_ptr = (value**)*exn_ptr;
    }
    fiber_debug_log ("finished with *exn_ptr=%p", *exn_ptr);
  } else {
    fiber_debug_log ("exn_ptr is null");
    CAMLassert(async_exn_ptr == NULL);
  }
}
#endif

int caml_try_realloc_stack(asize_t required_space)
{
#if defined(USE_MMAP_MAP_STACK) || defined(STACK_GUARD_PAGES)
  (void) required_space;
  return 0;
#else
  struct stack_info *old_stack, *new_stack;
  asize_t wsize;
  int stack_used;
  CAMLnoalloc;

  old_stack = Caml_state->current_stack;
  stack_used = Stack_high(old_stack) - (value*)old_stack->sp;
  wsize = Stack_high(old_stack) - Stack_base(old_stack);
  uintnat max_stack_wsize = caml_max_stack_wsize;
  wsize = wsize & (~1); // zero alignment bit
  do {
    if (wsize >= max_stack_wsize) return 0;
    wsize *= 2;
  } while (wsize < stack_used + required_space);

  if (wsize > 4096 / sizeof(value)) {
    CAML_GC_MESSAGE(STACKS,
                    "Growing stack to %"
                    ARCH_INTNAT_PRINTF_FORMAT "uk bytes\n",
                    Bsize_wsize(wsize) / 1024);
  } else {
    CAML_GC_MESSAGE(STACKS,
                    "Growing stack to %"
                    ARCH_INTNAT_PRINTF_FORMAT "u bytes\n",
                    Bsize_wsize(wsize) * sizeof(value));
  }

  new_stack = alloc_size_class_stack_noexc(wsize, stack_cache_bucket(wsize),
                                           Stack_handle_value(old_stack),
                                           Stack_handle_exception(old_stack),
                                           Stack_handle_effect(old_stack),
                                           Stack_handle_tick(old_stack),
                                           old_stack->id);

  if (!new_stack) return 0;
  memcpy(Stack_high(new_stack) - stack_used,
         Stack_high(old_stack) - stack_used,
         stack_used * sizeof(value));
  new_stack->sp = Stack_high(new_stack) - stack_used;
  Stack_parent(new_stack) = Stack_parent(old_stack);

  new_stack->local_arenas = caml_refresh_locals(old_stack);
  new_stack->local_sp = old_stack->local_sp;
  new_stack->local_top = old_stack->local_top;
  new_stack->local_limit = old_stack->local_limit;
  new_stack->dyn = old_stack->dyn;

  // Detach locals stack and dynamic bindings from old_stack so they will not be freed
  old_stack->local_arenas = NULL;
  old_stack->local_sp = 0;
  old_stack->local_top = NULL;
  old_stack->local_limit = 0;
  caml_dynamic_table_init(&old_stack->dyn);

#ifdef NATIVE_CODE
  /* There's no need to do another pass rewriting from
     Caml_state->async_exn_handler because every asynchronous exception trap
     frame is also a normal exception trap frame.  However
     Caml_state->async_exn_handler itself must be updated. */
  caml_rewrite_exception_stack(old_stack, (value**)&Caml_state->exn_handler,
                               (value**) &Caml_state->async_exn_handler,
                               new_stack);
#endif

  /* Update stack pointers in Caml_state->c_stack. It is possible to have
   * multiple c_stack_links to point to the same stack since callbacks are run
   * on existing stacks. */
  {
    for (struct c_stack_link *link = Caml_state->c_stack;
         link != NULL;
         link = link->prev) {
      if (link->stack == old_stack) {
        ptrdiff_t delta =
          (char*)Stack_high(new_stack) - (char*)Stack_high(old_stack);
#ifdef WITH_FRAME_POINTERS
        struct stack_frame {
          struct stack_frame* prev;
          void* retaddr;
        };

        /* Frame pointer is pushed just below the c_stack_link.
           This is somewhat tricky to guarantee when there are stack
           arguments to C calls: see caml_c_call_copy_stack_args */
        struct stack_frame* fp = ((struct stack_frame*)link) - 1;
        CAMLassert(fp->prev == link->sp);

        /* Rewrite OCaml frame pointers above this C frame */
        while (Stack_base(old_stack) <= (value*)fp->prev &&
               (value*)fp->prev < Stack_high(old_stack)) {
          fp->prev = (struct stack_frame*)((char*)fp->prev + delta);
          fp = fp->prev;
        }
#endif
        link->stack = new_stack;
        link->sp = (char*)link->sp + delta;
      }
      if (link->async_exn_handler >= (char*) Stack_base(old_stack)
          && link->async_exn_handler < (char*) Stack_high(old_stack)) {
        /* The asynchronous exception trap frame pointed to by the current
           c_stack_link lies on the OCaml stack being reallocated.  Repoint the
           trap frame to the new stack. */
        fiber_debug_log("Rewriting link->async_exn_handler %p...",
          link->async_exn_handler);
        link->async_exn_handler +=
          (char*) Stack_high(new_stack) - (char*) Stack_high(old_stack);
        fiber_debug_log("...to %p", link->async_exn_handler);
      } else {
        fiber_debug_log("Not touching link->async_exn_handler %p",
          link->async_exn_handler);
      }
    }
  }

  caml_free_stack(old_stack);
  Caml_state->current_stack = new_stack;
  return 1;
#endif
}

struct stack_info* caml_alloc_main_stack (uintnat init_wsize)
{
  const int64_t id = new_fiber_id();
  struct stack_info* stk =
    caml_alloc_stack_noexc(init_wsize, Val_unit, Val_unit, Val_unit, id);
  return stk;
}

static void free_stack_memory(struct stack_info* stack)
{
  (void)caml_atomic_counter_incr(&stacks_freed);
  (void)caml_atomic_counter_decr(&stack_count);
  CAML_EV_COUNTER(EV_C_STACK_FREED, stack->size);
#if defined(DEBUG) && defined(STACK_CHECKS_ENABLED)
  memset(stack, 0x42, (char*)stack->handler - (char*)stack);
#endif
#if defined(USE_MMAP_MAP_STACK)
  munmap(stack, stack->size);
#elif defined(STACK_GUARD_PAGES)
  caml_mem_unmap(stack, stack->size);
#else
  caml_stat_free(stack);
#endif
}

struct stack_cache* caml_alloc_stack_caches(void)
{
  int i;

  struct stack_cache* stack_caches =
    (struct stack_cache*)
    caml_stat_alloc_noexc(sizeof(struct stack_cache) * NUM_STACK_SIZE_CLASSES);

  if (stack_caches == NULL)
    return NULL;

  for(i = 0; i < NUM_STACK_SIZE_CLASSES; i++) {
    stack_caches[i].head = NULL;
    stack_caches[i].len = MAX_STACK_CACHE_LIMIT;
  }

  return stack_caches;
}

void caml_free_stack_caches(struct stack_cache* caches)
{
  for (int i = 0; i < NUM_STACK_SIZE_CLASSES; i++) {
    while (caches[i].head != NULL) {
      struct stack_info* stk = caches[i].head;
      caches[i].head = Stack_cache_next(stk);
      free_stack_memory(stk);
    }
  }
  caml_stat_free(caches);
}

#ifndef STACK_GUARD_PAGES
// Must not be greater than MAX_STACK_CACHE_LIMIT
static uintnat caml_stack_cache_limit(int domain_idx)
{
  // It's common to use the initial domain to distribute fibers to a pool of
  // worker domains, so we let it cache at least one fiber per running domain.
  if(domain_idx == 0) {
    uintnat n = atomic_load_relaxed(&caml_num_domains_running);
    return n < caml_cache_stacks_per_class ? caml_cache_stacks_per_class : n;
  }
  return caml_cache_stacks_per_class;
}
#endif /* !STACK_GUARD_PAGES */

void caml_enable_stack_caches(struct stack_cache* caches)
{
  CAMLassert(caches != NULL);

  for(int i = 0; i < NUM_STACK_SIZE_CLASSES; i++) {
    struct stack_cache* cache = &caches[i];

    CAMLassert(cache->head == NULL);
    CAMLassert(cache->len >= MAX_STACK_CACHE_LIMIT);

    // Allow other domains to start pushing stacks
    cache->len -= MAX_STACK_CACHE_LIMIT;
  }
}

void caml_disable_stack_caches(struct stack_cache* caches)
{
  CAMLassert(caches != NULL);

  for(int i = 0; i < NUM_STACK_SIZE_CLASSES; i++) {
    struct stack_cache* cache = &caches[i];

    // Stop other domains from pushing stacks
    cache->len += MAX_STACK_CACHE_LIMIT;

    while (cache->len > MAX_STACK_CACHE_LIMIT) {
      struct stack_info* top = cache->head;

      // len includes domains with pending pushes, so we may reach the end of
      // the cache. If so, spin until the pushes resolve.
      if(top == NULL) {
        cpu_relax();
        continue;
      }

      struct stack_info* next = Stack_cache_next(top);
      if(atomic_compare_exchange_weak(&cache->head, &top, next)) {
        cache->len -= 1;
        free_stack_memory(top);
      }
    }

    CAMLassert(cache->head == NULL);
    CAMLassert(cache->len >= MAX_STACK_CACHE_LIMIT);
  }
}

/* Return the mapping (or other stack memory) of [stack] to its owner's
   local cache, or free it. The fiber state held on the stack (local
   allocations, dynamic bindings) must already have been freed or moved. */
static void stack_release_memory(struct stack_info* stack)
{
  // If this fiber was allocated by a domain at index [domain_idx], the stack
  // cache at that index has been initialized and will never be freed.
  struct stack_cache* caches = caml_get_stack_caches(stack->domain_idx);
  int cache_bucket = stack->cache_bucket;

  CAMLassert(stack->magic == 42);
  CAMLassert(caches != NULL);

  if (cache_bucket != -1) {
#if defined(DEBUG) && defined(STACK_CHECKS_ENABLED)
    memset(Stack_base(stack), 0x42,
          (Stack_high(stack)-Stack_base(stack))*sizeof(value));
#endif
    struct stack_cache* cache = &caches[cache_bucket];
#ifdef STACK_GUARD_PAGES
    /* Local caches are flushed at each minor collection and the mapped
       population is bounded by [caml_max_guarded_stacks], so no per-class
       limit applies (short of the enable/disable protocol ceiling). */
    uintnat limit = MAX_STACK_CACHE_LIMIT;
#else
    uintnat limit = caml_stack_cache_limit(stack->domain_idx);
#endif
    uintnat len = atomic_fetch_add(&cache->len, 1);
    if (len >= limit) {
      // The cache may have fewer than [len] stacks, but we know other domains
      // have committed to pushing stacks up to [limit].
      cache->len -= 1;
      free_stack_memory(stack);
    } else {
      bool freed = false;
      do {
        struct stack_info* top = cache->head;
        Stack_cache_next(stack) = top;
        freed = atomic_compare_exchange_weak(&cache->head, &top, stack);
      } while(!freed);
    }
  } else {
    free_stack_memory(stack);
  }
}

void caml_free_stack (struct stack_info* stack)
{
  CAMLnoalloc;

#ifdef STACK_GUARD_PAGES
  CAMLassert(stack->idled_from == NULL);
#endif

  // Don't need to update local_sp since this is no longer the current stack.
  caml_free_local_arenas(stack->local_arenas);

  caml_dynamic_table_free(&stack->dyn);

  stack_release_memory(stack);
}

void caml_free_gc_regs_buckets(value *gc_regs_buckets)
{
  while (gc_regs_buckets != NULL) {
    value *next = (value*)gc_regs_buckets[0];
    caml_stat_free(gc_regs_buckets);
    gc_regs_buckets = next;
  }
}

#ifdef STACK_GUARD_PAGES

/**** Idling and waking stacks ****/

/* Idle guarded stack [stack]: copy its data into a malloced buffer and
   return the idle copy, which takes over the fiber state (local
   allocations, dynamic bindings). The exception and frame-pointer chains
   are left pointing into the old guarded stack, to be relocated from
   [idled_from] when the stack is woken. Cannot raise: aborts on
   allocation failure, as the caller (the minor GC) cannot recover. */
static struct stack_info* stack_idle(struct stack_info* stack)
{
  /* The data from [sp] to the handler (including any padding word) and
     the handler itself are contiguous: copy them together. */
  size_t data_bytes = (char*)stack->handler - (char*)stack->sp;
  /* Lay the copy out with the handler 16-aligned, as on a mapped stack. */
  size_t handler_off = round_up_p2(sizeof(struct stack_info) + data_bytes,
                                   16);

  CAMLassert(stack->idled_from == NULL);
  CAMLassert(stack->magic == 42);

  struct stack_info* idle =
    caml_stat_alloc_noexc(handler_off + sizeof(struct stack_handler));
  if (idle == NULL)
    caml_fatal_error("Fatal error: out of memory idling a fiber stack");

  *idle = *stack;
  idle->sp = (char*)idle + handler_off - data_bytes;
  idle->handler = (struct stack_handler*)((char*)idle + handler_off);
  idle->idled_from = Stack_high(stack);
  memcpy(idle->sp, stack->sp, data_bytes + sizeof(struct stack_handler));

  (void)caml_atomic_counter_incr(&stacks_idled);
  atomic_fetch_add(&stacks_idled_bytes,
                   data_bytes + sizeof(struct stack_handler));
  CAML_EV_COUNTER(EV_C_STACK_IDLED,
                  data_bytes + sizeof(struct stack_handler));
  return idle;
}

/* Where the data of a woken stack moved: from the guarded stack spanning
   [old_low, old_high) to one [delta] bytes away. */
struct stack_reloc {
  char* old_low;
  char* old_high;
  ptrdiff_t delta;
};

/* The inverse of [stack_idle]: copy idle stack [idle] onto a guarded
   stack, relocate its stack-internal pointers, and free the idle copy.
   Fills [*reloc] with the data's movement, for the relocation of any
   cross-stack links into this stack. Cannot raise: aborts if no guarded
   stack can be obtained, since the callers sit on the stack-switching
   path where raising is impossible. */
static struct stack_info* stack_wake(struct stack_info* idle,
                                     struct stack_reloc* reloc)
{
  int bucket = idle->cache_bucket;

  CAMLassert(idle->idled_from != NULL);
  CAMLassert(idle->magic == 42);
  /* Only fibers are ever captured in continuations, and fibers are
     always of the first (smallest) pooled size class. */
  CAMLassert(bucket >= 0 && bucket < NUM_STACK_SIZE_CLASSES);

  struct stack_info* stack =
    alloc_size_class_stack_noexc(caml_fiber_wsz << bucket, bucket, Val_unit,
                                 Val_unit, Val_unit, Val_null, idle->id);
  if (stack == NULL)
    caml_fatal_error("Fatal error: out of memory waking a fiber stack");

  size_t data_bytes = (char*)idle->handler - (char*)idle->sp;
  char* new_sp = (char*)stack->handler - data_bytes;
  CAMLassert(new_sp >= (char*)Stack_base(stack));
  memcpy(new_sp, idle->sp, data_bytes + sizeof(struct stack_handler));
  stack->sp = new_sp;
  stack->exception_ptr = idle->exception_ptr; /* relocated below */
  stack->local_arenas = idle->local_arenas;
  stack->local_sp = idle->local_sp;
  stack->local_top = idle->local_top;
  stack->local_limit = idle->local_limit;
  stack->dyn = idle->dyn;

  /* Relocate the exception chain and (under WITH_FRAME_POINTERS) the
     frame-pointer chain by the distance the stack data has moved since
     it was idled. Both chains climb the stack, reaching non-stack
     memory (NULL, or the eventual resumer's stack) at their final link. */
  char* old_high = (char*)idle->idled_from;
  char* old_low = old_high - ((char*)Stack_high(idle) - (char*)idle->sp);
  ptrdiff_t delta = (char*)Stack_high(stack) - old_high;
  reloc->old_low = old_low;
  reloc->old_high = old_high;
  reloc->delta = delta;

  /* Relocate chain of exception handler pointers */
  char** p = (char**)&stack->exception_ptr;
  while (old_low <= *p && *p < old_high) {
    *p = *p + delta;
    p = (char**)*p;
  }
#ifdef WITH_FRAME_POINTERS
  /* Relocate chain of frame pointers, starting at [sp] with the frame
     pointer pushed when the stack was suspended (by
     SWITCH_OCAML_STACKS in amd64.S). The chain's last link is
     rewritten by UPDATE_BASE_POINTER when the stack is resumed. */
  p = (char**)stack->sp;
  while (old_low <= *p && *p < old_high) {
    *p = *p + delta;
    p = (char**)*p;
  }
#endif

  (void)caml_atomic_counter_incr(&stacks_woken);
  atomic_fetch_add(&stacks_woken_bytes,
                   data_bytes + sizeof(struct stack_handler));
  CAML_EV_COUNTER(EV_C_STACK_WOKEN,
                  data_bytes + sizeof(struct stack_handler));
  caml_stat_free(idle);
  return stack;
}

struct stack_info* caml_cont_idle_stacks(value cont)
{
  struct stack_info* stack = Ptr_val(Field(cont, 0));
  struct stack_info* head = NULL;
  struct stack_info* prev = NULL;

  CAMLassert(Is_block(cont) && Tag_val(cont) == Cont_tag);
  CAMLassert(stack != NULL);

  while (stack != NULL) {
    struct stack_info* next = Stack_parent(stack);
    struct stack_info* idle = stack_idle(stack);
    stack_release_memory(stack);
    if (prev == NULL)
      head = idle;
    else
      Stack_parent(prev) = idle;
    prev = idle;
    stack = next;
  }
  /* Fields 0 and 1 hold integer-tagged pointers: plain stores are fine. */
  Field(cont, 0) = Val_ptr(head);
  Field(cont, 1) = Val_ptr(prev);
  return head;
}

/* Wake the whole stack chain of [cont], whose (idle) head is [idle];
   the chain has just been taken from the continuation, so we own it.
   Updates the last-fiber field of [cont] and returns the new head. */
static value continuation_wake_stacks(value cont, struct stack_info* idle)
{
  struct stack_info* head = NULL;
  struct stack_info* prev = NULL;
  CAMLnoalloc;

  while (idle != NULL) {
    struct stack_info* next = Stack_parent(idle);
    struct stack_reloc reloc;
    struct stack_info* stack = stack_wake(idle, &reloc);
    if (prev == NULL) {
      head = stack;
    } else {
      Stack_parent(prev) = stack;
#ifdef WITH_FRAME_POINTERS
      /* The oldest frame pointer saved on [prev] (pushed by the first
         function run on it, under caml_runstack) points into this, its
         parent, stack: relocate it too. The corresponding link of the
         chain's last fiber is instead overwritten by UPDATE_BASE_POINTER
         when the chain is resumed. The offset is as in
         UPDATE_BASE_POINTER (amd64.S). */
      char** slot = (char**)((char*)prev->handler - 48);
      if (reloc.old_low <= *slot && *slot < reloc.old_high)
        *slot = *slot + reloc.delta;
#endif
    }
    prev = stack;
    idle = next;
  }
  Field(cont, 1) = Val_ptr(prev);
  return Val_ptr(head);
}

void caml_stack_cache_flush_local(void)
{
  struct stack_cache* caches = Caml_state->stack_caches;
  uintnat cached = 0;
  uintnat demoted = 0;

  /* Total the global cache before deciding what goes to the extra cache. */
  caml_plat_lock_blocking(&stack_cache_global_lock);
  for (int i = 0; i < NUM_STACK_SIZE_CLASSES; i++) {
    cached += atomic_load_relaxed(&stack_cache_global[i].len);
  }
  for (int i = 0; i < NUM_STACK_SIZE_CLASSES; i++) {
    /* We are in a stop-the-world section, so no mutator is pushing to
       this domain's local caches; they can be emptied wholesale. */
    struct stack_info* stk = atomic_exchange(&caches[i].head, NULL);
    uintnat n = 0;
    while (stk != NULL) {
      struct stack_info* next = Stack_cache_next(stk);
#ifdef MADV_FREE
      /* A demoted stack has gone a whole minor cycle unused: let the OS
         reclaim its dirty pages (everything above the guard page; the
         stack_info page holds the cache links). Best effort: on failure
         the pages just stay resident. */
      char* data = (char*)Stack_base(stk);
      (void)madvise(data, ((char*)stk + stk->size) - data, MADV_FREE);
#endif
      Stack_cache_stamp(stk) = caml_major_cycles_completed;
      if (cached < caml_max_guarded_stacks) {
        cache_push_with_lock(&stack_cache_global[i], stk);
        ++ cached;
      } else {
        cache_push_with_lock(&stack_cache_extra[i], stk);
      }
      ++ n;
      stk = next;
    }
    atomic_fetch_sub(&caches[i].len, n);
    demoted += n;
  }
  caml_plat_unlock(&stack_cache_global_lock);
  atomic_fetch_add(&stacks_demoted, demoted);
  if (demoted > 0) {
    CAML_GC_MESSAGE(STACKS,
                    "Demoted %" ARCH_INTNAT_PRINTF_FORMAT "u guarded "
                    "stacks to the global cache.\n", demoted);
  }
}

/* Free cached stacks that are unlikely to be needed: everything on
   the extra cache, and any global-cache stack that has sat there for
   a whole major cycle. Called by one domain at the end of each major
   cycle. */
void caml_stack_cache_trim(void)
{
  uintnat freed = 0;
  /* [caml_major_cycles_completed] has already been advanced for the
     cycle now ending, so a stack demoted during that cycle carries a
     stamp one less than [current], and older stamps have gone a full
     cycle unused. */
  uintnat current = caml_major_cycles_completed;

  caml_plat_lock_blocking(&stack_cache_global_lock);
  for (int i = 0; i < NUM_STACK_SIZE_CLASSES; i++) {
    struct stack_info* stk = atomic_exchange(&stack_cache_extra[i].head, NULL);
    while (stk != NULL) {
      struct stack_info* next = Stack_cache_next(stk);
      atomic_store_relaxed(&stack_cache_extra[i].len,
                           atomic_load_relaxed(&stack_cache_extra[i].len) - 1);
      free_stack_memory(stk);
      ++ freed;
      stk = next;
    }
    struct stack_info* keep = NULL;
    stk = atomic_exchange(&stack_cache_global[i].head, NULL);
    while (stk != NULL) {
      struct stack_info* next = Stack_cache_next(stk);
      if (Stack_cache_stamp(stk) + 1 < current) {
        atomic_store_relaxed(
          &stack_cache_global[i].len,
          atomic_load_relaxed(&stack_cache_global[i].len) - 1);
        free_stack_memory(stk);
        ++ freed;
      } else {
        Stack_cache_next(stk) = keep;
        keep = stk;
      }
      stk = next;
    }
    atomic_store_relaxed(&stack_cache_global[i].head, keep);
  }
  caml_plat_unlock(&stack_cache_global_lock);
  if (freed > 0) {
    CAML_GC_MESSAGE(STACKS,
                    "Freed %" ARCH_INTNAT_PRINTF_FORMAT "u cached stacks "
                    "unused for a major cycle.\n", freed);
  }
}

void caml_stack_cache_free_unused(void)
{
  uintnat freed = 0;

  caml_plat_lock_blocking(&stack_cache_global_lock);
  for (int i = 0; i < NUM_STACK_SIZE_CLASSES; i++) {
    struct stack_cache* cs[2] =
      { &stack_cache_global[i], &stack_cache_extra[i] };
    for (int j = 0; j < 2; j++) {
      struct stack_info* stk = atomic_exchange(&cs[j]->head, NULL);
      while (stk != NULL) {
        struct stack_info* next = Stack_cache_next(stk);
        atomic_store_relaxed(&cs[j]->len,
                             atomic_load_relaxed(&cs[j]->len) - 1);
        free_stack_memory(stk);
        ++ freed;
        stk = next;
      }
    }
  }
  caml_plat_unlock(&stack_cache_global_lock);
  CAML_GC_MESSAGE(STACKS,
                  "Freed %" ARCH_INTNAT_PRINTF_FORMAT "u cached stack "
                  "mappings (lifetime: %" ARCH_INTNAT_PRINTF_FORMAT
                  "u created, %" ARCH_INTNAT_PRINTF_FORMAT "u idled, %"
                  ARCH_INTNAT_PRINTF_FORMAT "u woken).\n",
                  freed,
                  atomic_load_relaxed(&stacks_created),
                  atomic_load_relaxed(&stacks_idled),
                  atomic_load_relaxed(&stacks_woken));
  CAML_GC_MESSAGE(STACKS,
                  "Stack cache hits: %" ARCH_INTNAT_PRINTF_FORMAT
                  "u local, %" ARCH_INTNAT_PRINTF_FORMAT "u global, %"
                  ARCH_INTNAT_PRINTF_FORMAT "u extra; KiB copied: %"
                  ARCH_INTNAT_PRINTF_FORMAT "u idling, %"
                  ARCH_INTNAT_PRINTF_FORMAT "u waking.\n",
                  atomic_load_relaxed(&stack_cache_hits_local),
                  atomic_load_relaxed(&stack_cache_hits_global),
                  atomic_load_relaxed(&stack_cache_hits_extra),
                  atomic_load_relaxed(&stacks_idled_bytes) / 1024,
                  atomic_load_relaxed(&stacks_woken_bytes) / 1024);
}

#endif /* STACK_GUARD_PAGES */

/* Report the stack counters */
void caml_stack_stats_print(void)
{
#define F_U "%"ARCH_INTNAT_PRINTF_FORMAT"u"
  CAML_GC_MESSAGE(STATS,
                  "Stacks: "F_U" created, "F_U" freed, "
                  F_U" live, "F_U" peak.\n",
                  atomic_load_relaxed(&stacks_created),
                  atomic_load_relaxed(&stacks_freed),
                  atomic_load_relaxed(&stack_count),
                  atomic_load_relaxed(&stack_count_peak));
#ifdef STACK_GUARD_PAGES
  CAML_GC_MESSAGE(STATS,
                  "  "F_U" idled ("F_U" KiB), "
                  F_U" woken ("F_U" KiB), "
                  F_U" demoted.\n",
                  atomic_load_relaxed(&stacks_idled),
                  atomic_load_relaxed(&stacks_idled_bytes) / 1024,
                  atomic_load_relaxed(&stacks_woken),
                  atomic_load_relaxed(&stacks_woken_bytes) / 1024,
                  atomic_load_relaxed(&stacks_demoted));
  CAML_GC_MESSAGE(STATS,
                  "  Cache hits: "F_U" local, "F_U" global, "F_U" extra\n",
                  atomic_load_relaxed(&stack_cache_hits_local),
                  atomic_load_relaxed(&stack_cache_hits_global),
                  atomic_load_relaxed(&stack_cache_hits_extra));
#endif
}

static void assert_is_cont(value cont) {
  CAMLassert(Is_block(cont) && Tag_val(cont) == Cont_tag);
}

/* Take the stack chain out of [cont] without mapping it. */
static value continuation_take(value cont)
{
  value v;
  value null_stk = Val_ptr(NULL);
  CAMLnoalloc;

  fiber_debug_log("cont: is_block(%d) tag_val(%ul) is_young(%d)",
                  Is_block(cont), Tag_val(cont), Is_young(cont));
  assert_is_cont(cont);

  /* this forms a barrier between execution and any other domains
     that might be marking this continuation */
  if (!Is_young(cont) && caml_marking_started())
    caml_darken_cont(cont);

  v = Field(cont, 0);

  if (caml_domain_alone()) {
    Field(cont, 0) = null_stk;
    return v;
  }

  if (atomic_compare_exchange_strong(Op_atomic_val(cont), &v, null_stk)) {
    return v;
  } else {
    return null_stk;
  }
}

/* Called (also from the stack-switching assembly code) to resume a
   continuation: idle stacks are woken here, and the last-fiber field of
   [cont] is updated (the assembly reloads it after this call). */
CAMLprim value caml_continuation_use_noexc (value cont)
{
  value v = continuation_take(cont);
#ifdef STACK_GUARD_PAGES
  struct stack_info* stk = Ptr_val(v);
  if (stk != NULL && stk->idled_from != NULL)
    v = continuation_wake_stacks(cont, stk);
#endif
  return v;
}

value caml_continuation_use_raw_noexc (value cont)
{
  return continuation_take(cont);
}

CAMLprim value caml_continuation_use (value cont)
{
  value v = caml_continuation_use_noexc(cont);
  if (v == Val_ptr(NULL))
    caml_raise_continuation_already_resumed();
  return v;
}

bool caml_continuation_is_preemption(value cont) {
  assert_is_cont(cont);
  return Wosize_val(cont) == 3;
}

value* caml_continuation_gc_regs(value cont) {
  assert_is_cont(cont);
  if (caml_continuation_is_preemption(cont)) {
    return (value*)Field(cont, 2);
  } else {
    return NULL;
  }
}

void caml_continuation_replace(value cont, struct stack_info* stk)
{
  assert_is_cont(cont);
  value n = Val_ptr(NULL);
  int b = atomic_compare_exchange_strong(Op_atomic_val(cont), &n, Val_ptr(stk));
  CAMLassert(b);
  (void)b; /* squash unused warning */
}

CAMLprim value caml_continuation_update_handler_noexc
  (value cont, value hval, value hexn, value heff, value htick)
{
  /* Note: this can be noalloc because, despite participating in marking (by
     potentially calling [caml_darken_cont], through
     [caml_continuation_use_noexc]), it can't actually enter the GC */
  CAMLnoalloc;
  value stack;
  struct stack_info* stk;

  stack = caml_continuation_use_raw_noexc (cont);
  stk = Ptr_val(stack);
  if (stk == NULL) {
    /* The continuation has already been taken */
    return cont;
  }
  stk = Ptr_val(Field(cont, 1));
  Stack_handle_value(stk) = hval;
  Stack_handle_exception(stk) = hexn;
  Stack_handle_effect(stk) = heff;
  Stack_handle_tick(stk) = htick;
  caml_continuation_replace(cont, Ptr_val(stack));

  return cont;
}

/* Update only the tick handler of a continuation, leaving all other handlers
   unchanged */
CAMLprim value caml_continuation_update_tick_handler_noexc
  (value cont, value htick)
{
  /* Note: this can be noalloc because, despite participating in marking (by
     potentially calling [caml_darken_cont], through
     [caml_continuation_use_noexc]), it can't actually enter the GC */
  CAMLnoalloc;
  value stack;
  struct stack_info *stk;

  stack = caml_continuation_use_raw_noexc (cont);
  stk = Ptr_val(stack);
  if (stk == NULL) {
    /* The continuation has already been taken */
    return cont;
  }
  while (Stack_parent(stk) != NULL) stk = Stack_parent(stk);
  Stack_handle_tick(stk) = htick;
  caml_continuation_replace(cont, Ptr_val(stack));

  return cont;
}

static const value * _Atomic caml_unhandled_effect_exn = NULL;
static const value * _Atomic caml_continuation_already_resumed_exn = NULL;

static const value * cache_named_exception(const value * _Atomic * cache,
                                           const char * name)
{
  const value * exn;
  exn = atomic_load_acquire(cache);
  if (exn == NULL) {
    exn = caml_named_value(name);
    if (exn == NULL) {
      fprintf(stderr, "Fatal error: exception %s\n", name);
      exit(2);
    }
    atomic_store_release(cache, exn);
  }
  return exn;
}

static const value * cache_named_effect(const value * _Atomic * cache,
                                        const char * name)
{
  const value * exn;
  exn = atomic_load_acquire(cache);
  if (exn == NULL) {
    exn = caml_named_value(name);
    if (exn == NULL) {
      fprintf(stderr, "Fatal error: effect %s\n", name);
      exit(2);
    }
    atomic_store_release(cache, exn);
  }
  return exn;
}

CAMLexport void caml_raise_continuation_already_resumed(void)
{
  const value * exn =
    cache_named_exception(&caml_continuation_already_resumed_exn,
                          "Effect.Continuation_already_resumed");
  caml_raise(*exn);
}

value caml_make_unhandled_effect_exn (value effect)
{
  CAMLparam1(effect);
  value res;
  const value * exn =
    cache_named_exception(&caml_unhandled_effect_exn, "Effect.Unhandled");
  res = caml_alloc_small(2,0);
  Field(res, 0) = *exn;
  Field(res, 1) = effect;
  CAMLreturn(res);
}

CAMLexport void caml_raise_unhandled_effect (value effect)
{
  caml_raise(caml_make_unhandled_effect_exn(effect));
}

static const value * _Atomic caml_preemption_effect = NULL;

CAMLexport value caml_get_preemption_effect(void) {
  CAMLnoalloc;
  const value *eff =
    cache_named_effect(&caml_preemption_effect, "Effect.Preemption");
  return *eff;
}

/* Call the tick handler for each running fiber *in reverse order*, stopping as
   soon as one preempts

   Returns Result_value(Val_true) if a preemption occurred,
   Result_value(Val_false) if one did not, or a Result_exception
   result if any of the callbacks raised an exception.
*/
caml_result caml_tick_fiber_res(struct stack_info *stack) {
  caml_result res;
  /* The tick handlers below run as callbacks on the current stack: if one
     grows it, [caml_try_realloc_stack] frees its [stack_info]. Only the
     current stack can move, so reload it after running the parents'
     handlers. */
  int is_current = stack == Caml_state->current_stack;

  if (Stack_parent(stack)) {
    res = caml_tick_fiber_res(Stack_parent(stack));
    if (caml_result_is_exception(res) || res.data == Val_true) {
      return res;
    }
    if (is_current) {
      stack = Caml_state->current_stack;
    }
  }

  if (Stack_is_preemptible(stack)) {
    res = caml_callback_res(Stack_handle_tick(stack), Val_unit);
    if (caml_result_is_exception(res)) {
      return res;
    }

    switch (Long_val(res.data)) {
    case TICK_RESULT_PREEMPT:
      return Result_value(Val_true);
    case TICK_RESULT_CONTINUE:
      break;
    default: {
      value exn =
        caml_exception_failure_value(caml_copy_string(
          "caml_tick_fiber: tick_handler returned invalid result"));
      return Result_exception(exn);
    }
    }
  }

  return Result_value(Val_false);
}
