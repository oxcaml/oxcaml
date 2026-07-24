/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2007 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

#include <unistd.h>
#define __USE_GNU
#if !defined(__OpenBSD__)
#define _GNU_SOURCE
#include <sys/ucontext.h>
#endif

/* Signal handling, code specific to the native-code compiler */

#include <signal.h>
#include <errno.h>
#include <stdio.h>
#include "caml/codefrag.h"
#include "caml/domain.h"
#include "caml/fail.h"
#include "caml/fiber.h"
#include "caml/frame_descriptors.h"
#include "caml/memory.h"
#include "caml/osdeps.h"
#include "caml/signals.h"
#include "caml/stack.h"

/* Get allocation info for the current frame, for re-doing allocations after GC
   or resuming preemption.

   Returns the allocation size in words (wosize, without header).
   Sets *alloc_len_out and *nallocs_out if they are non-NULL. */
Caml_inline intnat current_frame_alloc_wosize(unsigned char** alloc_len_out,
                                              int* nallocs_out)
{
  frame_descr* d;
  caml_domain_state * dom_st = Caml_state;
  caml_frame_descrs* fds = caml_get_frame_descrs();
  struct stack_info* stack = dom_st->current_stack;

  char * sp = (char*)stack->sp;
  sp = First_frame(sp);
  uintnat retaddr = Saved_return_address(sp);

  /* Find the frame descriptor for the current allocation */
  d = caml_find_frame_descr(fds, retaddr);
  CAMLassert(d && !frame_return_to_C(d) && frame_has_allocs(d));

  /* Compute the total allocation size at this point,
     including allocations combined by Comballoc */
  unsigned char* alloc_len = frame_end_of_live_ofs(d);
  int nallocs = *alloc_len++;

  if (nallocs == 0) {
    return 0; /* This is a poll */
  }

  intnat allocsz = 0;
  for (int i = 0; i < nallocs; i++) {
    allocsz += Whsize_wosize(Wosize_encoded_alloc_len(alloc_len[i]));
  }
  /* We have computed whsize (including header) but need wosize (without) */
  allocsz -= 1;

  if (alloc_len_out != NULL) *alloc_len_out = alloc_len;
  if (nallocs_out != NULL) *nallocs_out = nallocs;

  return allocsz;
}

/* This routine is the common entry point for garbage collection
   and signal handling.  It can trigger a callback to OCaml code.
   With system threads, this callback can cause a context switch.
   Hence [caml_garbage_collection] must not be called from regular C code
   (e.g. the [caml_alloc] function) because the context of the call
   (e.g. [intern_val]) may not allow context switching.
   Only generated assembly code can call [caml_garbage_collection],
   via the caml_call_gc assembly stubs.  */

void caml_garbage_collection(void)
{
  caml_domain_state * dom_st = Caml_state;

  /* Synchronise for the case when [young_limit] was used to interrupt
     us. */
  atomic_thread_fence(memory_order_acquire);

  unsigned char* alloc_len = NULL;
  int nallocs = 0;
  intnat allocsz = current_frame_alloc_wosize(&alloc_len, &nallocs);

  if (nallocs == 0) {
    /* This is a poll */
    caml_process_pending_actions_flags(CAML_FROM_CAML);
    return;
  }

  caml_alloc_small_dispatch(dom_st, allocsz, CAML_DO_TRACK | CAML_FROM_CAML,
                            nallocs, alloc_len);
}

/* Redo the allocation that was interrupted by preemption. */
void caml_redo_preempted_allocation(void)
{
  caml_domain_state * dom_st = Caml_state;

  intnat allocsz = current_frame_alloc_wosize(NULL, NULL);

  if (allocsz == 0) {
    /* This is a poll - no allocation to redo */
    return;
  }

  dom_st->young_ptr -= Whsize_wosize(allocsz);
  /* Check to see if that put us over the limit, and GC if so */
  if (Caml_check_gc_interrupt(dom_st)) {
    Alloc_small_enter_GC(dom_st, allocsz);
  }
}

#if defined(STACK_GUARD_PAGES) || defined(FAULTING_SAFEPOINTS)

#if !defined(POSIX_SIGNALS)
#error "POSIX signals are required for stack guard pages and for the \
faulting safepoints"
#endif

typedef void (*sigaction_t)(int sig, siginfo_t *info, void *context);

#define DECLARE_SIGNAL_HANDLER(name) \
  static void name(int sig, siginfo_t * info, ucontext_t * context)

#define SET_SIGACT(sigact,name)                                       \
  sigact.sa_sigaction = (sigaction_t) (name);    \
  sigact.sa_flags = SA_SIGINFO

#ifdef STACK_GUARD_PAGES
CAMLextern void caml_raise_stack_overflow_nat(void);
#endif

static sigaction_t prior_segv_handler = NULL;
#ifdef SYS_macosx
static sigaction_t prior_sigbus_handler = NULL;
#endif

#ifdef FAULTING_SAFEPOINTS

/* The GC entry stub common to all architectures (amd64.S, arm64.S). */
extern void caml_call_gc(void);

/* The faulting instructions recognized by our SEGV handler: at
   present just poll points.

   [match_len] bytes of [bytes] identify a row at the fault PC; a
   genuine poll faults at trigger page + [addr_offset]; the SEGV
   handler turns the fault into a call to [entry], resuming at PC +
   [insn_len]. Add rows to fault_insns to give polls variant encodings
   or to make SEGV serve other purposes (e.g. stack checks). */
typedef struct {
  uint8_t match_len;   /* bytes of [bytes] compared at the fault PC */
  uint8_t insn_len;    /* the faked call resumes at PC + insn_len */
  uint8_t addr_offset; /* the load faults at trigger page + this */
  void (*entry)(void); /* GC entry stub the faked call enters */
  uint8_t bytes[4];
} fault_insn;

/* Bound on [addr_offset] over all faulting loads: payloads are at
   most one byte (amd64 disp8 must also be non-negative or the load
   would miss the trigger page). Used as a cheap first filter on
   si_addr in the handler. */
#define FAULT_ADDR_RANGE 256

#endif /* FAULTING_SAFEPOINTS */

/**** AMD64: context accessors and fault instruction encodings ****/

/* All knowledge of each platform's mcontext layout lives in the
   context_* accessors and context_fake_call, keeping the handler
   logic itself platform-independent. */

#ifdef TARGET_amd64

Caml_inline void context_set_pc(ucontext_t* context, void (*pc)(void))
{
#ifdef SYS_macosx
  context->uc_mcontext->__ss.__rip = (unsigned long long) pc;
#else
  context->uc_mcontext.gregs[REG_RIP] = (greg_t) pc;
#endif
}

#ifdef FAULTING_SAFEPOINTS

Caml_inline unsigned char* context_pc(ucontext_t* context)
{
#ifdef SYS_macosx
  return (unsigned char*) context->uc_mcontext->__ss.__rip;
#else
  return (unsigned char*) context->uc_mcontext.gregs[REG_RIP];
#endif
}

Caml_inline uintnat context_sp(ucontext_t* context)
{
#ifdef SYS_macosx
  return (uintnat) context->uc_mcontext->__ss.__rsp;
#else
  return (uintnat) context->uc_mcontext.gregs[REG_RSP];
#endif
}

Caml_inline void context_set_sp(ucontext_t* context, uintnat sp)
{
#ifdef SYS_macosx
  context->uc_mcontext->__ss.__rsp = (unsigned long long) sp;
#else
  context->uc_mcontext.gregs[REG_RSP] = (greg_t) sp;
#endif
}

/* %rcx: see [Proc.destroyed_at_poll]. */
Caml_inline uintnat context_fault_reg(ucontext_t* context)
{
#ifdef SYS_macosx
  return (uintnat) context->uc_mcontext->__ss.__rcx;
#else
  return (uintnat) context->uc_mcontext.gregs[REG_RCX];
#endif
}

/* Rewrite [context] so that returning from the signal behaves as if
   the poll site had called [pi->entry]: push the address of the
   instruction after the poll as the return address (OCaml frames have
   no red zone, and polls imply [contains_calls], hence the headroom
   and the standard call-site stack alignment class) and resume in the
   stub. */
static void context_fake_call(ucontext_t* context, const fault_insn* pi)
{
  uintnat sp = context_sp(context) - sizeof(uintnat);
  *(unsigned char**) sp = context_pc(context) + pi->insn_len;
  context_set_sp(context, sp);
  context_set_pc(context, pi->entry);
}

/* GC entry stubs differing in how much SIMD state they preserve. */
extern void caml_call_gc_sse(void);
extern void caml_call_gc_avx(void);
extern void caml_call_gc_avx512(void);

/* The load's displacement indicates what SIMD state is live at the
   poll, hence which stub must save it. */
static const fault_insn fault_insns[] = {
  {2, 2,  0, &caml_call_gc,        {0x8b, 0x09}},       /* movl (%rcx),%ecx */
  {3, 3,  4, &caml_call_gc_sse,    {0x8b, 0x49, 0x04}}, /* movl 4(%rcx),%ecx */
  {3, 3,  8, &caml_call_gc_avx,    {0x8b, 0x49, 0x08}}, /* movl 8(%rcx),%ecx */
  {3, 3, 12, &caml_call_gc_avx512, {0x8b, 0x49, 0x0c}}, /* movl 12(%rcx),%ecx */
};

#endif /* FAULTING_SAFEPOINTS */

#endif /* TARGET_amd64 */

/**** ARM64: context accessors and fault instruction encodings ****/

#ifdef TARGET_arm64

/* On Darwin OCaml targets arm64, not arm64e, so the thread-state
   fields are plain integers, not pointer-authenticated. */

Caml_inline unsigned char* context_pc(ucontext_t* context)
{
#ifdef SYS_macosx
  return (unsigned char*) context->uc_mcontext->__ss.__pc;
#else
  return (unsigned char*) context->uc_mcontext.pc;
#endif
}

Caml_inline void context_set_pc(ucontext_t* context, void (*pc)(void))
{
#ifdef SYS_macosx
  context->uc_mcontext->__ss.__pc = (unsigned long long) pc;
#else
  context->uc_mcontext.pc = (uintnat) pc;
#endif
}

#ifdef FAULTING_SAFEPOINTS

Caml_inline uintnat context_sp(ucontext_t* context)
{
#ifdef SYS_macosx
  return (uintnat) context->uc_mcontext->__ss.__sp;
#else
  return (uintnat) context->uc_mcontext.sp;
#endif
}

/* x16 = reg_tmp1: see assembly_code_for_poll in
   backend/arm64/emit.ml. */
Caml_inline uintnat context_fault_reg(ucontext_t* context)
{
#ifdef SYS_macosx
  return (uintnat) context->uc_mcontext->__ss.__x[16];
#else
  return (uintnat) context->uc_mcontext.regs[16];
#endif
}

/* A call is faked by setting LR, exactly as BL would. */
static void context_fake_call(ucontext_t* context, const fault_insn* pi)
{
  uintnat lr = (uintnat) (context_pc(context) + pi->insn_len);
#ifdef SYS_macosx
  context->uc_mcontext->__ss.__lr = lr;
#else
  context->uc_mcontext.regs[30] = lr;
#endif
  context_set_pc(context, pi->entry);
}

/* ARM64 at present only has one GC entry stub. If variant encodings
   are ever wanted, a payload in the load's immediate field costs no
   code size here, and amd64's payload values are multiples of 4 so
   the scaled 32-bit LDR immediate can carry the same offsets. */
static const fault_insn fault_insns[] = {
  {4, 4, 0, &caml_call_gc, {0x10, 0x02, 0x40, 0xb9}}, /* ldr w16, [x16] */
};

#endif /* FAULTING_SAFEPOINTS */

#endif /* TARGET_arm64 */

/**** Fault instruction recognition (architecture-independent) ****/

#ifdef FAULTING_SAFEPOINTS

#define FAULT_INSNS_COUNT (sizeof fault_insns / sizeof fault_insns[0])

/* Match the instruction at [pc] against the table; NULL if it is not
   a recognized fault instruction. */
static const fault_insn* fault_insn_match(const unsigned char* pc)
{
  for (size_t i = 0; i < FAULT_INSNS_COUNT; i++) {
    const fault_insn* pi = &fault_insns[i];
    bool match = true;
    for (size_t j = 0; match && j < pi->match_len; j++) {
      if (pc[j] != pi->bytes[j]) match = false;
    }
    if (match) return pi;
  }
  return NULL;
}

/* Defence against wild memory accesses that happen to hit the trigger
   page: identify the fault as an armed poll executed by OCaml code
   and return its table row, or return NULL, in which case the caller
   falls through to the pre-existing SEGV behaviour and the fault
   crashes in the ordinary way. Runs in signal context: everything
   here is a plain load or a relaxed atomic on this domain's own
   state, and [caml_find_frame_descr] is a lock-free probe. */
static const fault_insn* safepoint_fault_check(siginfo_t* info,
                                              ucontext_t* context)
{
  /* not an access to the trigger page */
  if ((uintnat) info->si_addr - (uintnat) caml_safepoint_trigger_page >=
      FAULT_ADDR_RANGE) {
    return NULL;
  }

#ifndef SYS_macosx
  /* Protection fault on the (mapped, PROT_NONE) trigger page. Not
     checked on Darwin, where the fault arrives as SIGBUS and si_code
     is less well specified; the positive checks below stand alone. */
  if (info->si_code != SEGV_ACCERR) return NULL;
#endif

  /* Running OCaml: domain state is set, and trigger is armed. */
  caml_domain_state* dom_st = Caml_state;
  if (dom_st == NULL) return NULL;
  if (atomic_load_relaxed(&dom_st->safepoint_trigger) !=
      (uintnat) caml_safepoint_trigger_page)
    return NULL;

  /* Recognised faulting instruction, through the right register,
     which points to the trigger page, and has the right fault
     address. */
  unsigned char* pc = context_pc(context);
  const fault_insn* pi = fault_insn_match(pc);
  if (pi == NULL) return NULL;
  if (context_fault_reg(context) != (uintnat) caml_safepoint_trigger_page)
    return NULL;
  if ((uintnat) info->si_addr !=
      (uintnat) caml_safepoint_trigger_page + pi->addr_offset)
    return NULL;

  /* SP is word-aligned, inside the current fiber's Caml stack, with
     room for a faked call. */
  uintnat sp = context_sp(context);
  struct stack_info* stack = dom_st->current_stack;
  if (stack == NULL) return NULL;
  if ((sp & (sizeof(uintnat) - 1)) != 0) return NULL;
  if (sp - sizeof(uintnat) < (uintnat) Stack_base(stack) ||
      sp > (uintnat) Stack_high(stack))
    return NULL;

  /* Last check: resume address is in the frametables and is a poll
     site (allocation frame with zero allocations). */
  frame_descr* d =
    caml_find_frame_descr(caml_get_frame_descrs(),
                          (uintnat) (pc + pi->insn_len));
  if (d == NULL || frame_return_to_C(d) || !frame_has_allocs(d))
    return NULL;
  if (*frame_end_of_live_ofs(d) != 0) return NULL;

  return pi;
}

#endif /* FAULTING_SAFEPOINTS */

/* This handler may acquire more duties over time (e.g.  stack
   checks), so the dispatch is strict and check-heavy: no fault is
   treated as ours without positive identification, and everything
   else falls through to the pre-existing SEGV behaviour. */
DECLARE_SIGNAL_HANDLER(segv_handler)
{
  struct sigaction act;
#ifdef FAULTING_SAFEPOINTS
  const fault_insn* pi = safepoint_fault_check(info, context);
  if (pi != NULL) {
    context_fake_call(context, pi);
    return; /* to a caml_call_gc stub */
  }
#endif
#ifdef STACK_GUARD_PAGES
  if (Caml_state) {
    struct stack_info *block = Caml_state->current_stack;
    char* fault_addr = info->si_addr;
    char* protected_low = Protected_stack_page(block);
    char* protected_high = protected_low + caml_plat_pagesize;
    if ((fault_addr >= protected_low) && (fault_addr < protected_high)) {
      /* Faulting in the current guard page; presume stack overflow. Raise the
         exception. */
      context_set_pc(context, &caml_raise_stack_overflow_nat);
      return; /* to caml_raise_stack_overflow_nat */
    }
  }
#endif /* STACK_GUARD_PAGES */

  /* Not a fault the runtime knows about */
  sigaction_t prior = prior_segv_handler;
#ifdef SYS_macosx
  if (sig == SIGBUS) prior = prior_sigbus_handler;
#endif
  if (prior) {
    /* Somebody else installed a handler for this signal before us.
     * We make "reasonable best efforts" to invoke it, as maybe it's
     * a fault they know about and can fix. We can't apply whatever
     * sa_flags they had, but we can call their handler. */
    prior(sig, info, context);
  } else { /* default behaviour for this signal */
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaction(sig, &act, NULL);
  }
  /* returning from SEGV handler restarts the failing instruction */
}

void caml_init_nat_signals(void)
{
  struct sigaction act, oldact;
#ifndef FAULTING_SAFEPOINTS
  /* The handler is optional when it only serves stack checks; under
     faulting safepoints poll points rely on it. */
  extern uintnat caml_enable_segv_handler;
  if (!caml_enable_segv_handler)
    return;
#endif
  SET_SIGACT(act, segv_handler);
  act.sa_flags |= SA_ONSTACK;
  sigemptyset(&act.sa_mask);
  sigaction(SIGSEGV, &act, &oldact);
  if (oldact.sa_sigaction != (sigaction_t)SIG_DFL) {
    prior_segv_handler = oldact.sa_sigaction;
  }
#ifdef SYS_macosx
  /* Darwin delivers a protection fault on a mapped page (an armed
     safepoint trigger, a fiber guard page) as SIGBUS, reserving
     SIGSEGV for unmapped addresses, so the handler must field both. */
  sigaction(SIGBUS, &act, &oldact);
  if (oldact.sa_sigaction != (sigaction_t)SIG_DFL) {
    prior_sigbus_handler = oldact.sa_sigaction;
  }
#endif
}

#else

void caml_init_nat_signals(void)
{
}

#endif
