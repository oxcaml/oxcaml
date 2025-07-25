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
  frame_descr* d;
  caml_domain_state * dom_st = Caml_state;
  caml_frame_descrs fds = caml_get_frame_descrs();
  struct stack_info* stack = dom_st->current_stack;

  char * sp = (char*)stack->sp;
  sp = First_frame(sp);
  uintnat retaddr = Saved_return_address(sp);

  /* Synchronise for the case when [young_limit] was used to interrupt
     us. */
  atomic_thread_fence(memory_order_acquire);

  { /* Find the frame descriptor for the current allocation */
    d = caml_find_frame_descr(fds, retaddr);
    /* Must be an allocation frame */
    CAMLassert(d && !frame_return_to_C(d) && frame_has_allocs(d));
  }

  { /* Compute the total allocation size at this point,
       including allocations combined by Comballoc */
    unsigned char* alloc_len = frame_end_of_live_ofs(d);
    int i, nallocs = *alloc_len++;
    intnat allocsz = 0;

    if (nallocs == 0) {
      /* This is a poll */
      caml_process_pending_actions();
      return;
    }
    else
    {
      for (i = 0; i < nallocs; i++) {
        allocsz += Whsize_wosize(Wosize_encoded_alloc_len(alloc_len[i]));
      }
      /* We have computed whsize (including header)
         but need wosize (without) */
      allocsz -= 1;
    }

    caml_alloc_small_dispatch(dom_st, allocsz, CAML_DO_TRACK | CAML_FROM_CAML,
                              nallocs, alloc_len);
  }
}

#ifdef STACK_GUARD_PAGES

#if !defined(POSIX_SIGNALS)
#error "stack checks cannot be disabled if POSIX signals are not available"
#endif

typedef void (*sigaction_t)(int sig, siginfo_t *info, void *context);

#define DECLARE_SIGNAL_HANDLER(name) \
  static void name(int sig, siginfo_t * info, ucontext_t * context)

#define SET_SIGACT(sigact,name)                                       \
  sigact.sa_sigaction = (sigaction_t) (name);    \
  sigact.sa_flags = SA_SIGINFO

CAMLextern void caml_raise_stack_overflow_nat(void);

static sigaction_t prior_segv_handler = NULL;

DECLARE_SIGNAL_HANDLER(segv_handler)
{
  struct sigaction act;
  if (Caml_state) {
    struct stack_info *block = Caml_state->current_stack;
    char* fault_addr = info->si_addr;
    char* protected_low = Protected_stack_page(block);
    char* protected_high = protected_low + caml_plat_pagesize;
    if ((fault_addr >= protected_low) && (fault_addr < protected_high)) {
      /* Faulting in the current guard page; presume stack overflow. Raise the
         exception. */
#ifdef SYS_macosx
      context->uc_mcontext->__ss.__rip = (unsigned long long) &caml_raise_stack_overflow_nat;
#else
      context->uc_mcontext.gregs[REG_RIP]= (greg_t) &caml_raise_stack_overflow_nat;
#endif
      return; /* to caml_raise_stack_overflow_nat */
    }
  }

  /* Not a stack-overflow on our current Caml stack */
  if (prior_segv_handler) {
    /* Somebody else installed a SEGV handler before us. We make
     * "reasonable best efforts" to invoke it, as maybe it's a SEGV
     * they know about and can fix. We can't apply whatever sa_flags
     * they had, but we can call their handler. */
    prior_segv_handler(sig, info, context);
  } else { /* default SEGV behaviour */
    act.sa_handler = SIG_DFL;
    act.sa_flags = 0;
    sigemptyset(&act.sa_mask);
    sigaction(SIGSEGV, &act, NULL);
  }
  /* returning from SEGV handler restarts the failing instruction */
}

void caml_init_nat_signals(void)
{
  struct sigaction act, oldact;
  SET_SIGACT(act, segv_handler);
  act.sa_flags |= SA_ONSTACK;
  sigemptyset(&act.sa_mask);
  sigaction(SIGSEGV, &act, &oldact);
  if (oldact.sa_sigaction != (sigaction_t)SIG_DFL) {
    prior_segv_handler = oldact.sa_sigaction;
  }
}

#else

void caml_init_nat_signals(void)
{
}

#endif
