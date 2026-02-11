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

#ifndef CAML_MAJOR_GC_H
#define CAML_MAJOR_GC_H

#ifdef CAML_INTERNALS

typedef enum {
  Phase_sweep_main,
  Phase_sweep_and_mark_main,
  Phase_mark_final,
  Phase_sweep_ephe
} gc_phase_t;

extern gc_phase_t caml_gc_phase;

<<<<<<< HEAD
Caml_inline int caml_marking_started(void) {
  return caml_gc_phase != Phase_sweep_main;
}
extern atomic_uintnat caml_gc_mark_phase_requested;

||||||| 23e84b8c4d
intnat caml_opportunistic_major_work_available (void);
=======
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
intnat caml_opportunistic_major_work_available (caml_domain_state*);
void caml_opportunistic_major_collection_slice (intnat);
/* auto-triggered slice from within the GC */
#define AUTO_TRIGGERED_MAJOR_SLICE -1
/* external triggered slice, but GC will compute the amount of work */
#define GC_CALCULATE_MAJOR_SLICE 0
void caml_major_collection_slice (intnat);
void caml_finish_sweeping(void);
void caml_finish_marking (void);
int caml_init_major_gc(caml_domain_state*);
void caml_teardown_major_gc(void);
void caml_darken(void*, value, volatile value* ignored);
void caml_darken_cont(value);
void caml_mark_root(value, value*);
<<<<<<< HEAD
void caml_mark_roots_stw(int, caml_domain_state**);

/* Compaction modes */
enum {
  Compaction_none,
  Compaction_forced,
  Compaction_auto,
};

void caml_finish_major_cycle(int compaction_mode);
||||||| 23e84b8c4d
void caml_empty_mark_stack(void);
void caml_finish_major_cycle(int force_compaction);
=======
void caml_empty_mark_stack(void);
void caml_finish_major_cycle(int force_compaction);
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
/* Reset any internal accounting the GC uses to set collection pacing.
 * For use at times when we have disturbed the usual pacing, for
 * example, after any synchronous major collection.
 */
void caml_reset_major_pacing(void);
#ifdef DEBUG
int caml_mark_stack_is_empty(void);
#endif
void caml_orphan_ephemerons(caml_domain_state*);
void caml_orphan_finalisers(caml_domain_state*);

/* This variable is only written with the world stopped,
   so it need not be atomic */
extern uintnat caml_major_cycles_completed;

<<<<<<< HEAD
||||||| 23e84b8c4d
double caml_mean_space_overhead(void);

=======
Caml_inline void caml_update_major_allocated_words(
  caml_domain_state *self, intnat words, int direct
) {
  self->allocated_words += words;
  if (direct) {
    self->allocated_words_direct += words;
  }
  if (self->gc_policy & CAML_GC_RAMP_UP) {
    self->allocated_words_suspended += words;
  }
}

>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
#endif /* CAML_INTERNALS */

#endif /* CAML_MAJOR_GC_H */
