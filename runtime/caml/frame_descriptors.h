/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       */
/*                   Tom Kelly, OCaml Labs Consultancy                    */
/*                 Stephen Dolan, University of Cambridge                 */
/*                                                                        */
/*   Copyright 2019 Indian Institute of Technology, Madras                */
/*   Copyright 2021 OCaml Labs Consultancy Ltd                            */
/*   Copyright 2019 University of Cambridge                               */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_FRAME_DESCRIPTORS_H
#define CAML_FRAME_DESCRIPTORS_H

#ifdef CAML_INTERNALS

#include <stdbool.h>
#include "config.h"
#include "misc.h"

/* The compiler generates a "frame descriptor" for every potential
 * return address. Each loaded module has a block of memory, the
 * "frame table", consisting of these frame descriptors
 * concatenated. Each frame descriptor includes:
 *
 * - frame_return_to_C(): Whether the return is to C from OCaml, in
 *   which case there is no actual stack frame, GC roots, allocation
 *   sizes, or debug info.  See caml_system__frametable in the various
 *   architecture-specific OCaml/C interfaces.
 *
 * - frame_size(): The stack frame size, in bytes. All stack frames
 *   are word-aligned so we also store information in the bottom two
 *   bits:
 *
 * - frame_has_allocs(): Whether it is the return address of a call
 *   into the garbage collector, and if so the sizes of all objects to
 *   be allocated by this call. Each size is stored reduced by 1, so
 *   that a single byte can record sizes (wosize) from 1 to 256 words.
 *
 * - frame_has_debug(): Whether the frame descriptor has debug
 *   information. (The debug info is one or more 32-bit relative
 *   pointers: if frame_has_allocs(), there is one for each of the set
 *   of allocations performed by this GC entry).
 *
 * - the register or stack frame offset of every "live" value,
 *   which should be scanned by the garbage collector if a GC is
 *   performed at this point.
 *
 * A frame descriptor is read as a packed byte sequence rather than
 * through a C struct: we treat a [frame_descr *] as an opaque byte
 * pointer and read its fields at explicit byte offsets via
 * [caml_read_unaligned_*]. This allows a denser descriptor format.
 *
 * There are two layouts; the field at offset 4 tells them apart (a uint16
 * equal to FRAME_LONG_MARKER means long):
 *
 *  normal:                      long:
 *    int32  retaddr_rel  @ 0      int32  retaddr_rel  @ 0
 *    uint16 frame_data   @ 4      uint16 marker       @ 4: FRAME_LONG_MARKER
 *    uint16 num_live     @ 6      uint16 _pad         @ 6
 *    uint16 live_ofs[]   @ 8      uint32 frame_data   @ 8
 *                                 uint32 num_live     @ 12
 *                                 uint32 live_ofs[]   @ 16
 *
 * retaddr_rel is the byte offset from the descriptor to the return address.
 *
 * After live_ofs[]:
 *
 *   If frame_has_allocs(), allocation sizes follow:
 *       uint8 num_allocs;
 *       uint8 alloc[num_allocs];
 *
 *   If frame_has_debug(), debug info follows (32-bit aligned):
 *       int32 debug_info[frame_has_allocs() ? num_allocs : 1];
 *
 * Each debug info is a signed relative offset, in bytes, from its own
 * address to a debuginfo structure. */

typedef unsigned char frame_descr;

#define FRAME_DESCRIPTOR_DEBUG 1
#define FRAME_DESCRIPTOR_ALLOC 2
#define FRAME_DESCRIPTOR_FLAGS 3
#define FRAME_RETURN_TO_C 0xFFFF
#define FRAME_LONG_MARKER 0x7FFF

/* Field byte offsets within a descriptor (normal and long). */
#define Frame_retaddr_rel_ofs   0
#define Frame_data_ofs          4
#define Frame_num_live_ofs      6
#define Frame_live_ofs          8
#define Frame_long_data_ofs     8
#define Frame_long_num_live_ofs 12
#define Frame_long_live_ofs     16

Caml_inline bool frame_return_to_C(frame_descr *d) {
  return caml_read_unaligned_uint16(d + Frame_data_ofs) == FRAME_RETURN_TO_C;
}

Caml_inline bool frame_is_long(frame_descr *d) {
  CAMLassert(d && !frame_return_to_C(d));
  return (caml_read_unaligned_uint16(d + Frame_data_ofs) == FRAME_LONG_MARKER);
}

Caml_inline uint32_t frame_data(frame_descr *d) {
  if (frame_is_long(d)) {
    return caml_read_unaligned_uint32(d + Frame_long_data_ofs);
  } else {
    return caml_read_unaligned_uint16(d + Frame_data_ofs);
  }
}

Caml_inline unsigned char *frame_end_of_live_ofs(frame_descr *d) {
  if (frame_is_long(d)) {
    uint32_t num_live = caml_read_unaligned_uint32(d + Frame_long_num_live_ofs);
    return d + Frame_long_live_ofs + (uintnat)num_live * sizeof(uint32_t);
  } else {
    uint16_t num_live = caml_read_unaligned_uint16(d + Frame_num_live_ofs);
    return d + Frame_live_ofs + (uintnat)num_live * sizeof(uint16_t);
  }
}

Caml_inline uint32_t frame_size(frame_descr *d) {
  return frame_data(d) &~ FRAME_DESCRIPTOR_FLAGS;
}

Caml_inline bool frame_has_allocs(frame_descr *d) {
  return (frame_data(d) & FRAME_DESCRIPTOR_ALLOC) != 0;
}

Caml_inline bool frame_has_debug(frame_descr *d) {
  return (frame_data(d) & FRAME_DESCRIPTOR_DEBUG) != 0;
}

/* Allocation lengths are encoded reduced by one, so values 0-255 mean
 * sizes 1-256 words. */

#define Wosize_encoded_alloc_len(n) ((uintnat)(n) + 1)

/* Used to compute offsets in frame tables.
   ty must have power-of-2 size */
#define Align_to(p, ty) \
  (void*)(((uintnat)(p) + sizeof(ty) - 1) & ~(sizeof(ty) - 1))

#define Hash_retaddr(addr, mask)                          \
  ((((uintnat)(addr) * 52437813) >> 5) & (mask))

#define Retaddr_frame(d) \
  ((uintnat)(d) + \
   (uintnat)(intnat)caml_read_unaligned_int32(d))

void caml_init_frame_descriptors(void);

void caml_register_frametables(void **tables, int ntables);
void caml_register_frametable(void *table);

/* Create copies of the frametables and register them in the runtime.
   It writes back the pointers of the new copies of the frametables.
   Calling 'caml_unregister_frametable(s)' on these copies is safe
   and will free the allocated memory. */
void caml_copy_and_register_frametables(void **table, int *sizes, int ntables);
void* caml_copy_and_register_frametable(void *table, int size);

/* The unregistered frametables can still be in use after calling
   this function. Thus, you should not free their memory.
   Note: it may reorder the content of the array 'tables'.
   This can be called from a custom block finalizer. */
void caml_unregister_frametables(void **tables, int ntables);
void caml_unregister_frametable(void *table);

/* a linked list of frametables */
typedef struct caml_frametable_list {
  intnat* frametable;
  struct caml_frametable_list *next;
} caml_frametable_list;

/* a hashtable of frame descriptors */
typedef struct caml_frame_descrs caml_frame_descrs;

caml_frame_descrs* caml_get_frame_descrs(void);

/* Find the current table of frame descriptors.
   The resulting structure is only valid until the next GC */
frame_descr* caml_find_frame_descr(caml_frame_descrs *fds, uintnat pc);


/* Returns the next frame descriptor (or NULL if none is available),
   and updates *pc and *sp to point to the following one.  */
frame_descr *caml_next_frame_descriptor
    (caml_frame_descrs * fds, uintnat * pc, char ** sp,
     struct stack_info* stack);

#endif /* CAML_INTERNALS */

#endif /* CAML_FRAME_DESCRIPTORS_H */
