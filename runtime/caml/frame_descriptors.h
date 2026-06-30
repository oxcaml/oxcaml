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
 * - frame_has_debug(): Whether we have debug information for this
 *   stack frame, and if so the "debuginfo" (source location) of this
 *   return address. (If frame_has_allocs(), this is an array of
 *   debuginfo, one for each of the set of allocations performed by
 *   this GC entry).
 *
 * - the register or stack frame offset of every "live" value,
 *   which should be scanned by the garbage collector if a GC is
 *   performed at this point.
 */

#define FRAME_DESCRIPTOR_DEBUG 1
#define FRAME_DESCRIPTOR_ALLOC 2
#define FRAME_DESCRIPTOR_FLAGS 3
#define FRAME_RETURN_TO_C 0xFFFF
#define FRAME_LONG_MARKER 0x7FFF

/* "Small" (compact) frame-descriptor format.
 *
 * Within a frametable, descriptors are concatenated in increasing
 * return-address order, and every descriptor is preceded by a "delta"
 * field encoding the difference between this descriptor's return address
 * and the previous one's:
 *
 *  - a leading 0 byte (FRAME_DELTA_ESCAPE) is an ESCAPE: the bytes that
 *    follow are a descriptor in the existing normal-or-long format (the
 *    wire layout documented at [frame_descr] below). Used for the
 *    first descriptor of each frametable (no previous return address),
 *    for the first descriptor of each new text section (the delta would
 *    span sections), and for any descriptor that does not fit the small
 *    format.
 *
 *  - otherwise the delta is an unsigned LEB128 value (always >= 1, so it
 *    never starts with a 0 byte and so is unambiguous with the escape).
 *    The return address is the previous descriptor's plus the delta. We
 *    use LEB128 rather than a fixed width because the compiler emits the
 *    delta as a label difference resolved by the assembler, which cannot
 *    choose a width by inspecting the value but emits LEB128 of a label
 *    difference directly (even across relaxable branches).
 *
 * A small-descriptor body (after the delta field) is:
 *
 *   (1) size+flags byte: top 6 bits = (frame_size_in_16-byte_units - 1),
 *       i.e. value 0..63 means 1..64 units, i.e. 16..1024 bytes. Bottom
 *       two bits are the flags (FRAME_DESCRIPTOR_ALLOC | _DEBUG).
 *
 *   (2a) if alloc:
 *          uint8_t reg_bitmap;  bit i set => register HOT_REGS[i] is live
 *          uint8_t num_allocs;
 *          uint8_t alloc_sizes[ceil(num_allocs/2)];  4 bits each,
 *                                                     low nibble first
 *          uint8_t num_live;  (live scannable STACK slots, not registers)
 *   (2b) if no alloc:
 *          uint8_t num_live;  (no register byte: non-alloc descriptors
 *                              never keep GC roots in registers)
 *
 *   (num_allocs is always immediately followed by its alloc sizes, and
 *    num_live -- in either branch -- by the live stack slots.)
 *
 *   (3) uint8_t stack_word_ofs[num_live]; each a live stack-slot WORD
 *       offset (byte offset / word size) into the frame.
 *
 *   (4) if debug: uint32_t debug_info[alloc ? num_allocs : 1]; identical
 *       layout to the normal format. */

#define FRAME_DELTA_ESCAPE 0

/* The eight most common GC-root registers, by measurement. The compiler
 * (backend/emitaux.ml, [hot_regs]) and the runtime MUST agree on this
 * table exactly: a mismatch causes SILENT GC HEAP CORRUPTION. The runtime
 * maps bit-index -> register-number (this forward table); the compiler
 * maps register-number -> bit-index. */
#define FRAME_NUM_HOT_REGS 8
CAMLunused static const unsigned char
  caml_frame_hot_regs[FRAME_NUM_HOT_REGS] = { 0, 1, 2, 3, 4, 5, 6, 8 };

/* A frame descriptor is a packed, unaligned byte sequence: since the alignment
 * relaxation the data is no longer C-struct-aligned, so we treat a
 * [frame_descr *] as an opaque byte pointer and read its fields at explicit
 * offsets via [caml_read_unaligned_*] (forming an aligned-typed pointer to the
 * unaligned data would be undefined behaviour).
 *
 * A [frame_descr *] points at a descriptor BODY, i.e. the byte after the delta
 * field. For a small descriptor that is the size+flags byte (see above). For an
 * escaped descriptor it is the normal-or-long wire layout below; the preceding
 * delta byte (== FRAME_DELTA_ESCAPE) and frame_data tell the formats apart:
 *
 *   normal:                          long (frame_data @4 == FRAME_LONG_MARKER):
 *     int32_t  retaddr_rel  @ 0        int32_t  retaddr_rel  @ 0
 *     uint16_t frame_data   @ 4        uint16_t marker       @ 4
 *     uint16_t num_live     @ 6        uint16_t _pad         @ 6
 *     uint16_t live_ofs[]   @ 8        uint32_t frame_data   @ 8
 *                                      uint32_t num_live     @ 12
 *                                      uint32_t live_ofs[]   @ 16
 *
 * After live_ofs[]: if frame_has_allocs(), { uint8_t num_allocs; uint8_t
 * alloc[num_allocs] }; if frame_has_debug(), 32-bit-aligned uint32_t
 * debug_info[frame_has_allocs() ? num_allocs : 1] (each a byte offset from
 * itself to a debuginfo structure). retaddr_rel is the byte offset from the
 * descriptor body to the return address. */
typedef unsigned char frame_descr;

/* Field byte offsets within an escaped descriptor body (normal and long). */
#define Frame_retaddr_rel_ofs   0
#define Frame_data_ofs          4
#define Frame_num_live_ofs      6
#define Frame_live_ofs          8
#define Frame_long_data_ofs     8
#define Frame_long_num_live_ofs 12
#define Frame_long_live_ofs     16

Caml_inline bool frame_is_small(frame_descr *d) {
  return ((const unsigned char *)d)[-1] != FRAME_DELTA_ESCAPE;
}

/* The size+flags byte of a small descriptor. */
Caml_inline unsigned char frame_small_sizeflags(frame_descr *d) {
  return ((const unsigned char *)d)[0];
}

/* Decoded form of one descriptor (small or escaped). [body] points at
 * the size+flags byte (small) or the [frame_descr] struct (escaped). */
struct frame_descr_decoded {
  bool is_small;
  bool return_to_C;
  bool is_long;
  bool has_allocs;
  bool has_debug;
  uint32_t frame_size;   /* in bytes */
  uint32_t num_live;     /* small: live stack slots; escaped: live_ofs[] */
  /* For small descriptors only: */
  const unsigned char *small_allocs;  /* alloc_sizes[] (4-bit nibbles) */
  uint8_t small_num_allocs;
  uint8_t small_reg_bitmap;
  const unsigned char *small_live;    /* stack_word_ofs[num_live] */
  /* The byte just past the live_ofs / stack offsets, i.e. where the
   * alloc-count (escaped) or the debug words begin. */
  const unsigned char *end_of_live;
  /* For escaped descriptors: num debuginfo words (= num_allocs or 1). */
  uint32_t num_debuginfo;
  /* One past the whole descriptor body (start of the next delta byte). */
  const unsigned char *end;
};

void caml_decode_frame_descr(frame_descr *d, struct frame_descr_decoded *out);

Caml_inline bool frame_return_to_C(frame_descr *d) {
  if (frame_is_small(d)) return false;
  return caml_read_unaligned_uint16(d + Frame_data_ofs) == FRAME_RETURN_TO_C;
}

Caml_inline bool frame_is_long(frame_descr *d) {
  CAMLassert(d && !frame_return_to_C(d));
  if (frame_is_small(d)) return false;
  return (caml_read_unaligned_uint16(d + Frame_data_ofs) == FRAME_LONG_MARKER);
}

/* Frame size in bytes of a small descriptor, from its size+flags byte:
   top 6 bits hold (size_in_16-byte_units - 1). */
Caml_inline uint32_t frame_small_size(frame_descr *d) {
  return (((uint32_t)(frame_small_sizeflags(d) >> 2)) + 1) * 16;
}

Caml_inline uint32_t frame_data(frame_descr *d) {
  if (frame_is_small(d)) {
    /* Reconstruct an equivalent normal frame_data: size | flags. */
    return frame_small_size(d) | (frame_small_sizeflags(d) & FRAME_DESCRIPTOR_FLAGS);
  }
  if (frame_is_long(d)) {
    return caml_read_unaligned_uint32(d + Frame_long_data_ofs);
  } else {
    return caml_read_unaligned_uint16(d + Frame_data_ofs);
  }
}

Caml_inline uint32_t frame_size(frame_descr *d) {
  if (frame_is_small(d)) return frame_small_size(d);
  return frame_data(d) &~ FRAME_DESCRIPTOR_FLAGS;
}

Caml_inline bool frame_has_allocs(frame_descr *d) {
  if (frame_is_small(d))
    return (frame_small_sizeflags(d) & FRAME_DESCRIPTOR_ALLOC) != 0;
  return (frame_data(d) & FRAME_DESCRIPTOR_ALLOC) != 0;
}

Caml_inline bool frame_has_debug(frame_descr *d) {
  if (frame_is_small(d))
    return (frame_small_sizeflags(d) & FRAME_DESCRIPTOR_DEBUG) != 0;
  return (frame_data(d) & FRAME_DESCRIPTOR_DEBUG) != 0;
}

/* End of the live-offset region: where alloc lengths / debug words begin.
 * For small descriptors this is past the stack-slot offset bytes; for
 * escaped descriptors it is past live_ofs[]. */
Caml_inline unsigned char *frame_end_of_live_ofs(frame_descr *d) {
  if (frame_is_small(d)) {
    struct frame_descr_decoded dec;
    caml_decode_frame_descr(d, &dec);
    return (unsigned char *)dec.end_of_live;
  }
  if (frame_is_long(d)) {
    uint32_t num_live = caml_read_unaligned_uint32(d + Frame_long_num_live_ofs);
    return d + Frame_long_live_ofs + (uintnat)num_live * sizeof(uint32_t);
  } else {
    uint16_t num_live = caml_read_unaligned_uint16(d + Frame_num_live_ofs);
    return d + Frame_live_ofs + (uintnat)num_live * sizeof(uint16_t);
  }
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
  ((uintnat)(d) + (uintnat)(intnat)caml_read_unaligned_int32(d))

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
