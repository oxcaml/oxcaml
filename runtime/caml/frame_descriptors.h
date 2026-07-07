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
 *   be allocated by this call. Each size is stored reduced by 1.
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
 * Within a frametable, descriptors are concatenated in increasing
 * return-address order.
 *
 * There are three descriptor layouts: "short", "medium", and "long".
 *
 * Almost all descriptors are "short". A short descriptor is preceded
 * by a "delta" field encoding the difference between this
 * descriptor's return address and the previous one's. The delta is an
 * unsigned LEB128 value, >= 1.  The return address is the previous
 * descriptor's plus the delta. We use LEB128 as it is efficient and
 * supported by the assemblers.
 *
 * Any descriptor not fitting the constraints of the "short" layout
 * begins with a zero byte (FRAME_DELTA_ESCAPE). Since a zero "delta"
 * is impossible, this marks it as non-short ("medium" or "long", see
 * below). This includes the first descriptor of any frametable (no
 * previous return address), and the first descriptor in any section.
 *
 * "Short" layout:
 *
 * A [frame_descr *] for a short descriptor points to the byte _after_
 * the delta, i.e. at the size+flags byte, which is never zero. A
 * descriptor is thus classified by its first byte: zero (the escape
 * byte) means medium/long, non-zero means short. (The _preceding_
 * byte cannot be used for this: an assembler may pad a ULEB128 delta
 * to a non-minimal encoding whose last byte is zero, as LLVM's MC
 * does when the delta spans an alignment boundary.) A
 * short-descriptor body is:
 *
 *   (1) size+flags byte: top 6 bits = frame size in 16-byte units:
 *       value 1..63 means 16..1008 bytes, and is never zero. Bottom
 *       two bits are the flags (FRAME_DESCRIPTOR_ALLOC | _DEBUG).
 *
 *   (2) if alloc:
 *          uint8  reg_bitmap;  bit i set => register HOT_REGS[i] is live
 *          uint8  num_allocs;
 *          uint8  alloc_sizes[ceil(num_allocs/2)];  4 bits each
 *                 (wosize-1, so 1..16 words), low nibble first
 *
 *   (3) uint8 stack_bitmap[ceil(frame_words / 8)]; a live-slot bitmap
 *       of the frame identifying live scannable slots. LSB-first
 *       within each byte.
 *
 *   (4) if debug: uint32_t debug_info[alloc ? num_allocs : 1]; as for
 *       the medium/long layouts.
 *
 * "Medium" and "Long" layouts:
 *
 * A [frame_descr *] points at the ESCAPE byte itself, so its first
 * byte is zero; the field offsets below include that byte. The
 * descriptor is laid out as either "medium" or "long" as follows;
 * the field at offset 5 tells them apart (a uint16 equal to
 * FRAME_LONG_MARKER means long):
 *
 *  medium:                      long:
 *    uint8  escape       @ 0      uint8  escape       @ 0: 0
 *    int32  retaddr_rel  @ 1      int32  retaddr_rel  @ 1
 *    uint16 frame_data   @ 5      uint16 marker       @ 5: FRAME_LONG_MARKER
 *    uint16 num_live     @ 7      uint16 _pad         @ 7
 *    uint16 live_ofs[]   @ 9      uint32 frame_data   @ 9
 *                                 uint32 num_live     @ 13
 *                                 uint32 live_ofs[]   @ 17
 *
 * retaddr_rel is the byte offset from the retaddr_rel field itself
 * to the return address (the assembler computes [.Lret - .]).
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

#define FRAME_DELTA_ESCAPE 0

/* The eight most common GC-root registers. The compiler
 * (frame_hot_regs in backend/<arch>/arch.ml) and the runtime must
 * agree on this table: a mismatch causes heap corruption. The GC maps
 * bit-index -> register-number (this forward table); the compiler
 * maps register-number -> bit-index.
 *
 * The amd64 set was chosen by measurement; the arm64 set (x0-x7) is an
 * initial guess pending measurement (a suboptimal choice only costs
 * escaped descriptors). */

#define FRAME_NUM_HOT_REGS 8
CAMLunused static const unsigned char
  caml_frame_hot_regs[FRAME_NUM_HOT_REGS] =
#ifdef TARGET_arm64
  { 0, 1, 2, 3, 4, 5, 6, 7 };
#else
  { 0, 1, 2, 3, 4, 5, 6, 8 };
#endif

/* Field byte offsets within an escaped descriptor (medium and long),
   counted from the descriptor pointer at the escape byte. */
#define Frame_retaddr_rel_ofs   1
#define Frame_data_ofs          5
#define Frame_num_live_ofs      7
#define Frame_live_ofs          9
#define Frame_long_data_ofs     9
#define Frame_long_num_live_ofs 13
#define Frame_long_live_ofs     17

Caml_inline bool frame_is_short(frame_descr *d) {
  return ((const unsigned char *)d)[0] != FRAME_DELTA_ESCAPE;
}

/* The size+flags byte of a short descriptor. */
Caml_inline unsigned char frame_short_sizeflags(frame_descr *d) {
  return ((const unsigned char *)d)[0];
}

/* Decoded form of one descriptor (short or escaped); see
 * caml_decode_frame_descr. */
struct frame_descr_decoded {
  bool is_short;
  bool return_to_C;
  bool is_long;
  bool has_allocs;
  bool has_debug;
  uint32_t frame_size;   /* in bytes */
  uint32_t num_live;     /* escaped only: number of live_ofs[] entries */
  uint8_t num_allocs;    /* if has_allocs: number of allocations */
  uint8_t num_debuginfo; /* debug words: num_allocs, or 1 without allocs */
  /* For short descriptors only: */
  const unsigned char *short_allocs;  /* alloc_sizes[] (4-bit nibbles) */
  uint8_t short_reg_bitmap;
  const unsigned char *short_live;    /* live-slot bitmap over the frame */
  uint8_t short_live_bytes;           /* its width: ceil(frame_words / 8) */
  /* The byte just past the live_ofs / stack offsets, i.e. where the
   * alloc-count (escaped) or the debug words begin. */
  const unsigned char *end_of_live;
  /* One past the whole descriptor body (start of the next delta). */
  const unsigned char *end;
};

void caml_decode_frame_descr(frame_descr *d, struct frame_descr_decoded *out);

Caml_inline bool frame_return_to_C(frame_descr *d) {
  if (frame_is_short(d)) return false;
  return caml_read_unaligned_uint16(d + Frame_data_ofs) == FRAME_RETURN_TO_C;
}

Caml_inline bool frame_is_long(frame_descr *d) {
  CAMLassert(d && !frame_return_to_C(d));
  if (frame_is_short(d)) return false;
  return (caml_read_unaligned_uint16(d + Frame_data_ofs) == FRAME_LONG_MARKER);
}

/* Frame size in bytes of a short descriptor, from its size+flags byte:
   top 6 bits hold the size in 16-byte units. */
Caml_inline uint32_t frame_short_size(frame_descr *d) {
  return ((uint32_t)(frame_short_sizeflags(d) >> 2)) * 16;
}

Caml_inline uint32_t frame_data(frame_descr *d) {
  if (frame_is_short(d)) {
    /* Reconstruct an equivalent normal frame_data: size | flags. */
    return frame_short_size(d) | (frame_short_sizeflags(d) & FRAME_DESCRIPTOR_FLAGS);
  }
  if (frame_is_long(d)) {
    return caml_read_unaligned_uint32(d + Frame_long_data_ofs);
  } else {
    return caml_read_unaligned_uint16(d + Frame_data_ofs);
  }
}

Caml_inline uint32_t frame_size(frame_descr *d) {
  if (frame_is_short(d)) return frame_short_size(d);
  return frame_data(d) &~ FRAME_DESCRIPTOR_FLAGS;
}

Caml_inline bool frame_has_allocs(frame_descr *d) {
  if (frame_is_short(d))
    return (frame_short_sizeflags(d) & FRAME_DESCRIPTOR_ALLOC) != 0;
  return (frame_data(d) & FRAME_DESCRIPTOR_ALLOC) != 0;
}

Caml_inline bool frame_has_debug(frame_descr *d) {
  if (frame_is_short(d))
    return (frame_short_sizeflags(d) & FRAME_DESCRIPTOR_DEBUG) != 0;
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
  ((uintnat)(d) + Frame_retaddr_rel_ofs \
   + (uintnat)(intnat)caml_read_unaligned_int32((d) + Frame_retaddr_rel_ofs))

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
