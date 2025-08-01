/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#ifndef CAML_MLVALUES_H
#define CAML_MLVALUES_H

#include "config.h"
#include "misc.h"
#include "tsan.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Definitions

  word: Four bytes on 32 and 16 bit architectures,
        eight bytes on 64 bit architectures.
  long: A C integer having the same number of bytes as a word.
  val: The ML representation of something.  A long or a block or a pointer
       outside the heap.  If it is a block, it is the (encoded) address
       of an object.  If it is a long, it is encoded as well.
  block: Something allocated.  It always has a header and some
          fields or some number of bytes (a multiple of the word size).
  field: A word-sized val which is part of a block.
  bp: Pointer to the first byte of a block.  (a char *)
  op: Pointer to the first field of a block.  (a value *)
  hp: Pointer to the header of a block.  (a char *)
  int32_t: Four bytes on all architectures.
  int64_t: Eight bytes on all architectures.

  Remark: A block size is always a multiple of the word size, and at least
          one word plus the header.

  bosize: Size (in bytes) of the "bytes" part.
  wosize: Size (in words) of the "fields" part.
  bhsize: Size (in bytes) of the block with its header.
  whsize: Size (in words) of the block with its header.

  hd: A header.
  tag: The value of the tag field of the header.
  color: The value of the color field of the header.
         This is for use only by the GC.
*/

typedef intnat value;
typedef uintnat header_t;
typedef header_t reserved_t;
typedef uintnat mlsize_t;
typedef unsigned int tag_t;             /* Actually, an unsigned char */
typedef uintnat color_t;
typedef uintnat mark_t;
typedef atomic_intnat atomic_value;
typedef volatile value * value_ptr;
typedef int32_t opcode_t;
typedef opcode_t * code_t;

#include "domain_state.h"

/* Or_null constructors. */

#define Val_null ((value) 0)
#define Val_this(v) (v)
#define This_val(v) (v)
#define Is_null(v) ((v) == Val_null)
#define Is_this(v) ((v) != Val_null)

/* Longs vs blocks. */

#if defined(__x86_64__) && defined(HAS_BMI)
// Specialize the implementation of Is_block and Is_long on x86-64.
//
// Is_block(x) returns 1 if the least significant bit of x is 0, and x != 0.
// Normally, that is translated into 4 assembly instructions.
//
// However, we can use TZCNT to compute Is_block(x) in just one instruction.
// TZCNT counts the number of trailing zeros in x, setting the carry flag
// to 1 if x == 0 and setting the zero flag to 1 if the LSB of x is 1.
// Therefore, Is_block(x) == 1 iff CF == 0 && ZF == 0.
// We discard the output register as unnecessary.
//
// Similarly, after TZCNT, Is_long(x) == 1 iff CF == 1 || ZF == 1.
//
// Unfortunately, we can't port this optimization to ARM, since CTZ
// there does not set any flags.
//
// On platforms prior to Haswell, TZCNT is not available and is silently
// interpreted as BSF, producing undefined results when x == 0.
// We don't have any CPUs with those architectures, so this seems fine.
Caml_inline int Is_block(value x) {
    int result;
    value never_used;
    __asm__ (
        "tzcnt %2, %1"
        : "=@cca" (result), "=r" (never_used)
        : "r" (x)
        : "cc"
    );
    return result;
}

Caml_inline int Is_long(value x) {
    int result;
    value never_used;
    __asm__ (
        "tzcnt %2, %1"
        : "=@ccbe" (result), "=r" (never_used)
        : "r" (x)
        : "cc"
    );
    return result;
}
#else
Caml_inline int Is_long(value x) {
  return ((x & 1) != 0 || x == 0);
}

Caml_inline int Is_block(value x) {
  return ((x & 1) == 0 && x != 0);
}
#endif

/* Conversion macro names are always of the form  "to_from". */
/* Example: Val_long as in "Val from long" or "Val of long". */
#define Val_long(x)     ((intnat) (((uintnat)(x) << 1)) + 1)
#define Long_val(x)     ((x) >> 1)
#define Max_long (((intnat)1 << (8 * sizeof(value) - 2)) - 1)
#define Min_long (-((intnat)1 << (8 * sizeof(value) - 2)))
#define Val_int(x) Val_long(x)
#define Int_val(x) ((int) Long_val(x))
#define Unsigned_long_val(x) ((uintnat)(x) >> 1)
#define Unsigned_int_val(x)  ((int) Unsigned_long_val(x))

/* Encoded exceptional return values, when functions are suffixed with
   _exn. Encoded exceptions are invalid values and must not be seen
   by the garbage collector. */
#define Make_exception_result(v) ((v) | 2)
#define Is_exception_result(v) (((v) & 3) == 2)
#define Extract_exception(v) ((v) & ~3)

/* Structure of the header:

For 16-bit and 32-bit architectures:
     +--------+-------+-----+
     | wosize | color | tag |
     +--------+-------+-----+
bits  31    10 9     8 7   0

For 64-bit architectures:

     +----------+--------+-------+-----+
     | reserved | wosize | color | tag |
     +----------+--------+-------+-----+
bits  63    64-R 63-R  10 9     8 7   0

where 0 <= R <= 31 is HEADER_RESERVED_BITS. R is always
set to 8 for the OxCaml compiler in order to support
mixed blocks. In the upstream compiler, R is set with the
--enable-reserved-header-bits=R argument.

*/

#define HEADER_BITS (sizeof(header_t) * CHAR_BIT)

#define HEADER_TAG_BITS 8
#define HEADER_TAG_MASK ((1ull << HEADER_TAG_BITS) - 1ull)

#define HEADER_COLOR_BITS 2
#define HEADER_COLOR_SHIFT HEADER_TAG_BITS
#define HEADER_COLOR_MASK (((1ull << HEADER_COLOR_BITS) - 1ull) \
                            << HEADER_COLOR_SHIFT)

#define HEADER_WOSIZE_BITS (HEADER_BITS - HEADER_TAG_BITS \
                            - HEADER_COLOR_BITS - HEADER_RESERVED_BITS)
#define HEADER_WOSIZE_SHIFT (HEADER_COLOR_SHIFT  + HEADER_COLOR_BITS)
#define HEADER_WOSIZE_MASK (((1ull << HEADER_WOSIZE_BITS) - 1ull) \
                             << HEADER_WOSIZE_SHIFT)

#define Tag_hd(hd) ((tag_t) ((hd) & HEADER_TAG_MASK))
#define Hd_with_tag(hd, tag) (((hd) &~ HEADER_TAG_MASK) | (tag))
#define Allocated_wosize_hd(hd) ((mlsize_t) (((hd) & HEADER_WOSIZE_MASK) \
                                     >> HEADER_WOSIZE_SHIFT))

/* A "clean" header, without reserved or color bits. */
#define Cleanhd_hd(hd) (((header_t)(hd)) & \
                        (HEADER_TAG_MASK | HEADER_WOSIZE_MASK))

#if HEADER_RESERVED_BITS > 0

#define HEADER_RESERVED_SHIFT (HEADER_BITS - HEADER_RESERVED_BITS)
#define Reserved_hd(hd)   (((header_t) (hd)) >> HEADER_RESERVED_SHIFT)
#define Hd_reserved(res)  ((header_t)(res) << HEADER_RESERVED_SHIFT)

#else /* HEADER_RESERVED_BITS is 0 */

#define Reserved_hd(hd)   ((reserved_t)0)
#define Hd_reserved(res)  ((header_t)0)

#endif


/* Header bits reserved for mixed blocks */

#define Is_mixed_block_reserved(res) (((reserved_t)(res)) > 0)

/* Native code versions of mixed block macros.

   The scannable size of a block is how many fields are values as opposed
   to flat floats/ints/etc. This is different than the (normal) size of a
   block for mixed blocks.

   The runtime has several functions that traverse over the structure of
   an OCaml value. (e.g. polymorphic comparison, GC marking/sweeping)
   All of these traversals must be written to have one of the following
   properties:
   - it's known that the input can never be a mixed block,
   - it raises an exception on mixed blocks, or
   - it uses the scannable size (not the normal size) to figure out which
   fields to recursively descend into.

   Otherwise, the traversal could attempt to recursively descend into
   a flat field, which could segfault (or worse).
*/

#define Scannable_wosize_val_native(val) (Scannable_wosize_hd (Hd_val (val)))
#define Reserved_mixed_block_scannable_wosize_native(sz)  (((mlsize_t)(sz)) + 1)
#define Mixed_block_scannable_wosize_reserved_native(res) \
  (((reserved_t)(res)) - 1)

Caml_inline mlsize_t Scannable_wosize_reserved_native(reserved_t res,
                                                      mlsize_t sz) {
  return
    Is_mixed_block_reserved(res)
    ? Mixed_block_scannable_wosize_reserved_native(res)
    : sz;
}

Caml_inline mlsize_t Scannable_wosize_hd_native(header_t hd) {
  reserved_t res = Reserved_hd(hd);
  return
    Is_mixed_block_reserved(res)
    ? Mixed_block_scannable_wosize_reserved_native(res)
    : Allocated_wosize_hd(hd);
}

/* Bytecode versions of mixed block macros.

   Bytecode always uses the size of the block as the scannable size. That's
   because bytecode doesn't represent mixed records as mixed blocks. They're
   "faux mixed blocks", which are regular blocks with a sentinel value set
   in the header bits.
*/

#define Scannable_wosize_hd_byte(hd)  (Allocated_wosize_hd (hd))
#define Scannable_wosize_val_byte(val) (Allocated_wosize_hd (Hd_val (val)))
Caml_inline mlsize_t Scannable_wosize_reserved_byte(reserved_t res,
                                                    mlsize_t size) {
  (void)res;
  return size;
}

/* Users should specify whether they want to use the bytecode or native
   versions of these macros. Internally to the runtime, the NATIVE_CODE
   macro lets us make that determination, so we can define suffixless
   versions of the mixed block macros.
*/
#ifdef CAML_INTERNALS
#ifdef NATIVE_CODE
#define Scannable_wosize_reserved(r, s) Scannable_wosize_reserved_native(r, s)
#define Scannable_wosize_hd(hd)         Scannable_wosize_hd_native(hd)
#define Scannable_wosize_val(val)       Scannable_wosize_val_native(val)
#else
#define Faux_mixed_block_sentinel ((reserved_t) 0xff)
#define Scannable_wosize_reserved(r, s) Scannable_wosize_reserved_byte(r, s)
#define Scannable_wosize_hd(hd)         Scannable_wosize_hd_byte(hd)
#define Scannable_wosize_val(val)       Scannable_wosize_val_byte(val)
#endif // NATIVE_CODE
#endif // CAML_INTERNALS

/* Color values are pre-shifted */

#define Color_hd(hd) ((hd) & HEADER_COLOR_MASK)
#define Hd_with_color(hd, color) (((hd) &~ HEADER_COLOR_MASK) | (color))

#define Hp_atomic_val(val) ((atomic_uintnat *)(val) - 1)
CAMLno_tsan_for_perf Caml_inline header_t Hd_val(value val)
{
  return atomic_load_explicit(Hp_atomic_val(val), memory_order_relaxed);
}

#define Color_val(val) (Color_hd (Hd_val (val)))

#define Hd_hp(hp) (* ((volatile header_t *) (hp)))      /* Also an l-value. */
#define Hp_val(val) (((volatile header_t *) (val)) - 1)
#define Hp_op(op) (Hp_val (op))
#define Hp_bp(bp) (Hp_val (bp))
#define Val_op(op) ((value) (op))
#define Val_hp(hp) ((value) (((header_t *) (hp)) + 1))
#define Op_hp(hp) ((volatile value *) Val_hp (hp))
#define Bp_hp(hp) ((char *) Val_hp (hp))

#define Num_tags (1ull << HEADER_TAG_BITS)
#define Max_wosize ((1ull << HEADER_WOSIZE_BITS) - 1ull)

#define Whsize_wosize(sz) ((sz) + 1)
#define Wosize_whsize(sz) ((sz) - 1)
#define Wosize_bhsize(sz) ((sz) / sizeof (value) - 1)
#define Bsize_wsize(sz) ((sz) * sizeof (value))
#define Wsize_bsize(sz) ((sz) / sizeof (value))
#define Bhsize_wosize(sz) (Bsize_wsize (Whsize_wosize (sz)))
#define Bhsize_bosize(sz) ((sz) + sizeof (header_t))

/* Note that Allocated_wosize_val and the other macros that read headers will not
   be optimized by common subexpression elimination, because of the
   atomic header loads.  It is best to bind the results of such macros
   to variables if they will be tested repeatedly, e.g. as the end condition
   in a for-loop.
*/

/* oxcaml: We rename the size macros to [Allocated_...] so that we're
   forced to think about whether C code needs to updated for mixed blocks, which
   have separate notions of scannable size and total size of an object, even for
   scannable tags. We call an object's size (including possibly non-scannable
   fields) its "allocated" size to document the fact that you shouldn't scan
   fields on the basis of this size alone.
 */

#define Allocated_wosize_val(val) (Allocated_wosize_hd (Hd_val (val)))
#define Allocated_wosize_op(op) (Allocated_wosize_val (op))
#define Allocated_wosize_bp(bp) (Allocated_wosize_val (bp))
#define Allocated_wosize_hp(hp) (Allocated_wosize_hd (Hd_hp (hp)))
#define Allocated_bosize_val(val) (Bsize_wsize (Allocated_wosize_val (val)))
#define Allocated_bosize_op(op) (Allocated_bosize_val (Val_op (op)))
#define Allocated_bosize_bp(bp) (Allocated_bosize_val (Val_bp (bp)))
#define Allocated_bosize_hd(hd) (Bsize_wsize (Allocated_wosize_hd (hd)))
#define Allocated_whsize_hp(hp) (Whsize_wosize (Allocated_wosize_hp (hp)))
#define Allocated_whsize_val(val) (Allocated_whsize_hp (Hp_val (val)))
#define Allocated_whsize_bp(bp) (Allocated_whsize_val (Val_bp (bp)))
#define Allocated_whsize_hd(hd) (Whsize_wosize (Allocated_wosize_hd (hd)))
#define Allocated_bhsize_hp(hp) (Bsize_wsize (Allocated_whsize_hp (hp)))
#define Allocated_bhsize_hd(hd) (Bsize_wsize (Allocated_whsize_hd (hd)))

#ifndef Hide_upstream_size_macros
#define Wosize_hd(hd)   Allocated_wosize_hd(hd)
#define Wosize_val(val) Allocated_wosize_val(val)
#define Wosize_op(op)   Allocated_wosize_op(op)
#define Wosize_bp(bp)   Allocated_wosize_bp(bp)
#define Wosize_hp(hp)   Allocated_wosize_hp(hp)
#define Bosize_val(val) Allocated_bosize_val(val)
#define Bosize_op(op)   Allocated_bosize_op(op)
#define Bosize_bp(bp)   Allocated_bosize_bp(bp)
#define Bosize_hd(hd)   Allocated_bosize_hd(hd)
#define Whsize_hp(hp)   Allocated_whsize_hp(hp)
#define Whsize_val(val) Allocated_whsize_val(val)
#define Whsize_bp(bp)   Allocated_whsize_bp(bp)
#define Whsize_hd(hd)   Allocated_whsize_hd(hd)
#define Bhsize_hp(hp)   Allocated_bhsize_hp(hp)
#define Bhsize_hd(hd)   Allocated_bhsize_hd(hd)
#endif // Hide_upstream_size_macros

#define Reserved_val(val) (Reserved_hd (Hd_val (val)))

#ifdef ARCH_BIG_ENDIAN
#define Tag_val(val) (((volatile unsigned char *) (val)) [-1])
                                                 /* Also an l-value. */
#define Tag_hp(hp) (((volatile unsigned char *) (hp)) [sizeof(value)-1])
                                                 /* Also an l-value. */
#else
#define Tag_val(val) (((volatile unsigned char *) (val)) [- (int)sizeof(value)])
                                                 /* Also an l-value. */
#define Tag_hp(hp) (((volatile unsigned char *) (hp)) [0])
                                                 /* Also an l-value. */
#endif

#define Unsafe_store_tag_val(dst, val) (Tag_val(dst) = val)
/* Currently [Tag_val(dst)] is an lvalue, but in the future we may
   have to break this property by using explicit (relaxed) atomics to
   avoid undefined behaviors. [Unsafe_store_tag_val(dst, val)] is
   provided to avoid direct uses of [Tag_val(dst)] on the left of an
   assignment. The use of [Unsafe] emphasizes that the function
   may result in unsafe data races in a concurrent setting. */

/* The lowest tag for blocks containing no value. */
#define No_scan_tag 251


/* 1- If tag < No_scan_tag : a tuple of fields.  */

/* Pointer to the first field. */
#define Op_val(x) ((value *) (x))
#define Op_atomic_val(x) ((atomic_value *) (x))
/* Fields are numbered from 0. */
#define Field(x, i) (((volatile value *)(x)) [i]) /* Also an l-value. */

/* NOTE: [Forward_tag] and [Infix_tag] must be just under
   [No_scan_tag], with [Infix_tag] the lower one.
   See [oldify_one] in minor_gc.c for more details.

   NOTE: Update stdlib/obj.ml whenever you change the tags.
 */

/* Forward_tag: forwarding pointer that the GC may silently shortcut.
   See stdlib/lazy.ml. */
#define Forward_tag 250
#define Forward_val(v) Field(v, 0)
/* FIXME: not immutable once shortcutting is implemented */

/* If tag == Infix_tag : an infix header inside a closure */
/* Infix_tag must be odd so that the infix header is scanned as an integer */
/* Infix_tag must be 1 modulo 2 and infix headers can only occur in blocks
   with tag Closure_tag (see compact.c). */

#define Infix_tag 249
#define Infix_offset_hd(hd) (Allocated_bosize_hd(hd))
#define Infix_offset_val(v) Infix_offset_hd(Hd_val(v))

/* Another special case: objects */
#define Object_tag 248
#define Class_val(val) Field((val), 0)
#define Oid_val(val) Long_val(Field((val), 1))
CAMLextern value caml_get_public_method (value obj, value tag);
/* Called as:
   caml_callback(caml_get_public_method(obj, caml_hash_variant(name)), obj) */
/* caml_get_public_method returns 0 if tag not in the table.
   Note however that tags being hashed, same tag does not necessarily mean
   same method name. */

Caml_inline value Val_ptr(void* p)
{
  CAMLassert(((value)p & 1) == 0);
  return (value)p + 1;
}
Caml_inline void* Ptr_val(value val)
{
  CAMLassert(val & 1);
  return (void*)(val - 1);
}

/* Special case of tuples of fields: closures */
#define Closure_tag 247
#define Code_val(val) (((code_t *) (val)) [0])     /* Also an l-value. */
#define Closinfo_val(val) Field((val), 1)          /* Arity and start env */
/* In the closure info field, the top 8 bits are the arity (signed).
   The next least significant bit is set iff the current closure is the
   last one to occur in the block.  (This is used in the compactor.)
   The low bit is set to one, to look like an integer.
   The remaining bits are the field number for the first word of the scannable
   part of the environment, or, in other words, the offset (in words) from the
   closure to the scannable part of the environment.
   The non-scannable part of the environment lives between the end of the
   last closure and the start of the scannable environment within the block. */
/* CR ncourant: it might be cleaner to use a packed struct here */
#ifdef ARCH_SIXTYFOUR
#define Arity_closinfo(info) ((intnat)(info) >> 56)
#define Start_env_closinfo(info) (((uintnat)(info) << 9) >> 10)
#define Is_last_closinfo(info) (((uintnat)(info) << 8) >> 63)
#define Make_closinfo(arity,delta,is_last) \
  (((uintnat)(arity) << 56) + ((uintnat)(is_last) << 55) \
    + ((uintnat)(delta) << 1) + 1)
#else
#define Arity_closinfo(info) ((intnat)(info) >> 24)
#define Start_env_closinfo(info) (((uintnat)(info) << 9) >> 10)
#define Is_last_closinfo(info) (((uintnat)(info) << 8) >> 31)
#define Make_closinfo(arity,delta,is_last) \
  (((uintnat)(arity) << 24) + ((uintnat)(is_last) << 23) \
    + ((uintnat)(delta) << 1) + 1)
#endif

/* This tag is used (with Forcing_tag & Forward_tag) to implement lazy values.
   See major_gc.c and stdlib/lazy.ml. */
#define Lazy_tag 246

/* Tag used for continuations (see fiber.c) */
#define Cont_tag 245

/* This tag is used (with Lazy_tag & Forward_tag) to implement lazy values.
 * See major_gc.c and stdlib/lazy.ml. */
#define Forcing_tag 244

/* Another special case: variants */
CAMLextern value caml_hash_variant(char const * tag);

/* 2- If tag >= No_scan_tag : a sequence of bytes. */

/* Pointer to the first byte */
#define Bp_val(v) ((char *) (v))
#define Val_bp(p) ((value) (p))
/* Bytes are numbered from 0. */
#define Byte(x, i) (((char *) (x)) [i])            /* Also an l-value. */
#define Byte_u(x, i) (((unsigned char *) (x)) [i]) /* Also an l-value. */

/* Abstract things.  Their contents is not traced by the GC; therefore
   they must not contain any [value].
*/
#define Abstract_tag 251
#define Data_abstract_val(v) ((void*) Op_val(v))

/* Strings. */
#define String_tag 252
#define String_val(x) ((const char *) Bp_val(x))
#define Bytes_val(x) ((unsigned char *) Bp_val(x))
CAMLextern mlsize_t caml_string_length (value);   /* size in bytes */
CAMLextern int caml_string_is_c_safe (value);
  /* true if string contains no '\0' null characters */

/* Floating-point numbers. */
#define Double_tag 253
#define Double_wosize ((sizeof(double) / sizeof(value)))
#ifndef ARCH_ALIGN_DOUBLE
#define Double_val(v) (* (double *)(v))
#define Store_double_val(v,d) (* (double *)(v) = (d))
#else
CAMLextern double caml_Double_val (value);
CAMLextern void caml_Store_double_val (value,double);
#define Double_val(v) caml_Double_val(v)
#define Store_double_val(v,d) caml_Store_double_val(v,d)
#endif

/* Arrays of floating-point numbers. */
#define Double_array_tag 254

/* Unboxed array tags (for mixed blocks) 
   These must stay in sync with Cmm_helpers.Unboxed_array_tags */
#define Unboxed_product_array_tag 0
#define Unboxed_int64_array_tag 1
#define Unboxed_int32_array_even_tag 2
#define Unboxed_int32_array_odd_tag 3
#define Unboxed_float32_array_even_tag 4
#define Unboxed_float32_array_odd_tag 5
#define Unboxed_vec128_array_tag 6
#define Unboxed_vec256_array_tag 7
#define Unboxed_vec512_array_tag 8
#define Unboxed_nativeint_array_tag 9

/* The [_flat_field] macros are for [floatarray] values and float-only records.
*/
#define Double_flat_field(v,i) Double_val((value)((volatile double *)(v) + (i)))
#define Store_double_flat_field(v,i,d) do{ \
  mlsize_t caml__temp_i = (i); \
  double caml__temp_d = (d); \
  Store_double_val((value)((double *) (v) + caml__temp_i), caml__temp_d); \
}while(0)

/* The [_array_field] macros are for [float array]. */
#ifdef FLAT_FLOAT_ARRAY
  #define Double_array_field(v,i) Double_flat_field(v,i)
  #define Store_double_array_field(v,i,d) Store_double_flat_field(v,i,d)
#else
  #define Double_array_field(v,i) Double_val (Field(v,i))
  CAMLextern void caml_Store_double_array_field (value, mlsize_t, double);
  #define Store_double_array_field(v,i,d) caml_Store_double_array_field (v,i,d)
#endif

/* The old [_field] macros are for backward compatibility only.
   They work with [floatarray], float-only records, and [float array]. */
#ifdef FLAT_FLOAT_ARRAY
  #define Double_field(v,i) Double_flat_field(v,i)
  #define Store_double_field(v,i,d) Store_double_flat_field(v,i,d)
#else
  Caml_inline double Double_field (value v, mlsize_t i) {
    if (Tag_val (v) == Double_array_tag){
      return Double_flat_field (v, i);
    }else{
      return Double_array_field (v, i);
    }
  }
  Caml_inline void Store_double_field (value v, mlsize_t i, double d) {
    if (Tag_val (v) == Double_array_tag){
      Store_double_flat_field (v, i, d);
    }else{
      Store_double_array_field (v, i, d);
    }
  }
#endif /* FLAT_FLOAT_ARRAY */

CAMLextern mlsize_t caml_array_length (value);   /* size in items */
CAMLextern int caml_is_double_array (value);   /* 0 is false, 1 is true */


/* Custom blocks.  They contain a pointer to a "method suite"
   of functions (for finalization, comparison, hashing, etc)
   followed by raw data.  The contents of custom blocks is not traced by
   the GC; therefore, they must not contain any [value].
   See [custom.h] for operations on method suites. */
#define Custom_tag 255
#define Data_custom_val(v) ((void *) (Op_val(v) + 1))
#define Custom_val_data(d) (Val_op((value *)d - 1))
struct custom_operations;       /* defined in [custom.h] */

/* Int32.t, Int64.t and Nativeint.t are represented as custom blocks. */

#define Int32_val(v) (*((int32_t *) Data_custom_val(v)))
#define Nativeint_val(v) (*((intnat *) Data_custom_val(v)))
#ifndef ARCH_ALIGN_INT64
#define Int64_val(v) (*((int64_t *) Data_custom_val(v)))
#else
CAMLextern int64_t caml_Int64_val(value v);
#define Int64_val(v) caml_Int64_val(v)
#endif

/* 3- Atoms are 0-tuples.  They are statically allocated once and for all. */

CAMLextern value caml_atom(tag_t);
#define Atom(tag) caml_atom(tag)

/* Booleans are integers 0 or 1 */

#define Val_bool(x) Val_int((x) != 0)
#ifdef __cplusplus
#define Bool_val(x) ((bool) Int_val(x))
#else
#define Bool_val(x) Int_val(x)
#endif
#define Val_false Val_int(0)
#define Val_true Val_int(1)
#define Val_not(x) (Val_false + Val_true - (x))

/* The unit value is 0 (tagged) */

#define Val_unit Val_int(0)

/* List constructors */
#define Val_emptylist Val_int(0)
#define Tag_cons 0

/* Option constructors */

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
#define Tag_some 0
#define Is_none(v) ((v) == Val_none)
#define Is_some(v) Is_block(v)

CAMLextern value caml_set_oo_id(value obj);

/* Users write this to assert that the ensuing C code is sensitive
   to the current layout of mixed blocks in a way that's subject
   to change in future compiler releases. We'll bump the version
   number when we make a breaking change. For example, we currently
   don't pack int32's efficiently, and we will want to someday.

   Users can write:

   Assert_mixed_block_layout_v1;

   (Hack: we define using _Static_assert rather than just an empty
   definition so that users can write a semicolon, which is treated
   better by C formatters.)
 */
#define Assert_mixed_block_layout_v1 _Static_assert(0, "")
#define Assert_mixed_block_layout_v2 _Static_assert(0, "")
#define Assert_mixed_block_layout_v3 _Static_assert(0, "")
#define Assert_mixed_block_layout_v4 _Static_assert(1, "")

/* Header for out-of-heap blocks. */

#define Caml_out_of_heap_header_with_reserved(wosize, tag, reserved)   \
      (/*CAMLassert ((wosize) <= Max_wosize),*/                        \
       ((header_t) (Hd_reserved(reserved))                             \
                    + ((header_t) (wosize) << HEADER_WOSIZE_SHIFT)     \
                    + (3 << HEADER_COLOR_SHIFT) /* [NOT_MARKABLE] */   \
                    + (tag_t) (tag)))

#define Caml_out_of_heap_header(wosize, tag)                           \
        Caml_out_of_heap_header_with_reserved(wosize, tag, 0)

#ifdef __cplusplus
}
#endif

#endif /* CAML_MLVALUES_H */
