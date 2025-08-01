/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
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

/* Structured output */

/* The interface of this file is "caml/intext.h" */

#include <string.h>
#include "caml/alloc.h"
#include "caml/codefrag.h"
#include "caml/config.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/gc.h"
#include "caml/intext.h"
#include "caml/io.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/mlvalues.h"
#include "caml/reverse.h"

static uintnat obj_counter;  /* Number of objects emitted so far */
static uintnat size_32;  /* Size in words of 32-bit block for struct. */
static uintnat size_64;  /* Size in words of 64-bit block for struct. */

/* Flags affecting marshaling */

enum {
  NO_SHARING = 1,               /* Flag to ignore sharing */
  CLOSURES = 2,                 /* Flag to allow marshaling code pointers */
  COMPAT_32 = 4                 /* Flag to ensure that output can safely
                                   be read back on a 32-bit platform */
};

static int extern_flags;        /* logical or of some of the flags above */

/* Stack for pending values to marshal */

struct extern_item { value * v; mlsize_t count; };

#define EXTERN_STACK_INIT_SIZE 256
#define EXTERN_STACK_MAX_SIZE (1024*1024*100)

static struct extern_item extern_stack_init[EXTERN_STACK_INIT_SIZE];

static struct extern_item * extern_stack = extern_stack_init;
static struct extern_item * extern_stack_limit = extern_stack_init
                                                   + EXTERN_STACK_INIT_SIZE;

/* Hash table to record already-marshaled objects and their positions */

struct object_position { value obj; uintnat pos; };

/* The hash table uses open addressing, linear probing, and a redundant
   representation:
   - a bitvector [present] records which entries of the table are occupied;
   - an array [entries] records (object, position) pairs for the entries
     that are occupied.
   The bitvector is much smaller than the array (1/128th on 64-bit
   platforms, 1/64th on 32-bit platforms), so it has better locality,
   making it faster to determine that an object is not in the table.
   Also, it makes it faster to empty or initialize a table: only the
   [present] bitvector needs to be filled with zeros, the [entries]
   array can be left uninitialized.
*/

struct position_table {
  int shift;
  mlsize_t size;                    /* size == 1 << (wordsize - shift) */
  mlsize_t mask;                    /* mask == size - 1 */
  mlsize_t threshold;               /* threshold == a fixed fraction of size */
  uintnat * present;                /* [Bitvect_size(size)] */
  struct object_position * entries; /* [size]  */
};

#define Bits_word (8 * sizeof(uintnat))
#define Bitvect_size(n) (((n) + Bits_word - 1) / Bits_word)

#define POS_TABLE_INIT_SIZE_LOG2 8
#define POS_TABLE_INIT_SIZE (1 << POS_TABLE_INIT_SIZE_LOG2)

static uintnat pos_table_present_init[Bitvect_size(POS_TABLE_INIT_SIZE)];
static struct object_position pos_table_entries_init[POS_TABLE_INIT_SIZE];

static struct position_table pos_table;

/* Forward declarations */

CAMLnoreturn_start
static void extern_out_of_memory(void)
CAMLnoreturn_end;

CAMLnoreturn_start
static void extern_invalid_argument(char *msg)
CAMLnoreturn_end;

CAMLnoreturn_start
static void extern_failwith(char *msg)
CAMLnoreturn_end;

CAMLnoreturn_start
static void extern_stack_overflow(void)
CAMLnoreturn_end;

static void free_extern_output(void);

/* Free the extern stack if needed */
static void extern_free_stack(void)
{
  if (extern_stack != extern_stack_init) {
    caml_stat_free(extern_stack);
    /* Reinitialize the globals for next time around */
    extern_stack = extern_stack_init;
    extern_stack_limit = extern_stack + EXTERN_STACK_INIT_SIZE;
  }
}

static struct extern_item * extern_resize_stack(struct extern_item * sp)
{
  asize_t newsize = 2 * (extern_stack_limit - extern_stack);
  asize_t sp_offset = sp - extern_stack;
  struct extern_item * newstack;

  if (newsize >= EXTERN_STACK_MAX_SIZE) extern_stack_overflow();
  if (extern_stack == extern_stack_init) {
    newstack = caml_stat_alloc_noexc(sizeof(struct extern_item) * newsize);
    if (newstack == NULL) extern_stack_overflow();
    memcpy(newstack, extern_stack_init,
           sizeof(struct extern_item) * EXTERN_STACK_INIT_SIZE);
  } else {
    newstack = caml_stat_resize_noexc(extern_stack,
                                      sizeof(struct extern_item) * newsize);
    if (newstack == NULL) extern_stack_overflow();
  }
  extern_stack = newstack;
  extern_stack_limit = newstack + newsize;
  return newstack + sp_offset;
}

/* Multiplicative Fibonacci hashing
   (Knuth, TAOCP vol 3, section 6.4, page 518).
   HASH_FACTOR is (sqrt(5) - 1) / 2 * 2^wordsize. */
#ifdef ARCH_SIXTYFOUR
#define HASH_FACTOR 11400714819323198486UL
#else
#define HASH_FACTOR 2654435769UL
#endif
#define Hash(v) (((uintnat)(v) * HASH_FACTOR) >> pos_table.shift)

/* When the table becomes 2/3 full, its size is increased. */
#define Threshold(sz) (((sz) * 2) / 3)

/* Initialize the position table */

static void extern_init_position_table(void)
{
  if (extern_flags & NO_SHARING) return;
  pos_table.size = POS_TABLE_INIT_SIZE;
  pos_table.shift = 8 * sizeof(value) - POS_TABLE_INIT_SIZE_LOG2;
  pos_table.mask = POS_TABLE_INIT_SIZE - 1;
  pos_table.threshold = Threshold(POS_TABLE_INIT_SIZE);
  pos_table.present = pos_table_present_init;
  pos_table.entries = pos_table_entries_init;
  memset(pos_table_present_init, 0, sizeof(pos_table_present_init));
}

/* Free the position table */

static void extern_free_position_table(void)
{
  if (pos_table.present != pos_table_present_init) {
    caml_stat_free(pos_table.present);
    caml_stat_free(pos_table.entries);
    /* Protect against repeated calls to extern_free_position_table */
    pos_table.present = pos_table_present_init;
  }
}

/* Accessing bitvectors */

Caml_inline uintnat bitvect_test(uintnat * bv, uintnat i)
{
  return bv[i / Bits_word] & ((uintnat) 1 << (i & (Bits_word - 1)));
}

Caml_inline void bitvect_set(uintnat * bv, uintnat i)
{
  bv[i / Bits_word] |= ((uintnat) 1 << (i & (Bits_word - 1)));
}

/* Grow the position table */

static void extern_resize_position_table(void)
{
  mlsize_t new_size, new_byte_size;
  int new_shift;
  uintnat * new_present;
  struct object_position * new_entries;
  uintnat i, h;
  struct position_table old = pos_table;

  /* Grow the table quickly (x 8) up to 10^6 entries,
     more slowly (x 2) afterwards. */
  if (old.size < 1000000) {
    new_size = 8 * old.size;
    new_shift = old.shift - 3;
  } else {
    new_size = 2 * old.size;
    new_shift = old.shift - 1;
  }
  if (new_size == 0
      || caml_umul_overflow(new_size, sizeof(struct object_position),
                            &new_byte_size))
    extern_out_of_memory();
  new_entries = caml_stat_alloc_noexc(new_byte_size);
  if (new_entries == NULL) extern_out_of_memory();
  new_present =
    caml_stat_calloc_noexc(Bitvect_size(new_size), sizeof(uintnat));
  if (new_present == NULL) {
    caml_stat_free(new_entries);
    extern_out_of_memory();
  }
  pos_table.size = new_size;
  pos_table.shift = new_shift;
  pos_table.mask = new_size - 1;
  pos_table.threshold = Threshold(new_size);
  pos_table.present = new_present;
  pos_table.entries = new_entries;

  /* Insert every entry of the old table in the new table */
  for (i = 0; i < old.size; i++) {
    if (! bitvect_test(old.present, i)) continue;
    h = Hash(old.entries[i].obj);
    while (bitvect_test(new_present, h)) {
      h = (h + 1) & pos_table.mask;
    }
    bitvect_set(new_present, h);
    new_entries[h] = old.entries[i];
  }

  /* Free the old tables if not statically allocated */
  if (old.present != pos_table_present_init) {
    caml_stat_free(old.present);
    caml_stat_free(old.entries);
  }
}

/* Determine whether the given object [obj] is in the hash table.
   If so, set [*pos_out] to its position in the output and return 1.
   If not, return 0.
   Either way, set [*h_out] to the hash value appropriate for
   [extern_record_location]. */
Caml_inline int extern_lookup_position(value obj,
                                       uintnat * pos_out, uintnat * h_out)
{
  uintnat h = Hash(obj);
  while (1) {
    if (! bitvect_test(pos_table.present, h)) {
      *h_out = h;
      return 0;
    }
    if (pos_table.entries[h].obj == obj) {
      *h_out = h;
      *pos_out = pos_table.entries[h].pos;
      return 1;
    }
    h = (h + 1) & pos_table.mask;
  }
}

/* Record the given object [obj] in the hashmap, associated to the specified data [data]. */
/* The [h] parameter is the index in the hash table where the object
   must be inserted.  It was determined during lookup. */
static void extern_record_location_with_data(value obj, uintnat h, uintnat data)
{
  if (extern_flags & NO_SHARING) return;
  bitvect_set(pos_table.present, h);
  pos_table.entries[h].obj = obj;
  pos_table.entries[h].pos = data;
  obj_counter++;
  if (obj_counter >= pos_table.threshold) extern_resize_position_table();
}

/* Record the output position for the given object [obj]. */
/* The [h] parameter is the index in the hash table where the object
   must be inserted.  It was determined during lookup. */
static void extern_record_location(value obj, uintnat h)
{
  extern_record_location_with_data(obj, h, obj_counter);
}

/* Update the data associated with the given object [obj]. */
static void extern_update_location_with_data(uintnat h, uintnat data)
{
  if (extern_flags & NO_SHARING) return;
  pos_table.entries[h].pos = data;
}

/* To buffer the output */

static char * extern_userprovided_output;
static char * extern_ptr, * extern_limit;

struct output_block {
  struct output_block * next;
  char * end;
  char data[SIZE_EXTERN_OUTPUT_BLOCK];
};

static struct output_block * extern_output_first, * extern_output_block;

static void init_extern_output(void)
{
  extern_userprovided_output = NULL;
  extern_output_first = caml_stat_alloc_noexc(sizeof(struct output_block));
  if (extern_output_first == NULL) caml_raise_out_of_memory();
  extern_output_block = extern_output_first;
  extern_output_block->next = NULL;
  extern_ptr = extern_output_block->data;
  extern_limit = extern_output_block->data + SIZE_EXTERN_OUTPUT_BLOCK;
}

static void close_extern_output(void)
{
  if (extern_userprovided_output == NULL){
    extern_output_block->end = extern_ptr;
  }
}

static void free_extern_output(void)
{
  struct output_block * blk, * nextblk;

  if (extern_userprovided_output == NULL) {
    for (blk = extern_output_first; blk != NULL; blk = nextblk) {
      nextblk = blk->next;
      caml_stat_free(blk);
    }
    extern_output_first = NULL;
  }
  extern_free_stack();
  extern_free_position_table();
}

static void grow_extern_output(intnat required)
{
  struct output_block * blk;
  intnat extra;

  if (extern_userprovided_output != NULL) {
    extern_failwith("Marshal.to_buffer: buffer overflow");
  }
  extern_output_block->end = extern_ptr;
  if (required <= SIZE_EXTERN_OUTPUT_BLOCK / 2)
    extra = 0;
  else
    extra = required;
  blk = caml_stat_alloc_noexc(sizeof(struct output_block) + extra);
  if (blk == NULL) extern_out_of_memory();
  extern_output_block->next = blk;
  extern_output_block = blk;
  extern_output_block->next = NULL;
  extern_ptr = extern_output_block->data;
  extern_limit = extern_output_block->data + SIZE_EXTERN_OUTPUT_BLOCK + extra;
}

static intnat extern_output_length(void)
{
  struct output_block * blk;
  intnat len;

  if (extern_userprovided_output != NULL) {
    return extern_ptr - extern_userprovided_output;
  } else {
    for (len = 0, blk = extern_output_first; blk != NULL; blk = blk->next)
      len += blk->end - blk->data;
    return len;
  }
}

/* Exception raising, with cleanup */

static void extern_out_of_memory(void)
{
  free_extern_output();
  caml_raise_out_of_memory();
}

static void extern_invalid_argument(char *msg)
{
  free_extern_output();
  caml_invalid_argument(msg);
}

static void extern_failwith(char *msg)
{
  free_extern_output();
  caml_failwith(msg);
}

static void extern_stack_overflow(void)
{
  caml_gc_message (0x04, "Stack overflow in marshaling value\n");
  free_extern_output();
  caml_raise_out_of_memory();
}

/* Conversion to big-endian */

Caml_inline void store16(char * dst, int n)
{
  dst[0] = n >> 8;  dst[1] = n;
}

Caml_inline void store32(char * dst, intnat n)
{
  dst[0] = n >> 24;  dst[1] = n >> 16;  dst[2] = n >> 8;  dst[3] = n;
}

Caml_inline void store64(char * dst, int64_t n)
{
  dst[0] = n >> 56;  dst[1] = n >> 48;  dst[2] = n >> 40;  dst[3] = n >> 32;
  dst[4] = n >> 24;  dst[5] = n >> 16;  dst[6] = n >> 8;   dst[7] = n;
}

/* Write characters, integers, and blocks in the output buffer */

Caml_inline void write(int c)
{
  if (extern_ptr >= extern_limit) grow_extern_output(1);
  *extern_ptr++ = c;
}

static void writeblock(const char * data, intnat len)
{
  if (extern_ptr + len > extern_limit) grow_extern_output(len);
  memcpy(extern_ptr, data, len);
  extern_ptr += len;
}

Caml_inline void writeblock_float8(const double * data, intnat ndoubles)
{
#if ARCH_FLOAT_ENDIANNESS == 0x01234567 || ARCH_FLOAT_ENDIANNESS == 0x76543210
  writeblock((const char *) data, ndoubles * 8);
#else
  caml_serialize_block_float_8(data, ndoubles);
#endif
}

static void writecode8(int code, intnat val)
{
  if (extern_ptr + 2 > extern_limit) grow_extern_output(2);
  extern_ptr[0] = code;
  extern_ptr[1] = val;
  extern_ptr += 2;
}

static void writecode16(int code, intnat val)
{
  if (extern_ptr + 3 > extern_limit) grow_extern_output(3);
  extern_ptr[0] = code;
  store16(extern_ptr + 1, (int) val);
  extern_ptr += 3;
}

static void writecode32(int code, intnat val)
{
  if (extern_ptr + 5 > extern_limit) grow_extern_output(5);
  extern_ptr[0] = code;
  store32(extern_ptr + 1, val);
  extern_ptr += 5;
}

#ifdef ARCH_SIXTYFOUR
static void writecode64(int code, intnat val)
{
  if (extern_ptr + 9 > extern_limit) grow_extern_output(9);
  extern_ptr[0] = code;
  store64(extern_ptr + 1, val);
  extern_ptr += 9;
}
#endif

/* Marshaling integers */

Caml_inline void extern_int(intnat n)
{
  if (n >= 0 && n < 0x40) {
    write(PREFIX_SMALL_INT + n);
  } else if (n >= -(1 << 7) && n < (1 << 7)) {
    writecode8(CODE_INT8, n);
  } else if (n >= -(1 << 15) && n < (1 << 15)) {
    writecode16(CODE_INT16, n);
#ifdef ARCH_SIXTYFOUR
  } else if (n < -((intnat)1 << 30) || n >= ((intnat)1 << 30)) {
      if (extern_flags & COMPAT_32)
        extern_failwith("output_value: integer cannot be read back on "
                        "32-bit platform");
      writecode64(CODE_INT64, n);
#endif
  } else {
    writecode32(CODE_INT32, n);
  }
}

Caml_inline void extern_unboxed_int(intnat n)
{
  if (extern_flags & COMPAT_32)
    extern_failwith("output_value: cannot marshal unboxed values on 32 bit");

  writecode64(CODE_UNBOXED_INT64, n);
}

Caml_inline void extern_null(void)
{
  writecode8(CODE_NULL, 0);
}

/* Marshaling references to previously-marshaled blocks */

Caml_inline void extern_shared_reference(uintnat d)
{
  if (d < 0x100) {
    writecode8(CODE_SHARED8, d);
  } else if (d < 0x10000) {
    writecode16(CODE_SHARED16, d);
#ifdef ARCH_SIXTYFOUR
  } else if (d >= (uintnat)1 << 32) {
    writecode64(CODE_SHARED64, d);
#endif
  } else {
    writecode32(CODE_SHARED32, d);
  }
}

/* Marshaling block headers */

Caml_inline void extern_header(mlsize_t sz, tag_t tag)
{
  if (tag < 16 && sz < 8) {
    write(PREFIX_SMALL_BLOCK + tag + (sz << 4));
  } else {
    header_t hd = Make_header(sz, tag, Caml_white);
#ifdef ARCH_SIXTYFOUR
    if (sz > 0x3FFFFF && (extern_flags & COMPAT_32))
      extern_failwith("output_value: array cannot be read back on "
                      "32-bit platform");
    if (hd < (uintnat)1 << 32)
      writecode32(CODE_BLOCK32, hd);
    else
      writecode64(CODE_BLOCK64, hd);
#else
    writecode32(CODE_BLOCK32, hd);
#endif
  }
}

/* Marshaling strings */

Caml_inline void extern_string(value v, mlsize_t len)
{
  if (len < 0x20) {
    write(PREFIX_SMALL_STRING + len);
  } else if (len < 0x100) {
    writecode8(CODE_STRING8, len);
  } else {
#ifdef ARCH_SIXTYFOUR
    if (len > 0xFFFFFB && (extern_flags & COMPAT_32))
      extern_failwith("output_value: string cannot be read back on "
                      "32-bit platform");
    if (len < (uintnat)1 << 32)
      writecode32(CODE_STRING32, len);
    else
      writecode64(CODE_STRING64, len);
#else
    writecode32(CODE_STRING32, len);
#endif
  }
  writeblock(String_val(v), len);
}

/* Marshaling FP numbers */

Caml_inline void extern_double(value v)
{
  write(CODE_DOUBLE_NATIVE);
  writeblock_float8((double *) v, 1);
}

/* Marshaling FP arrays */

Caml_inline void extern_double_array(value v, mlsize_t nfloats)
{
  if (nfloats < 0x100) {
    writecode8(CODE_DOUBLE_ARRAY8_NATIVE, nfloats);
  } else {
#ifdef ARCH_SIXTYFOUR
    if (nfloats > 0x1FFFFF && (extern_flags & COMPAT_32))
      extern_failwith("output_value: float array cannot be read back on "
                      "32-bit platform");
    if (nfloats < (uintnat) 1 << 32)
      writecode32(CODE_DOUBLE_ARRAY32_NATIVE, nfloats);
    else
      writecode64(CODE_DOUBLE_ARRAY64_NATIVE, nfloats);
#else
    writecode32(CODE_DOUBLE_ARRAY32_NATIVE, nfloats);
#endif
  }
  writeblock_float8((double *) v, nfloats);
}

/* Marshaling custom blocks */

Caml_inline void extern_custom(value v,
                               /*out*/ uintnat * sz_32,
                               /*out*/ uintnat * sz_64)
{
  char * size_header;
  char const * ident = Custom_ops_val(v)->identifier;
  void (*serialize)(value v, uintnat * bsize_32, uintnat * bsize_64)
        = Custom_ops_val(v)->serialize;
  const struct custom_fixed_length* fixed_length
        = Custom_ops_val(v)->fixed_length;
  if (serialize == NULL)
    extern_invalid_argument("output_value: abstract value (Custom)");
  if (fixed_length == NULL) {
    write(CODE_CUSTOM_LEN);
    writeblock(ident, strlen(ident) + 1);
    /* Reserve 12 bytes for the lengths (sz_32 and sz_64). */
    if (extern_ptr + 12 >= extern_limit) grow_extern_output(12);
    size_header = extern_ptr;
    extern_ptr += 12;
    serialize(v, sz_32, sz_64);
    /* Store length before serialized block */
    store32(size_header, *sz_32);
    store64(size_header + 4, *sz_64);
  } else {
    write(CODE_CUSTOM_FIXED);
    writeblock(ident, strlen(ident) + 1);
    serialize(v, sz_32, sz_64);
        if (*sz_32 != fixed_length->bsize_32 ||
            *sz_64 != fixed_length->bsize_64)
          caml_fatal_error(
            "output_value: incorrect fixed sizes specified by %s",
            ident);
  }
}

/* Marshaling code pointers */

static void extern_code_pointer(char * codeptr)
{
  struct code_fragment * cf;
  const char * digest;

  cf = caml_find_code_fragment_by_pc(codeptr);
  if (cf != NULL) {
    if ((extern_flags & CLOSURES) == 0)
      extern_invalid_argument("output_value: functional value");
    digest = (const char *) caml_digest_of_code_fragment(cf);
    if (digest == NULL)
      extern_invalid_argument("output_value: private function");
    writecode32(CODE_CODEPOINTER, codeptr - cf->code_start);
    writeblock(digest, 16);
  } else {
    extern_invalid_argument("output_value: abstract value (outside heap)");
  }
}

/* Marshaling the non-scanned-environment part of closures */

Caml_inline mlsize_t extern_closure_up_to_env(value v)
{
  mlsize_t startenv, i;
  value info;

  startenv = Start_env_closinfo(Closinfo_val(v));
  i = 0;
  do {
    if (i > 0) {
      /* The padding before an infix header */
      while (Field(v, i) == Val_long(0)) {
        extern_int(0);
        i++;
      }
      /* The infix header */
      extern_int(Long_val(Field(v, i++)));
    }
    /* The default entry point */
    extern_code_pointer((char *) Field(v, i++));
    /* The closure info. */
    info = Field(v, i++);
    extern_int(Long_val(info));
    /* The direct entry point if arity is neither 0 nor 1 */
    if (Arity_closinfo(info) != 0 && Arity_closinfo(info) != 1) {
      extern_code_pointer((char *) Field(v, i++));
    }
  } while (!Is_last_closinfo(info));
  CAMLassert(i <= startenv);
  /* The non-scanned part of the environment */
  while (i < startenv) {
    extern_unboxed_int(Field(v, i++));
  }
  return startenv;
}

/* Marshal the given value in the output buffer */

static void extern_rec(value v)
{
  struct extern_item * sp;
  uintnat h = 0;
  uintnat pos = 0;

  extern_init_position_table();
  sp = extern_stack;

  while(1) {
  if (Is_null(v)) {
    extern_null();
  } else if (Is_long(v)) {
    extern_int(Long_val(v));
  }
  else if (! (Is_in_value_area(v))) {
    /* Naked pointer outside the heap: try to marshal it as a code pointer,
       otherwise fail. */
    extern_code_pointer((char *) v);
  }
  else {
    header_t hd = Hd_val(v);
    tag_t tag = Tag_hd(hd);
    mlsize_t sz = Wosize_hd(hd);
    reserved_t reserved = Reserved_hd(hd);
    if (Is_mixed_block_reserved(reserved)) {
      extern_invalid_argument("output_value: mixed block");
      break;
    }

    if (tag == Forward_tag) {
      value f = Forward_val (v);
      if (Is_block (f)
          && (!Is_in_value_area(f) || Tag_val (f) == Forward_tag
              || Tag_val (f) == Lazy_tag || Tag_val (f) == Forcing_tag
#ifdef FLAT_FLOAT_ARRAY
              || Tag_val (f) == Double_tag
#endif
              )){
        /* Do not short-circuit the pointer. */
      }else{
        v = f;
        continue;
      }
    }
    /* Atoms are treated specially for two reasons: they are not allocated
       in the externed block, and they are automatically shared. */
    if (sz == 0) {
      extern_header(0, tag);
      goto next_item;
    }
    /* Check if object already seen */
    if (! (extern_flags & NO_SHARING)) {
      if (extern_lookup_position(v, &pos, &h)) {
        extern_shared_reference(obj_counter - pos);
        goto next_item;
      }
    }
    /* Output the contents of the object */
    switch(tag) {
    case String_tag: {
      mlsize_t len = caml_string_length(v);
      extern_string(v, len);
      size_32 += 1 + (len + 4) / 4;
      size_64 += 1 + (len + 8) / 8;
      extern_record_location(v, h);
      break;
    }
    case Double_tag: {
      CAMLassert(sizeof(double) == 8);
      extern_double(v);
      size_32 += 1 + 2;
      size_64 += 1 + 1;
      extern_record_location(v, h);
      break;
    }
    case Double_array_tag: {
      mlsize_t nfloats;
      CAMLassert(sizeof(double) == 8);
      nfloats = Wosize_val(v) / Double_wosize;
      extern_double_array(v, nfloats);
      size_32 += 1 + nfloats * 2;
      size_64 += 1 + nfloats;
      extern_record_location(v, h);
      break;
    }
    case Abstract_tag:
      extern_invalid_argument("output_value: abstract value (Abstract)");
      break;
    case Infix_tag:
      writecode32(CODE_INFIXPOINTER, Infix_offset_hd(hd));
      v = v - Infix_offset_hd(hd); /* PR#5772 */
      continue;
    case Custom_tag: {
      uintnat sz_32, sz_64;
      extern_custom(v, &sz_32, &sz_64);
      size_32 += 2 + ((sz_32 + 3) >> 2);  /* header + ops + data */
      size_64 += 2 + ((sz_64 + 7) >> 3);
      extern_record_location(v, h);
      break;
    }
    case Closure_tag: {
      mlsize_t i;
      extern_header(sz, tag);
      size_32 += 1 + sz;
      size_64 += 1 + sz;
      extern_record_location(v, h);
      i = extern_closure_up_to_env(v);
      if (i >= sz) goto next_item;
      /* Remember that we still have to serialize fields i + 1 ... sz - 1 */
      if (i < sz - 1) {
        sp++;
        if (sp >= extern_stack_limit) sp = extern_resize_stack(sp);
        sp->v = &Field(v, i + 1);
        sp->count = sz - i - 1;
      }
      /* Continue serialization with the first environment field */
      v = Field(v, i);
      continue;
    }
    default: {
      extern_header(sz, tag);
      size_32 += 1 + sz;
      size_64 += 1 + sz;
      extern_record_location(v, h);
      /* Remember that we still have to serialize fields 1 ... sz - 1 */
      if (sz > 1) {
        sp++;
        if (sp >= extern_stack_limit) sp = extern_resize_stack(sp);
        sp->v = &Field(v, 1);
        sp->count = sz - 1;
      }
      /* Continue serialization with the first field */
      v = Field(v, 0);
      continue;
    }
    }
  }
  next_item:
    /* Pop one more item to marshal, if any */
    if (sp == extern_stack) {
        /* We are done.   Cleanup the stack and leave the function */
        extern_free_stack();
        extern_free_position_table();
        return;
    }
    v = *((sp->v)++);
    if (--(sp->count) == 0) sp--;
  }
  /* Never reached as function leaves with return */
}

static int extern_flag_values[] = { NO_SHARING, CLOSURES, COMPAT_32 };

static intnat extern_value(value v, value flags,
                           /*out*/ char header[32],
                           /*out*/ int * header_len)
{
  intnat res_len;
  /* Parse flag list */
  extern_flags = caml_convert_flag_list(flags, extern_flag_values);
  /* Initializations */
  obj_counter = 0;
  size_32 = 0;
  size_64 = 0;
  /* Marshal the object */
  extern_rec(v);
  /* Record end of output */
  close_extern_output();
  /* Write the header */
  res_len = extern_output_length();
#ifdef ARCH_SIXTYFOUR
  if (res_len >= ((intnat)1 << 32) ||
      size_32 >= ((intnat)1 << 32) || size_64 >= ((intnat)1 << 32)) {
    /* The object is too big for the small header format.
       Fail if we are in compat32 mode, or use big header. */
    if (extern_flags & COMPAT_32) {
      free_extern_output();
      caml_failwith("output_value: object too big to be read back on "
                    "32-bit platform");
    }
    store32(header, Intext_magic_number_big);
    store32(header + 4, 0);
    store64(header + 8, res_len);
    store64(header + 16, obj_counter);
    store64(header + 24, size_64);
    *header_len = 32;
    return res_len;
  }
#endif
  /* Use the small header format */
  store32(header, Intext_magic_number_small);
  store32(header + 4, res_len);
  store32(header + 8, obj_counter);
  store32(header + 12, size_32);
  store32(header + 16, size_64);
  *header_len = 20;
  return res_len;
}

void caml_output_val(struct channel *chan, value v, value flags)
{
  char header[32];
  int header_len;
  struct output_block * blk, * nextblk;

  if (! caml_channel_binary_mode(chan))
    caml_failwith("output_value: not a binary channel");
  init_extern_output();
  extern_value(v, flags, header, &header_len);
  /* During [caml_really_putblock], concurrent [caml_output_val] operations
     can take place (via signal handlers or context switching in systhreads),
     and [extern_output_first] may change. So, save it in a local variable. */
  blk = extern_output_first;
  caml_really_putblock(chan, header, header_len);
  while (blk != NULL) {
    caml_really_putblock(chan, blk->data, blk->end - blk->data);
    nextblk = blk->next;
    caml_stat_free(blk);
    blk = nextblk;
  }
  Flush_if_unbuffered(chan);
}

CAMLprim value caml_output_value(value vchan, value v, value flags)
{
  CAMLparam3 (vchan, v, flags);
  struct channel * channel = Channel(vchan);

  Lock(channel);
  caml_output_val(channel, v, flags);
  Unlock(channel);
  CAMLreturn (Val_unit);
}

CAMLprim value caml_output_value_to_bytes(value v, value flags)
{
  char header[32];
  int header_len;
  intnat data_len, ofs;
  value res;
  struct output_block * blk, * nextblk;

  init_extern_output();
  data_len = extern_value(v, flags, header, &header_len);
  /* PR#4030: it is prudent to save extern_output_first before allocating
     the result, as in caml_output_val */
  blk = extern_output_first;
  res = caml_alloc_string(header_len + data_len);
  ofs = 0;
  memcpy(&Byte(res, ofs), header, header_len);
  ofs += header_len;
  while (blk != NULL) {
    intnat n = blk->end - blk->data;
    memcpy(&Byte(res, ofs), blk->data, n);
    ofs += n;
    nextblk = blk->next;
    caml_stat_free(blk);
    blk = nextblk;
  }
  return res;
}

CAMLprim value caml_output_value_to_string(value v, value flags)
{
  return caml_output_value_to_bytes(v,flags);
}

CAMLexport intnat caml_output_value_to_block(value v, value flags,
                                             char * buf, intnat len)
{
  char header[32];
  int header_len;
  intnat data_len;
  /* At this point we don't know the size of the header.
     Guess that it is small, and fix up later if not. */
  extern_userprovided_output = buf + 20;
  extern_ptr = extern_userprovided_output;
  extern_limit = buf + len;
  data_len = extern_value(v, flags, header, &header_len);
  if (header_len != 20) {
    /* Bad guess!  Need to shift the output to make room for big header.
       Make sure there is room. */
    if (header_len + data_len > len)
      caml_failwith("Marshal.to_buffer: buffer overflow");
    memmove(buf + header_len, buf + 20, data_len);
  }
  memcpy(buf, header, header_len);
  return header_len + data_len;
}

CAMLprim value caml_output_value_to_buffer(value buf, value ofs, value len,
                                           value v, value flags)
{
  intnat l =
    caml_output_value_to_block(v, flags,
                               &Byte(buf, Long_val(ofs)), Long_val(len));
  return Val_long(l);
}

CAMLexport void caml_output_value_to_malloc(value v, value flags,
                                            /*out*/ char ** buf,
                                            /*out*/ intnat * len)
{
  char header[32];
  int header_len;
  intnat data_len;
  char * res;
  struct output_block * blk, * nextblk;

  init_extern_output();
  data_len = extern_value(v, flags, header, &header_len);
  res = caml_stat_alloc_noexc(header_len + data_len);
  if (res == NULL) extern_out_of_memory();
  *buf = res;
  *len = header_len + data_len;
  memcpy(res, header, header_len);
  res += header_len;
  for (blk = extern_output_first; blk != NULL; blk = nextblk) {
    intnat n = blk->end - blk->data;
    memcpy(res, blk->data, n);
    res += n;
    nextblk = blk->next;
    caml_stat_free(blk);
  }
}

/* Functions for writing user-defined marshallers */

CAMLexport void caml_serialize_int_1(int i)
{
  if (extern_ptr + 1 > extern_limit) grow_extern_output(1);
  extern_ptr[0] = i;
  extern_ptr += 1;
}

CAMLexport void caml_serialize_int_2(int i)
{
  if (extern_ptr + 2 > extern_limit) grow_extern_output(2);
  store16(extern_ptr, i);
  extern_ptr += 2;
}

CAMLexport void caml_serialize_int_4(int32_t i)
{
  if (extern_ptr + 4 > extern_limit) grow_extern_output(4);
  store32(extern_ptr, i);
  extern_ptr += 4;
}

CAMLexport void caml_serialize_int_8(int64_t i)
{
  if (extern_ptr + 8 > extern_limit) grow_extern_output(8);
  store64(extern_ptr, i);
  extern_ptr += 8;
}

CAMLexport void caml_serialize_float_4(float f)
{
  caml_serialize_block_4(&f, 1);
}

CAMLexport void caml_serialize_float_8(double f)
{
  caml_serialize_block_float_8(&f, 1);
}

CAMLexport void caml_serialize_block_1(void * data, intnat len)
{
  if (extern_ptr + len > extern_limit) grow_extern_output(len);
  memcpy(extern_ptr, data, len);
  extern_ptr += len;
}

CAMLexport void caml_serialize_block_2(void * data, intnat len)
{
  if (extern_ptr + 2 * len > extern_limit) grow_extern_output(2 * len);
#ifndef ARCH_BIG_ENDIAN
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 2, q += 2)
      Reverse_16(q, p);
    extern_ptr = q;
  }
#else
  memcpy(extern_ptr, data, len * 2);
  extern_ptr += len * 2;
#endif
}

CAMLexport void caml_serialize_block_4(void * data, intnat len)
{
  if (extern_ptr + 4 * len > extern_limit) grow_extern_output(4 * len);
#ifndef ARCH_BIG_ENDIAN
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 4, q += 4)
      Reverse_32(q, p);
    extern_ptr = q;
  }
#else
  memcpy(extern_ptr, data, len * 4);
  extern_ptr += len * 4;
#endif
}

CAMLexport void caml_serialize_block_8(void * data, intnat len)
{
  if (extern_ptr + 8 * len > extern_limit) grow_extern_output(8 * len);
#ifndef ARCH_BIG_ENDIAN
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 8, q += 8)
      Reverse_64(q, p);
    extern_ptr = q;
  }
#else
  memcpy(extern_ptr, data, len * 8);
  extern_ptr += len * 8;
#endif
}

CAMLexport void caml_serialize_block_float_8(void * data, intnat len)
{
  if (extern_ptr + 8 * len > extern_limit) grow_extern_output(8 * len);
#if ARCH_FLOAT_ENDIANNESS == 0x01234567
  memcpy(extern_ptr, data, len * 8);
  extern_ptr += len * 8;
#elif ARCH_FLOAT_ENDIANNESS == 0x76543210
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 8, q += 8)
      Reverse_64(q, p);
    extern_ptr = q;
  }
#else
  {
    unsigned char * p;
    char * q;
    for (p = data, q = extern_ptr; len > 0; len--, p += 8, q += 8)
      Permute_64(q, 0x01234567, p, ARCH_FLOAT_ENDIANNESS);
    extern_ptr = q;
  }
#endif
}

enum reachable_words_node_state {
  /* This node is reachable from at least two distinct roots, so it doesn't
   * have a unique owner and will be ignored in all future traversals. */
  Shared = -1,
  /* This node is one of the roots and has not been visited yet (i.e. the computation
   * starting at that root still hasn't ran */
  RootUnprocessed = -2,
  /* This node is one of the roots and the computation for that root has already ran */
  RootProcessed = -3,
  /* Sentinel value for a state that should never be observed */
  Invalid = -4,
  /* States that are non-negative integers indicate that a node has only been visited
   * starting from a single root. The state is then equal to the identifier of the
   * root that we reached it from */
};

static void add_to_long_value(value *v, intnat x) {
  *v = Val_long(Long_val(*v) + x);
}

/* Performs traversal through the OCaml object reachability graph to deterime
   how much memory an object has access to.

   Assumes that the position_table has already been initialized using
   [reachable_words_init].  We can run this function multiple times
   without clearing the position table to share data between runs starting
   from different roots. Identifiers must be positive integers.

   For each value node visited, we record its traversal status in the [pos] field
   of its entry in [position_table.entries]. The statuses are described in detail
   in the [reachable_words_node_state] enum.

   Returns the total size of elements marked, that is ones that are reachable
   from the current root and can be reached by at most one root from the ones
   that already ran.

   [shared_size] is incremented by the total size of elements that were newly
   marked [Shared], that is ones that we just found out are reachable from at least
   two roots.

   If [sizes_by_root_id] is not [Val_unit], we expect it to be an OCaml array
   with length equal to the number of roots. Then during the traversal we will
   update the number of words uniquely reachable from each root.
   That is, when we visit a node for the first time, we add its size to the
   corresponding root identifier, and when we visit it for the second time, we
   undo this addition. */
intnat reachable_words_once(value root, intnat identifier, value sizes_by_root_id,
    intnat *shared_size) {
  struct extern_item * sp;
  intnat size;
  uintnat mark = Invalid, new_mark;
  value v = root;
  uintnat h;
  int previously_marked, should_traverse;
  sp = extern_stack;
  size = 0;

  CAMLassert(identifier >= 0);

  while (1) {
    if (Is_long(v) || Is_null(v)) {
      /* Tagged integers or nulls contribute 0 to the size, nothing to do */
    } else if (! Is_in_heap_or_young(v)) {
      /* Out-of-heap blocks contribute 0 to the size, nothing to do */
      /* However, in no-naked-pointers mode, we don't distinguish
         between major heap blocks and out-of-heap blocks,
         and the test above is always false,
         so we end up counting out-of-heap blocks too. */
    } else {
      header_t hd = Hd_val(v);
      tag_t tag = Tag_hd(hd);
      mlsize_t sz = Wosize_hd(hd);
      intnat sz_with_header = 1 + sz;
      /* Infix pointer: go back to containing closure */
      if (tag == Infix_tag) {
        v = v - Infix_offset_hd(hd);
        continue;
      }

      previously_marked = extern_lookup_position(v, &mark, &h);
      if (!previously_marked) {
        /* All roots must have been marked by [reachable_words_mark_root] before
         * calling this function so we can safely assign new_mark to
         * identifier */
        CAMLassert(v != root);
        should_traverse = 1;
        new_mark = identifier;
      } else if (mark == RootUnprocessed && v == root) {
        should_traverse = 1;
        new_mark = RootProcessed;
      } else if (mark == Shared || mark == RootUnprocessed || mark == RootProcessed) {
        should_traverse = 0;
      } else if (mark == identifier) {
        should_traverse = 0;
      } else {
        CAMLassert(mark != Invalid);
        /* mark is some other root's identifier */
        should_traverse = 1;
        new_mark = Shared;
      }

      if (should_traverse) {
        if (!previously_marked) {
          extern_record_location_with_data(v, h, new_mark);
        } else {
          extern_update_location_with_data(h, new_mark);
        }

        /* The block contributes to the total size */
        size += sz_with_header;           /* header word included */
        if (sizes_by_root_id != Val_unit) {
          if (new_mark == Shared) {
            /* mark is identifier of some other root that we counted this node
             * as contributing to. Since it is evidently not uniquely reachable, we
             * undo this contribution */
            add_to_long_value(&Field(sizes_by_root_id, mark), -sz_with_header);
            *shared_size += sz_with_header;
          } else {
            CAMLassert(new_mark == identifier || (v == root && new_mark == RootProcessed));
            add_to_long_value(&Field(sizes_by_root_id, identifier), sz_with_header);
          }
        }
        if (tag < No_scan_tag) {
          /* i is the position of the first field to traverse recursively,
             and j is the position of the last such field.
          */
          uintnat i =
            tag == Closure_tag ? Start_env_closinfo(Closinfo_val(v)) : 0;
          uintnat j = Scannable_wosize_hd(hd);
          if (i < j) {
            if (i < j - 1) {
              /* Remember that we need to count fields i + 1 ... j - 1 */
              sp++;
              if (sp >= extern_stack_limit) sp = extern_resize_stack(sp);
              sp->v = &Field(v, i + 1);
              sp->count = j - i - 1;
            }
            /* Continue with field i */
            v = Field(v, i);
            continue;
          }
        }
      }
    }
    /* Pop one more item to traverse, if any */
    if (sp == extern_stack) break;
    v = *((sp->v)++);
    if (--(sp->count) == 0) sp--;
  }

  return size;
}

void reachable_words_init(void)
{
  obj_counter = 0;
  extern_flags = 0;
  extern_init_position_table();
}

void reachable_words_mark_root(value v)
{
  uintnat h, mark;
  extern_lookup_position(v, &mark, &h);
  extern_record_location_with_data(v, h, RootUnprocessed);
}

void reachable_words_cleanup(void)
{
  extern_free_stack();
  extern_free_position_table();
}

CAMLprim value caml_obj_reachable_words(value v)
{
  CAMLparam1(v);
  CAMLlocal1(size);

  intnat shared_size = 0;

  reachable_words_init();
  reachable_words_mark_root(v);
  size = Val_long(reachable_words_once(v, 0, Val_unit, &shared_size));
  reachable_words_cleanup();

  CAMLreturn(size);
}

CAMLprim value caml_obj_uniquely_reachable_words(value v)
{
  CAMLparam1(v);
  CAMLlocal2(sizes_by_root_id, ret);

  intnat length, shared_size;

  length = Wosize_val(v);
  sizes_by_root_id = caml_alloc(length, 0);
  shared_size = 0;

  reachable_words_init();
  for (intnat i = 0; i < length; i++) {
    reachable_words_mark_root(Field(v, i));
    Field(sizes_by_root_id, i) = Val_int(0);
  }
  for (intnat i = 0; i < length; i++) {
    reachable_words_once(Field(v, i), i, sizes_by_root_id, &shared_size);
  }
  reachable_words_cleanup();

  ret = caml_alloc_small(2, 0);
  Field(ret, 0) = sizes_by_root_id;
  Field(ret, 1) = Val_long(shared_size);
  CAMLreturn(ret);
}
