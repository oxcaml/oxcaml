/* Stubs for testing heap extents (caml_add_extent). */

#define CAML_INTERNALS

#include <stdlib.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/gc.h>
#include <caml/custom.h>
#include <caml/camlatomic.h>
#include <caml/shared_heap.h>

/* Counters are atomic: extents may be swept (and so freed) by several
   domains concurrently. */

static atomic_uintnat freed_extents = 0;
static atomic_uintnat finalised_customs = 0;

static void extent_free_callback(void *base, size_t size)
{
  (void)size;
  atomic_fetch_add(&freed_extents, 1);
  free(base);
}

CAMLprim value heap_extent_freed_count(value unit)
{
  (void)unit;
  return Val_long(atomic_load(&freed_extents));
}

CAMLprim value heap_extent_finalised_count(value unit)
{
  (void)unit;
  return Val_long(atomic_load(&finalised_customs));
}

/* Create a heap extent containing one block per element of [sizes],
   each a tag-0 block of the given wosize (which may be 0) with all
   fields initialized to Val_unit. Returns the array of blocks. */

CAMLprim value heap_extent_make(value sizes)
{
  CAMLparam1(sizes);
  CAMLlocal1(blocks);
  mlsize_t n = Wosize_val(sizes);
  size_t wsize = 0;
  for (mlsize_t i = 0; i < n; i++)
    wsize += Whsize_wosize(Long_val(Field(sizes, i)));
  if (wsize == 0)
    caml_invalid_argument("heap_extent_make: empty extent");

  value *base = malloc(wsize * sizeof(value));
  if (base == NULL)
    caml_failwith("heap_extent_make: out of memory");

  blocks = caml_alloc(n, 0);

  value *p = base;
  for (mlsize_t i = 0; i < n; i++) {
    mlsize_t wo = Long_val(Field(sizes, i));
    *p = Make_header(wo, 0, 0);
    for (mlsize_t j = 0; j < wo; j++)
      p[1 + j] = Val_unit;
    p += Whsize_wosize(wo);
  }

  caml_add_extent(base, wsize * sizeof(value), extent_free_callback);

  p = base;
  for (mlsize_t i = 0; i < n; i++) {
    Store_field(blocks, i, Val_hp(p));
    p += Whsize_wosize(Long_val(Field(sizes, i)));
  }
  CAMLreturn(blocks);
}

/* The address of a block, for checking whether compaction moved it. */

CAMLprim value heap_extent_block_address(value v)
{
  return caml_copy_nativeint((intnat)v);
}

/* Custom blocks with a counting finaliser, to exercise finalisation
   of custom blocks during extent sweeping. */

static void extent_custom_finalise(value v)
{
  (void)v;
  atomic_fetch_add(&finalised_customs, 1);
}

static struct custom_operations extent_custom_ops = {
  "heap_extent.test_custom",
  extent_custom_finalise,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

/* Create a heap extent of [n] custom blocks, each with a finaliser
   that increments a counter. Returns the array of blocks. */

CAMLprim value heap_extent_make_custom(value vn)
{
  CAMLparam1(vn);
  CAMLlocal1(blocks);
  mlsize_t n = Long_val(vn);
  /* Each block: header, custom_operations pointer, one payload word */
  mlsize_t block_whsize = 3;
  size_t wsize = n * block_whsize;
  if (wsize == 0)
    caml_invalid_argument("heap_extent_make_custom: empty extent");

  value *base = malloc(wsize * sizeof(value));
  if (base == NULL)
    caml_failwith("heap_extent_make_custom: out of memory");

  blocks = caml_alloc(n, 0);

  value *p = base;
  for (mlsize_t i = 0; i < n; i++) {
    *p = Make_header(block_whsize - 1, Custom_tag, 0);
    p[1] = (value)&extent_custom_ops;
    p[2] = Val_long(i);
    p += block_whsize;
  }

  caml_add_extent(base, wsize * sizeof(value), extent_free_callback);

  p = base;
  for (mlsize_t i = 0; i < n; i++) {
    Store_field(blocks, i, Val_hp(p));
    p += block_whsize;
  }
  CAMLreturn(blocks);
}
