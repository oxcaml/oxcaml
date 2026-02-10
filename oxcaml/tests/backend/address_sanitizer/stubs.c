#include <assert.h>
#include <smmintrin.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/gc.h>
#include <caml/memory.h>
#include <caml/simd.h>

#define BUILTIN(name) void name() { assert(0); }

BUILTIN(caml_sse_vec128_load_aligned);
BUILTIN(caml_sse_vec128_load_unaligned);
BUILTIN(caml_sse3_vec128_load_known_unaligned);
BUILTIN(caml_sse_vec128_store_aligned);
BUILTIN(caml_sse_vec128_store_unaligned);
BUILTIN(caml_sse_vec128_store_aligned_uncached);
BUILTIN(caml_sse2_vec128_load_low64);
BUILTIN(caml_sse2_vec128_load_low32);
BUILTIN(caml_sse2_int32_store_uncached);
BUILTIN(caml_sse2_int64_store_uncached);
BUILTIN(caml_sse2_vec128_load_low64_copy_high64);
BUILTIN(caml_sse2_vec128_load_high64_copy_low64);
BUILTIN(caml_sse2_vec128_load_zero_low32);
BUILTIN(caml_sse2_vec128_load_zero_low64);
BUILTIN(caml_sse2_vec128_store_low32);
BUILTIN(caml_sse2_vec128_store_low64);
BUILTIN(caml_sse2_vec128_store_mask8);
BUILTIN(caml_sse3_vec128_load_broadcast64);
BUILTIN(caml_sse41_vec128_load_aligned_uncached);

BUILTIN(caml_avx_vec256_load_aligned);
BUILTIN(caml_avx_vec256_load_unaligned);
BUILTIN(caml_avx_vec256_load_known_unaligned);
BUILTIN(caml_avx_vec256_store_aligned);
BUILTIN(caml_avx_vec256_store_unaligned);
BUILTIN(caml_avx_vec256_load_aligned_uncached);
BUILTIN(caml_avx_vec256_store_aligned_uncached);
BUILTIN(caml_avx_vec256_load_broadcast128);
BUILTIN(caml_avx_vec256_load_broadcast64);
BUILTIN(caml_avx_vec256_load_broadcast32);
BUILTIN(caml_avx_vec128_load_broadcast32);
BUILTIN(caml_avx_vec128_load_mask64);
BUILTIN(caml_avx_vec256_load_mask64);
BUILTIN(caml_avx_vec128_load_mask32);
BUILTIN(caml_avx_vec256_load_mask32);
BUILTIN(caml_avx_vec128_store_mask64);
BUILTIN(caml_avx_vec256_store_mask64);
BUILTIN(caml_avx_vec128_store_mask32);
BUILTIN(caml_avx_vec256_store_mask32);

#define DO_NOT_SANITIZE __attribute__((no_sanitize("address")))

CAMLprim value ocaml_address_sanitizer_test_alloc(size_t len, int64_t tag) {
  assert(len > 0);
  header_t *block = malloc((len * sizeof(value)) + sizeof(header_t));
  *block = Caml_out_of_heap_header(/*wosize=*/len, /*tag=*/tag);
  if (Scannable_tag(tag)) {
    for (size_t i = 0; i < len; i++) Op_hp(block)[i] = Val_unit;
  }
  return Val_hp(block);
}

CAMLprim value ocaml_address_sanitizer_test_free(value block) {
  // The explicit cast to [void*] here is necessary to avoid compiler warnings
  // under runtime5.
  free((void *)(Hp_val(block)));
  return Val_unit;
}

CAMLprim __m128i ocaml_address_sanitizer_test_vec128_of_int64s(int64_t low,
                                                               int64_t high) {
  return _mm_set_epi64x(high, low);
}

CAMLprim __m256i ocaml_address_sanitizer_test_vec256_of_int64s(int64_t w0,
                                                               int64_t w1,
                                                               int64_t w2,
                                                               int64_t w3) {
  return _mm256_set_epi64x(w3, w2, w1, w0);
}

CAMLprim value DO_NOT_SANITIZE caml_prefetch_read_low(const void *ptr) {
  __builtin_prefetch(ptr, /*is_write=*/0, /*locality=*/1);
  return Val_unit;
}

CAMLprim value DO_NOT_SANITIZE caml_prefetch_write_low(const void *ptr) {
  __builtin_prefetch(ptr, /*is_write=*/1, /*locality=*/1);
  return Val_unit;
}

CAMLprim value DO_NOT_SANITIZE caml_cldemote(const void *ptr) {
#if (defined(__i386__) || defined(__x86_64__))
  asm volatile("cldemote (%0)\n" : : "r"(ptr));
#endif
  return Val_unit;
}

static char *DO_NOT_SANITIZE bigstring_element_at_pos(value v_bstr, intnat pos) {
  return ((char *)Caml_ba_data_val(v_bstr)) + pos;
}

CAMLprim intnat DO_NOT_SANITIZE caml_bigstring_fetch_and_add_int_untagged(value v_bstr, intnat pos, intnat n) {
  intnat *decode = (intnat *)bigstring_element_at_pos(v_bstr, pos);
  return __sync_fetch_and_add(decode, n);
}
