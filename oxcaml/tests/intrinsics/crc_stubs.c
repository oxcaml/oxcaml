#include "caml/config.h"
#include "caml/mlvalues.h"
#include "caml/alloc.h"

#ifdef  __ARM_FEATURE_CRC32
#include <arm_acle.h>

static inline uint32_t crc64(uint32_t initial, uint64_t data)
{
  return __crc32cd(initial, data);
}

#elif defined(__SSE4_2__) || defined(_MSC_VER)

#ifdef _MSC_VER
#include <intrin.h>
#else
#include <smmintrin.h>
#endif

static inline uint32_t crc64(uint32_t initial, uint64_t data)
{
   return _mm_crc32_u64(initial, data);
}

#else

#error "Target not supported"
uint64_t crc64(uint64_t initial, uint64_t data);
#endif

int64_t caml_int64_crc32(int64_t initial, int64_t data)
{
  return (int64_t)crc64((uint32_t)initial, (uint64_t) data);
}

CAMLprim value caml_int64_crc_bytecode(value v_initial, value v_data)
{
  return caml_copy_int64(caml_int64_crc32(Int64_val(v_initial), Int64_val(v_data)));
}
