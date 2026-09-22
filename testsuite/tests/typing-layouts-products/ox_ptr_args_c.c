#include <string.h>
#include <stdint.h>
#include "caml/mlvalues.h"
#include "caml/alloc.h"

/* The offset is a plain byte offset (non-mixed pointee), added verbatim. */
static char *decode_fat_pointer(value pair)
{
  return (char *)Bytes_val(Field(pair, 0))
         + (intnat)Int64_val(Field(pair, 1));
}

value test_fill_native(char *p, intnat c, intnat n)
{
  memset(p, (int)c, (size_t)n);
  return Val_unit;
}

value test_fill_bytecode(value pair, value c, value n)
{
  memset(decode_fat_pointer(pair), (int)Long_val(c), (size_t)Long_val(n));
  return Val_unit;
}

intnat test_cmp_native(char *p, char *q, intnat n)
{
  int r = memcmp(p, q, (size_t)n);
  return r < 0 ? -1 : r > 0 ? 1 : 0;
}

value test_cmp_bytecode(value pair1, value pair2, value n)
{
  int r = memcmp(decode_fat_pointer(pair1), decode_fat_pointer(pair2),
                 (size_t)Long_val(n));
  return Val_long(r < 0 ? -1 : r > 0 ? 1 : 0);
}

/* Bytecode versions of the direct libc bindings */

value test_memset_bytecode(value pair, value c, value n)
{
  void *r = memset(decode_fat_pointer(pair), (int)Long_val(c),
                   (size_t)Long_val(n));
  return caml_copy_nativeint((intnat)r);
}

value test_memmove_bytecode(value dst, value src, value n)
{
  void *r = memmove(decode_fat_pointer(dst), decode_fat_pointer(src),
                    (size_t)Long_val(n));
  return caml_copy_nativeint((intnat)r);
}

intnat test_ptr_id(char *p)
{
  return (intnat)p;
}

value test_ptr_id_bytecode(value pair)
{
  return caml_copy_nativeint((intnat)decode_fat_pointer(pair));
}
