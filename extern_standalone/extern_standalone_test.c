/* Test harness for extern_standalone.c (and for extern_standalone.cpp,
   by linking this file against the C++ build instead).

   Build:  cc -std=c11 -Wall -Wextra extern_standalone.c \
               extern_standalone_test.c -o test_extern

   Constructs OCaml values by hand (native-code OxCaml representation)
   and marshals them, writing the concatenated results to the file given
   as argv[1]; then exercises the error paths, which are self-checking.
   The dumped values are intended to be compared against the output of
   extern_standalone_gen_expected.ml (the same values marshalled by the
   real Marshal module) using extern_standalone_compare.py: they match
   byte-for-byte except for the 32-bit size field at bytes 12-15 of each
   marshal header, which is written as 0 here.  Reading the output back
   with extern_standalone_check_readback.ml must also yield the original
   values.  See extern_standalone_test_big.c for a stress test. */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef intptr_t intnat;
typedef uintptr_t uintnat;
typedef intnat value;

typedef enum {
  CAML_EXTERN_OK = 0,
  CAML_EXTERN_ERROR_OUT_OF_MEMORY = 1,
  CAML_EXTERN_ERROR_INVALID_ARGUMENT = 2
} caml_extern_error;

extern caml_extern_error caml_output_value_to_malloc(value v, char ** buf,
                                                     intnat * len);
extern const char * caml_extern_error_message(void);

/* Value construction.  Header: 8 reserved bits (zero here), 46 size bits,
   2 colour bits (NOT_MARKABLE), 8 tag bits. */

#define TAG_CLOSURE 247
#define TAG_ABSTRACT 251
#define TAG_STRING 252
#define TAG_DOUBLE 253
#define TAG_DOUBLE_ARRAY 254
#define TAG_CUSTOM 255

static value mk_long(long n) { return ((value) n << 1) | 1; }

static value mk_block(unsigned tag, size_t wosize)
{
  uintnat * p = malloc((wosize + 1) * sizeof(value));
  if (p == NULL) abort();
  p[0] = ((uintnat) wosize << 10) | (3u << 8) | tag;
  return (value) (p + 1);
}

static void set_field(value b, size_t i, value f)
{
  ((value *) b)[i] = f;
}

static value mk_string(const char * str)
{
  size_t len = strlen(str);
  size_t wosize = (len + sizeof(value)) / sizeof(value);
  size_t bosize = wosize * sizeof(value);
  value b = mk_block(TAG_STRING, wosize);
  char * data = (char *) b;
  memset(data, 0, bosize);
  memcpy(data, str, len);
  data[bosize - 1] = (char) (bosize - 1 - len);
  return b;
}

static value mk_double(double d)
{
  value b = mk_block(TAG_DOUBLE, 1);
  memcpy((void *) b, &d, 8);
  return b;
}

static value mk_double_array(const double * d, size_t n)
{
  value b = mk_block(TAG_DOUBLE_ARRAY, n);
  memcpy((void *) b, d, n * 8);
  return b;
}

static value mk_list(const long * elts, size_t n)
{
  value l = mk_long(0);              /* [] */
  for (size_t i = n; i > 0; i--) {
    value cell = mk_block(0, 2);
    set_field(cell, 0, mk_long(elts[i - 1]));
    set_field(cell, 1, l);
    l = cell;
  }
  return l;
}

static FILE * out;

static void dump(value v)
{
  char * buf;
  intnat len;
  caml_extern_error err = caml_output_value_to_malloc(v, &buf, &len);
  if (err != CAML_EXTERN_OK) {
    fprintf(stderr, "unexpected error %d: %s\n", (int) err,
            caml_extern_error_message());
    exit(2);
  }
  fwrite(buf, 1, len, out);
  free(buf);
}

int main(int argc, char ** argv)
{
  if (argc != 2) { fprintf(stderr, "usage: %s OUTFILE\n", argv[0]); return 2; }
  out = fopen(argv[1], "wb");
  if (out == NULL) { perror("fopen"); return 2; }

  /* Same values, in the same order, as gen_expected.ml */
  dump(mk_long(42));

  value pair = mk_block(0, 2);
  set_field(pair, 0, mk_long(1));
  set_field(pair, 1, mk_long(2));
  dump(pair);

  dump(mk_string("hello"));

  value shared = mk_block(0, 2);
  set_field(shared, 0, pair);
  set_field(shared, 1, pair);
  dump(shared);

  dump(mk_double(3.14));

  double floats[3] = { 1.5, 2.5, 3.5 };
  dump(mk_double_array(floats, 3));

  long ints[3] = { 1, 2, 3 };
  dump(mk_list(ints, 3));

  dump(mk_string(""));
  dump(mk_long(-5));
  dump(mk_long(-1000));
  dump(mk_long(100000));
  dump(mk_long(1L << 40));

  char buf100[101], buf300[301];
  memset(buf100, 'x', 100); buf100[100] = '\0';
  memset(buf300, 'y', 300); buf300[300] = '\0';
  dump(mk_string(buf100));
  dump(mk_string(buf300));

  /* A variant with a non-zero tag and an atom (empty block) */
  value variant = mk_block(3, 1);
  set_field(variant, 0, mk_string("payload"));
  dump(variant);
  dump(mk_block(0, 0));

  /* Nested structure exercising the extern stack */
  value nested = mk_block(0, 3);
  set_field(nested, 0, mk_list(ints, 3));
  set_field(nested, 1, mk_string("mid"));
  set_field(nested, 2, mk_double(2.25));
  dump(nested);

  fclose(out);

  /* Error paths, not compared against the OCaml output */

  caml_extern_error err;

  value abstract = mk_block(TAG_ABSTRACT, 1);
  set_field(abstract, 0, mk_long(0));
  char * buf;
  intnat len;
  err = caml_output_value_to_malloc(abstract, &buf, &len);
  if (err != CAML_EXTERN_ERROR_INVALID_ARGUMENT
      || strcmp(caml_extern_error_message(),
                "output_value: abstract value (Abstract)") != 0) {
    fprintf(stderr, "expected abstract error, got %d: %s\n", (int) err,
            caml_extern_error_message());
    return 2;
  }

  /* Custom blocks are now rejected */
  value custom = mk_block(TAG_CUSTOM, 2);
  set_field(custom, 0, (value) &main);   /* would be the ops pointer */
  set_field(custom, 1, mk_long(0));
  err = caml_output_value_to_malloc(custom, &buf, &len);
  if (err != CAML_EXTERN_ERROR_INVALID_ARGUMENT
      || strcmp(caml_extern_error_message(),
                "output_value: abstract value (Custom)") != 0) {
    fprintf(stderr, "expected custom error, got %d: %s\n", (int) err,
            caml_extern_error_message());
    return 2;
  }

  /* Closures are always rejected */
  value closure = mk_block(TAG_CLOSURE, 3);
  set_field(closure, 0, (value) &main);
  set_field(closure, 1, (value) ((1ULL << 56) + (1ULL << 55) + (2 << 1) + 1));
  set_field(closure, 2, mk_long(7));
  err = caml_output_value_to_malloc(closure, &buf, &len);
  if (err != CAML_EXTERN_ERROR_INVALID_ARGUMENT
      || strcmp(caml_extern_error_message(),
                "output_value: functional value") != 0) {
    fprintf(stderr, "expected functional-value error, got %d: %s\n",
            (int) err, caml_extern_error_message());
    return 2;
  }

  printf("error-path tests passed\n");
  return 0;
}
