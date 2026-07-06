/* Stress test for extern_standalone.c (or extern_standalone.cpp):
   marshals a list of 10000 distinct strings, exercising position table
   resize (>170 shared objects), output block growth (>8100 bytes) and
   extern stack resize (list longer than 256 elements).

   Build:  cc -std=c11 -Wall -Wextra extern_standalone.c \
               extern_standalone_test_big.c -o test_big
   (also worth running with -fsanitize=address).  Writes the marshalled
   list to the file given as argv[1]; compare it against the output of
   extern_standalone_gen_expected_big.ml using
   extern_standalone_compare.py, and read it back with
   extern_standalone_check_big.ml. */

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
  value b = mk_block(252, wosize);
  char * data = (char *) b;
  memset(data, 0, bosize);
  memcpy(data, str, len);
  data[bosize - 1] = (char) (bosize - 1 - len);
  return b;
}

#define N 10000

int main(int argc, char ** argv)
{
  if (argc != 2) { fprintf(stderr, "usage: %s OUTFILE\n", argv[0]); return 2; }
  FILE * out = fopen(argv[1], "wb");
  if (out == NULL) { perror("fopen"); return 2; }

  /* A list of N distinct strings "item0" ... "item9999" */
  value l = mk_long(0);
  for (long i = N - 1; i >= 0; i--) {
    char name[32];
    snprintf(name, sizeof name, "item%ld", i);
    value cell = mk_block(0, 2);
    set_field(cell, 0, mk_string(name));
    set_field(cell, 1, l);
    l = cell;
  }

  char * buf;
  intnat len;
  caml_extern_error err = caml_output_value_to_malloc(l, &buf, &len);
  if (err != CAML_EXTERN_OK) {
    fprintf(stderr, "error %d: %s\n", (int) err, caml_extern_error_message());
    return 2;
  }
  fwrite(buf, 1, len, out);
  free(buf);
  fclose(out);
  return 0;
}
