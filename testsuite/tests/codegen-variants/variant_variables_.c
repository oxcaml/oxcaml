#define CAML_NAME_SPACE
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* The macros that this compilation sees, among the one that the test adds
   to cflags and mkexe and the codegen-variant axis macros. */
static const char *macros(void)
{
  return ""
#ifdef FROM_TEST
    " FROM_TEST"
#endif
#ifdef WITH_FRAME_POINTERS
    " WITH_FRAME_POINTERS"
#endif
#ifdef STACK_CHECKS_ENABLED
    " STACK_CHECKS_ENABLED"
#endif
#ifdef WITH_ADDRESS_SANITIZER
    " WITH_ADDRESS_SANITIZER"
#endif
    ;
}

#ifdef STANDALONE
int main(void)
{
  printf("mkexe:%s\n", macros());
  return 0;
}
#else
/* Whether this compilation is instrumented for AddressSanitizer, against
   what the axis macro of the variant says. */
#if defined(__SANITIZE_ADDRESS__)
#define INSTRUMENTED 1
#elif defined(__has_feature)
#if __has_feature(address_sanitizer)
#define INSTRUMENTED 1
#endif
#endif

/* Whether the program is linked with the AddressSanitizer runtime, which
   defines this function.  The reference is weak, and only an ELF link
   leaves a weak reference undefined: elsewhere no variant has
   AddressSanitizer, and there is no such runtime to look for. */
#ifdef __ELF__
extern void __asan_init(void) __attribute__((weak));
#define ASAN_RUNTIME_LINKED (__asan_init != 0)
#else
#define ASAN_RUNTIME_LINKED 0
#endif

static const char *instrumentation(void)
{
#if defined(INSTRUMENTED) && !defined(WITH_ADDRESS_SANITIZER)
  return " instrumented, for a variant without it";
#elif !defined(INSTRUMENTED) && defined(WITH_ADDRESS_SANITIZER)
  return " not instrumented, for a variant with it";
#elif !defined(WITH_ADDRESS_SANITIZER)
  return ASAN_RUNTIME_LINKED
    ? " its runtime is linked, for a variant without it"
    : " as the variant has it";
#else
  return " as the variant has it";
#endif
}

value variant_macros(value unit)
{
  (void)unit;
  return caml_copy_string(macros());
}

value variant_instrumentation(value unit)
{
  (void)unit;
  return caml_copy_string(instrumentation());
}
#endif
