/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*                  Jacob Van Buren, Jane Street, New York                */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* A check of the build, not a stub: this file is compiled in each
   codegen-variant context of the build with the C flags of the context's
   stubs, and fails to compile unless they are those of the context's
   variant (see the dune file).  The stub archives of a context are
   installed in the library tree of its variant and linked with a runtime
   and OCaml code built for it: they must see the axis macros that those
   were built with, as the stack layout macros of the runtime's headers
   depend on them.

   The EXPECT_* macros say what the context's variant is.  They come from
   the variant's name alone and not from the flags under test (see
   stub_macros_expected in Makefile.common-ox). */

#include <caml/config.h>

#if !defined(EXPECT_asan) || !defined(EXPECT_fp) || !defined(EXPECT_sc)
#error "the expectations of this check are missing"
#endif

/* The flags state the variant in full (see m.h). */
#ifndef CAML_CODEGEN_VARIANT_EXPLICIT
#error "CAML_CODEGEN_VARIANT_EXPLICIT is not defined"
#endif

#if defined(WITH_ADDRESS_SANITIZER) != EXPECT_asan
#error "WITH_ADDRESS_SANITIZER: not as in this context's codegen variant"
#endif

#if defined(WITH_FRAME_POINTERS) != EXPECT_fp
#error "WITH_FRAME_POINTERS: not as in this context's codegen variant"
#endif

#if defined(STACK_CHECKS_ENABLED) != EXPECT_sc
#error "STACK_CHECKS_ENABLED: not as in this context's codegen variant"
#endif

/* The stubs are instrumented by AddressSanitizer in the variants that have
   it, and elsewhere they must not be: programs built for those variants
   are linked without the AddressSanitizer runtime.  (How to detect the
   instrumentation: see CAMLno_asan in caml/misc.h.) */
#if defined(__has_feature)
#  if __has_feature(address_sanitizer)
#    define INSTRUMENTED 1
#  endif
#elif defined(__SANITIZE_ADDRESS__)
#  define INSTRUMENTED 1
#endif

#if defined(INSTRUMENTED) != EXPECT_asan
#error "AddressSanitizer instrumentation: not as in this context's variant"
#endif

typedef int caml_codegen_variant_stub_macros_checked;
