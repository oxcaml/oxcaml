/******************************************************************************
 *                                  OxCaml                                    *
 *                          Kevin Svetlitski, Jane Street                     *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************/

#ifndef OCAML_DEMANGLE_H
#define OCAML_DEMANGLE_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Returns a heap-allocated, demangled OCaml symbol name. If `symbol` is not
   an OCaml symbol name, this returns `NULL`. The result is NUL-terminated
   and must be released with `free()`; `NULL` is also returned if the
   allocation fails. `symbol` need not be NUL-terminated: exactly `length`
   bytes of it are read. */
const char* demangle_ocaml_symbol(size_t length, const char* symbol);

#ifdef __cplusplus
}
#endif

#endif /* OCAML_DEMANGLE_H */
