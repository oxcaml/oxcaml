/* Copyright (c) 2021 Nathan Rebours <nathan.p.rebours@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#define CAML_INTERNALS

#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/stack.h"
#include "caml/callback.h"
#include "caml/alloc.h"
#include "caml/osdeps.h"
#include "caml/codefrag.h"

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/mman.h>
#include <unistd.h>

CAMLprim value jit_get_page_size(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);

  result = Val_long(getpagesize());

  CAMLreturn(result);
}

CAMLprim value jit_dlsym(value symbol) {
  CAMLparam1(symbol);
  CAMLlocal1(result);
  void *addr;

  addr = caml_globalsym(String_val(symbol));
  if(!addr) {
    result = Val_int(0);
  } else {
    result = caml_alloc(1, 0);
    Store_field(result, 0, caml_copy_nativeint((intnat) addr));
  }

  CAMLreturn (result);
}

CAMLprim value jit_memalign(value section_size) {
  CAMLparam1 (section_size);
  CAMLlocal1 (result);
  void *addr = NULL;
  long res, size;

  size = Long_val(section_size);
  res = posix_memalign(&addr, getpagesize(), size);
  if (res) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string(strerror(res)));
  } else {
    result = caml_alloc(1, 0);
    Store_field(result, 0, caml_copy_nativeint((intnat) addr));
  };

  CAMLreturn(result);
}

CAMLprim value jit_load_section(value addr, value section, value section_size) {
  CAMLparam3 (addr, section, section_size);
  int size = Int_val(section_size);
  const char *src = String_val(section);
  void *dest = (intnat*) Nativeint_val(addr);

  memcpy(dest, src, size);

  CAMLreturn(Val_unit);
}

CAMLprim value jit_mprotect_ro(value caml_addr, value caml_size) {
  CAMLparam2 (caml_addr, caml_size);
  CAMLlocal1 (result);

  void *addr;
  int size;

  size = Long_val(caml_size);
  addr = (intnat*) Nativeint_val(caml_addr);

  if (mprotect(addr, size, PROT_READ)) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, Val_int(errno));
  } else {
    result = caml_alloc(1, 0);
    Store_field(result, 0, Val_unit);
  };

  CAMLreturn(result);
}

CAMLprim value jit_mprotect_rx(value caml_addr, value caml_size) {
  CAMLparam2 (caml_addr, caml_size);
  CAMLlocal1 (result);

  void *addr;
  int size;

  size = Int_val(caml_size);
  addr = (intnat*) Nativeint_val(caml_addr);

  if (mprotect(addr, size, PROT_READ | PROT_EXEC)) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, Val_int(errno));
  } else {
    result = caml_alloc(1, 0);
    Store_field(result, 0, Val_unit);
  };

  CAMLreturn(result);
}

static void *addr_from_caml_option(value option)
{
  void *sym = NULL;
  if (Is_block(option)) {
    sym = (intnat*) Nativeint_val(Field(option,0));
  }
  return sym;
}

CAMLprim value jit_run(value symbols_addresses) {
  CAMLparam1 (symbols_addresses);
  CAMLlocal1 (result);
  void *sym,*sym2;

  intnat entrypoint;

  sym = addr_from_caml_option(Field(symbols_addresses, 0));
  if (NULL != sym) caml_register_frametable(sym);

  sym = addr_from_caml_option(Field(symbols_addresses, 1));
  if (NULL != sym) caml_register_dyn_global(sym);

  sym = addr_from_caml_option(Field(symbols_addresses, 2));
  sym2 = addr_from_caml_option(Field(symbols_addresses, 3));
  if (NULL != sym && NULL != sym2)
    caml_page_table_add(In_static_data, sym, sym2);

  sym = addr_from_caml_option(Field(symbols_addresses, 4));
  sym2 = addr_from_caml_option(Field(symbols_addresses, 5));
  if (NULL != sym && NULL != sym2)
    caml_register_code_fragment((char *) sym, (char *) sym2,
                                DIGEST_LATER, NULL);

  entrypoint = Nativeint_val(Field(symbols_addresses, 6));
  result = caml_callback((value)(&entrypoint), 0);

  CAMLreturn (result);
}

CAMLprim value jit_run_toplevel(value symbols_addresses) {
  CAMLparam1 (symbols_addresses);
  CAMLlocal2 (res, v);

  res = caml_alloc(1,0);
  v = jit_run(symbols_addresses);
  Store_field(res, 0, v);

  CAMLreturn(res);
}

CAMLprim value jit_addr_to_obj(value address) {
  CAMLparam1 (address);
  CAMLlocal1 (obj);

  obj = (value) ((intnat*) Nativeint_val(address));

  CAMLreturn(obj);
}
