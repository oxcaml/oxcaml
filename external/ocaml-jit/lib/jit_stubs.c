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

#include "caml/config.h"
#include "caml/mlvalues.h"
#include "caml/frame_descriptors.h"
#define NATIVE_CODE
#include "caml/globroots.h"
#include "caml/memory.h"
#include "caml/stack.h"
#include "caml/callback.h"
#include "caml/alloc.h"
#include "caml/osdeps.h"
#include "caml/codefrag.h"
#include "caml/bigarray.h"
#include "caml/fail.h"

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdalign.h>

/* ARM64 cache maintenance for JIT code */
#if defined(__aarch64__)
#if defined(__APPLE__)
#include <libkern/OSCacheControl.h>
#define JIT_FLUSH_ICACHE(addr, size) sys_icache_invalidate((addr), (size))
#else
/* Linux ARM64: use __builtin___clear_cache or inline assembly */
#define JIT_FLUSH_ICACHE(addr, size) __builtin___clear_cache((char*)(addr), (char*)(addr) + (size))
#endif
#else
/* x86_64 has coherent I-cache, no explicit flush needed */
#define JIT_FLUSH_ICACHE(addr, size) ((void)0)
#endif

#ifdef __linux__
bool __attribute__((weak)) TCMalloc_MallocExtension_MallocIsTCMalloc(void) {
  return false;
}
#endif

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

#if !defined(__APPLE__)

#define SBRK_FAILED ((void*)-1)

static void* alloc_page_aligned_using_sbrk(size_t page_size, size_t size) {
  assert(size % page_size == 0);
  uint8_t* brk = sbrk(0);
  if (brk == SBRK_FAILED) {
    return NULL;
  }
  uint8_t* next_page_start = (uint8_t*) ((((uintptr_t)brk) + (page_size - 1)) & ~(page_size - 1));
  size_t padding = next_page_start - brk;
  brk = sbrk(padding + size);
  if (brk == SBRK_FAILED) {
    return NULL;
  }
  assert((uintptr_t)brk % page_size == 0);
  return next_page_start;
}

#endif

#if defined(__has_feature)
  // For clang
  #if __has_feature(address_sanitizer)
    #define ASAN_IS_ENABLED 1
  #else
    #define ASAN_IS_ENABLED 0
  #endif
#else
  // For gcc
  #ifdef __SANITIZE_ADDRESS__
    #define ASAN_IS_ENABLED 1
  #else
    #define ASAN_IS_ENABLED 0
  #endif
#endif

/* [musl] infamously does not provide a preprocessor macro of its own,
   but this is good enough for practical purposes. */
#if defined(__linux__) && !defined(__GLIBC__)
  #define MUSL 1
#else
  #define MUSL 0
#endif

#define STATIC_BUFFER_ALIGNMENT (1U << 16)
static void *alloc_page_aligned_statically(size_t page_size, size_t size) {
  static char alignas(STATIC_BUFFER_ALIGNMENT) buffer[1U << 20];
  static char *buffer_current = &buffer[0];
  static const char *const buffer_end = &buffer[sizeof(buffer)];
  assert(page_size <= STATIC_BUFFER_ALIGNMENT);
  assert((page_size & (page_size - 1)) == 0); // [page_size] is a power of 2
  assert((((uintptr_t)buffer_current) & (page_size - 1)) == 0); // [buffer_current] is aligned to [page_size]
  // round up [size] to the nearest multiple of [page_size]
  size = (size + (page_size - 1)) & ~(page_size - 1);
  if (buffer_current + size > buffer_end) {
    return NULL;
  }
  void *result = buffer_current;
  buffer_current += size;
  return result;
}

CAMLprim value jit_memalign(value section_size) {
  CAMLparam1 (section_size);
  CAMLlocal1 (result);
  const size_t size = Long_val(section_size);
  const size_t page_size = getpagesize();
  void* addr;
  if (MUSL) {
    /* [musl]'s [malloc] uses both [sbrk] *and* [mmap], so we resort to allocating
       things statically when linked against it. This is really a bandaid solution
       to make the tests pass. For serious usage of this under [musl], we'll need to
       do better. */
    addr = alloc_page_aligned_statically(page_size, size);
  } else if (ASAN_IS_ENABLED
#ifdef __linux__
             || TCMalloc_MallocExtension_MallocIsTCMalloc()
#endif
  ) {
    /* AddressSanitizer and TCMalloc use [mmap], not [sbrk], which results in
       addresses which are too large to apply relocations to against other
       sections (e.g. [.rodata]), so we manually use [sbrk] when linked against
       either. */
#if defined(__APPLE__)
    /* sbrk is deprecated on macOS, so we'll have to make do */
    addr = aligned_alloc(page_size, size);
#else
    addr = alloc_page_aligned_using_sbrk(page_size, size);
#endif
  } else {
    addr = aligned_alloc(page_size, size);
  }
  if (addr == NULL) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string(strerror(errno)));
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

  /* Flush instruction cache before making memory executable.
     On ARM64, the instruction cache is not coherent with the data cache,
     so we must explicitly invalidate the I-cache after writing code. */
  JIT_FLUSH_ICACHE(addr, size);

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
  if (NULL != sym) {
    caml_register_frametables(&sym, 1);
  }

  sym = addr_from_caml_option(Field(symbols_addresses, 1));
  if (NULL != sym) {
    caml_register_dyn_globals(&sym, 1);
  }

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

CAMLprim value jit_obj_to_addr(value obj) {
  CAMLparam1(obj);
  CAMLreturn(caml_copy_nativeint((intnat) obj));
}

/* ===========================================================================
   GDB JIT Interface
   ---------------------------------------------------------------------------
   Protocol per https://sourceware.org/gdb/current/onlinedocs/gdb.html/JIT-Interface.html
   and binutils-gdb/gdb/jit.{c,h}.

   The inferior publishes generated code to GDB via two well-known symbols:

     __jit_debug_descriptor    -- a singleton struct read by GDB
     __jit_debug_register_code -- a no-op function GDB sets a breakpoint on

   Each registration: malloc a jit_code_entry, splice into the doubly-linked
   list, set descriptor->relevant_entry / action_flag, call the trampoline.

   This box is Mach-O so GDB JIT is not testable here; the symfile produced
   is ELF and intended for x86-64 Linux GDB.
   =========================================================================== */

typedef enum {
  JIT_NOACTION = 0,
  JIT_REGISTER = 1,
  JIT_UNREGISTER = 2
} jit_actions_t;

struct jit_code_entry {
  struct jit_code_entry *next_entry;
  struct jit_code_entry *prev_entry;
  const char *symfile_addr;
  uint64_t symfile_size;
};

struct jit_descriptor {
  uint32_t version;
  uint32_t action_flag;
  struct jit_code_entry *relevant_entry;
  struct jit_code_entry *first_entry;
};

/* GDB looks up these symbols by name in the inferior. They must have
   external linkage and the descriptor must be initialised to {1, 0, 0, 0}. */
struct jit_descriptor __jit_debug_descriptor = { 1, 0, 0, 0 };

/* GDB places a breakpoint at the entry of this function. The body is a
   memory-clobber asm so the compiler cannot optimise the call site away
   even after inlining/LTO. The noinline attribute keeps a real call site. */
void __attribute__((noinline)) __jit_debug_register_code(void) {
  __asm__ volatile("" ::: "memory");
}

/* Splice [entry] at the head of the descriptor's linked list, set
   relevant_entry/action_flag, and notify GDB. */
static void caml_jit_gdb_notify(struct jit_code_entry *entry,
                                jit_actions_t action) {
  __jit_debug_descriptor.relevant_entry = entry;
  __jit_debug_descriptor.action_flag = action;
  __jit_debug_register_code();
}

/* Register an in-memory ELF symfile with GDB.

   [symfile] is an Owee_buf.t (a 1-D int8_unsigned Bigarray). Its underlying
   bytes are off-heap so GDB sees a stable address. The OCaml caller is
   responsible for keeping the Bigarray alive (see Gdb_jit.handle).

   Returns the malloc'd jit_code_entry pointer as a nativeint, used later for
   unregistration. */
CAMLprim value caml_jit_gdb_register(value symfile) {
  CAMLparam1(symfile);
  CAMLlocal1(result);

  void *data = Caml_ba_data_val(symfile);
  intnat size = Caml_ba_array_val(symfile)->dim[0];

  struct jit_code_entry *entry =
    (struct jit_code_entry *)malloc(sizeof(struct jit_code_entry));
  if (entry == NULL)
    caml_failwith("Gdb_jit.register: malloc failed");

  entry->symfile_addr = (const char *)data;
  entry->symfile_size = (uint64_t)size;
  entry->prev_entry = NULL;
  entry->next_entry = __jit_debug_descriptor.first_entry;
  if (entry->next_entry != NULL)
    entry->next_entry->prev_entry = entry;
  __jit_debug_descriptor.first_entry = entry;

  caml_jit_gdb_notify(entry, JIT_REGISTER);

  result = caml_copy_nativeint((intnat)entry);
  CAMLreturn(result);
}

/* Unregister a previously registered entry. [entry_addr] is the nativeint
   returned by [caml_jit_gdb_register]. */
CAMLprim value caml_jit_gdb_unregister(value entry_addr) {
  CAMLparam1(entry_addr);

  struct jit_code_entry *entry =
    (struct jit_code_entry *)Nativeint_val(entry_addr);

  if (entry->prev_entry != NULL)
    entry->prev_entry->next_entry = entry->next_entry;
  else
    __jit_debug_descriptor.first_entry = entry->next_entry;
  if (entry->next_entry != NULL)
    entry->next_entry->prev_entry = entry->prev_entry;

  caml_jit_gdb_notify(entry, JIT_UNREGISTER);

  free(entry);
  CAMLreturn(Val_unit);
}
