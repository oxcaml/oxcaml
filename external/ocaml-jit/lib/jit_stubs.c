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
#ifdef CAML_RUNTIME_5
#include "caml/frame_descriptors.h"
#define NATIVE_CODE
#include "caml/globroots.h"
#endif
#include "caml/memory.h"
#include "caml/stack.h"
#include "caml/callback.h"
#include "caml/alloc.h"
#include "caml/osdeps.h"
#include "caml/codefrag.h"
#include "caml/fail.h"
#ifdef CAML_RUNTIME_5
#include "caml/unloadable.h"
#endif

#include <assert.h>
#include <stdbool.h>
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

/* True if the build uses an allocation backend that supports unloading the
 * JIT'd buffer when its CU becomes unreachable. The unload path calls [free]
 * on the buffer (see [jit_unit_on_unload]), which is well-defined only when
 * the buffer was returned by [aligned_alloc].
 *
 * Disabled on three Linux configurations:
 *   - [musl]: [jit_memalign] uses a static [.bss] arena (musl's malloc mixes
 *     [sbrk] and [mmap], breaking relocations), so [free] would corrupt
 *     unrelated process state.
 *   - AddressSanitizer: [jit_memalign] uses [sbrk] (because ASan's
 *     intercepted [aligned_alloc] returns high addresses out of relocation
 *     range), and ASan reports the resulting [free] as a SEGV.
 *   - TCMalloc: same [sbrk] path, and TCMalloc's [free] does not know about
 *     pointers returned from [sbrk].
 *
 * On macOS, ASan-instrumented [aligned_alloc] is paired with an ASan-aware
 * [free], so unloading remains supported.
 *
 * When unloading is disabled, [Eval.eval] still works: the unit is compiled
 * as non-unloadable (black-headered data, no Code_block scaffolding), and
 * the buffer is leaked for the life of the process. */
CAMLprim value jit_supports_unloading(value unit) {
  CAMLparam1(unit);
  int supports;
  if (MUSL) {
    supports = 0;
  } else if (ASAN_IS_ENABLED
#ifdef __linux__
             || TCMalloc_MallocExtension_MallocIsTCMalloc()
#endif
  ) {
#if defined(__APPLE__)
    supports = 1;
#else
    supports = 0;
#endif
  } else {
    supports = 1;
  }
  CAMLreturn(Val_bool(supports));
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
#ifdef CAML_RUNTIME_5
    caml_register_frametables(&sym, 1);
#else
    caml_register_frametable(sym);
#endif
  }

  sym = addr_from_caml_option(Field(symbols_addresses, 1));
  if (NULL != sym) {
#ifdef CAML_RUNTIME_5
    caml_register_dyn_globals(&sym, 1);
#else
    caml_register_dyn_global(sym);
#endif
  }

#ifndef CAML_RUNTIME_5
  sym = addr_from_caml_option(Field(symbols_addresses, 2));
  sym2 = addr_from_caml_option(Field(symbols_addresses, 3));
  if (NULL != sym && NULL != sym2)
    caml_page_table_add(In_static_data, sym, sym2);
#endif

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

#ifdef CAML_RUNTIME_5
/* Loader-private state stashed in [u->loader_data] for [jit_unit_on_unload]
 * to use. The base/size describe the page-aligned buffer the JIT loader
 * reserved for the unit's text and data sections (see [jit_memalign]). */
struct jit_unit_loader_data {
  void *buffer_base;
  size_t buffer_size;
};

static void jit_unit_on_unload(struct caml_unloadable_unit *u) {
  /* Called from STW once the GC has determined the unit is unreachable.
   * Frees the per-unit metadata arrays, restores the JIT buffer to RW so
   * [free] is well-defined, and releases the buffer back to the allocator.
   *
   * This path only runs when unloading is supported (see
   * [jit_supports_unloading]); under that gate [jit_memalign] always uses
   * [aligned_alloc], so [free] here is well-defined. */
  struct jit_unit_loader_data *ld =
      (struct jit_unit_loader_data *)u->loader_data;

  caml_stat_free(u->code_blocks);
  caml_stat_free(u->data_blocks);
  caml_stat_free(u->text_ranges);
  caml_stat_free(u->text_range_fragnums);

  if (ld != NULL) {
    if (ld->buffer_base != NULL && ld->buffer_size > 0) {
      /* Restore RW protection on the entire buffer before [free]: parts
       * have been mapped RX (text) or RO (.rodata) and the allocator may
       * touch arbitrary regions of the buffer when reclaiming. The
       * stored [buffer_size] is rounded up to the page size at
       * registration time so [mprotect] does not affect memory past
       * the buffer. */
      (void)mprotect(ld->buffer_base, ld->buffer_size, PROT_READ | PROT_WRITE);
      free(ld->buffer_base);
    }
    caml_stat_free(ld);
  }

  caml_stat_free(u);
}

/* Build and register an [caml_unloadable_unit] for a JIT-emitted compilation
 * unit. Inputs are nativeint arrays / scalars from the OCaml side:
 *   - [code_blocks]: addresses of the unit's [Code_block] static-data items
 *     (one per function defined in the unit).
 *   - [data_blocks_table_addr]: address of a static array, emitted by the
 *     compiler in the unit's data section, enumerating the unit's static
 *     data blocks (those emitted with white/UNMARKED headers per B.1). The
 *     array layout is [count; addr_1; ...; addr_count] where [count] is one
 *     [intnat]-sized word and each address is [intnat]-sized. May be 0n if
 *     the unit has no static data blocks.
 *   - [function_entries]: addresses of each function's entry, sorted in
 *     ascending order. Used to derive per-function text ranges:
 *     [function_entries[i] .. function_entries[i+1])  (and the last entry
 *     extends to [code_end]). Per-function fragments are necessary so that
 *     F.2 (stack-RA scan) can recover the entry from a return address via
 *     [caml_find_code_fragment_by_pc]->code_start.
 *   - [code_end_addr]: address of the unit's [code_end] symbol; the upper
 *     bound of the last function's text range.
 *   - [frametable_addr]: address of the unit's frame table (or 0 if none).
 *   - [buffer_base_addr]: base of the JIT-allocated buffer covering both
 *     text and data; passed through to the on_unload callback for [free].
 *   - [buffer_size]: size of that buffer in bytes.
 *
 * The struct and its arrays are allocated via [caml_stat_alloc_noexc]; the
 * runtime keeps them alive until the unit is unloaded, at which point the
 * installed [on_unload] callback releases everything. */
CAMLprim value jit_register_unloadable_unit_native(
    value code_blocks, value data_blocks_table_addr, value function_entries,
    value code_end_addr, value frametable_addr, value gc_roots_addr,
    value buffer_base_addr, value buffer_size) {
  CAMLparam5(code_blocks, data_blocks_table_addr, function_entries,
             code_end_addr, frametable_addr);
  CAMLxparam3(gc_roots_addr, buffer_base_addr, buffer_size);

  uintnat n_code = Wosize_val(code_blocks);
  uintnat n_funcs = Wosize_val(function_entries);

  intnat *data_blocks_table =
      (intnat *)Nativeint_val(data_blocks_table_addr);
  uintnat n_data = (data_blocks_table == NULL) ? 0
                                               : (uintnat)data_blocks_table[0];

  /* Allocate every owned buffer up-front and check each. On any failure,
   * free everything allocated so far and raise. This avoids the
   * longjmp-leaks-prior-allocations pattern of a cascade of
   * [caml_stat_alloc_noexc] + [caml_raise_out_of_memory]. */
  struct caml_unloadable_unit *u =
      caml_stat_alloc_noexc(sizeof(struct caml_unloadable_unit));
  struct jit_unit_loader_data *ld =
      caml_stat_alloc_noexc(sizeof(struct jit_unit_loader_data));
  value *code_blocks_buf =
      (n_code > 0) ? caml_stat_alloc_noexc(n_code * sizeof(value)) : NULL;
  value *data_blocks_buf =
      (n_data > 0) ? caml_stat_alloc_noexc(n_data * sizeof(value)) : NULL;
  char **text_ranges_buf =
      (n_funcs > 0) ? caml_stat_alloc_noexc(2 * n_funcs * sizeof(char *))
                    : NULL;
  int *text_range_fragnums_buf =
      (n_funcs > 0) ? caml_stat_alloc_noexc(n_funcs * sizeof(int)) : NULL;

  if (u == NULL || ld == NULL
      || (n_code > 0 && code_blocks_buf == NULL)
      || (n_data > 0 && data_blocks_buf == NULL)
      || (n_funcs > 0 && text_ranges_buf == NULL)
      || (n_funcs > 0 && text_range_fragnums_buf == NULL)) {
    caml_stat_free(u);
    caml_stat_free(ld);
    caml_stat_free(code_blocks_buf);
    caml_stat_free(data_blocks_buf);
    caml_stat_free(text_ranges_buf);
    caml_stat_free(text_range_fragnums_buf);
    caml_raise_out_of_memory();
  }

  u->next = NULL;
  u->num_code_blocks = n_code;
  u->num_data_blocks = n_data;
  u->num_text_ranges = n_funcs;
  u->frametable = (intnat *)Nativeint_val(frametable_addr);
  u->gc_roots = (void *)Nativeint_val(gc_roots_addr);
  u->on_unload = &jit_unit_on_unload;
  u->code_blocks = code_blocks_buf;
  u->data_blocks = data_blocks_buf;
  u->text_ranges = text_ranges_buf;
  u->text_range_fragnums = text_range_fragnums_buf;
  u->loader_data = ld;

  ld->buffer_base = (void *)Nativeint_val(buffer_base_addr);
  {
    /* Round [buffer_size] up to the page size: the buffer came from
     * [aligned_alloc(page_size, ...)] in [jit_memalign], so the kernel-
     * visible mapping covers at least the page-aligned length. Storing
     * the page-aligned size makes [mprotect] in [jit_unit_on_unload]
     * self-consistent and avoids relying on implementation-defined
     * "round length up" behaviour. */
    size_t raw = (size_t)Long_val(buffer_size);
    size_t pg = (size_t)getpagesize();
    ld->buffer_size = (raw + pg - 1) & ~(pg - 1);
  }

  for (uintnat i = 0; i < n_code; i++) {
    u->code_blocks[i] = (value)Nativeint_val(Field(code_blocks, i));
  }

  for (uintnat i = 0; i < n_data; i++) {
    /* The array entries are stored as raw addresses (Csymbol_address, which
     * the assembler emits as machine words). */
    u->data_blocks[i] = (value)data_blocks_table[1 + i];
  }

  /* Per-function text ranges: [entry_i .. entry_{i+1}); last extends to
   * [code_end]. Each range starts at the function entry, NOT at the
   * back-pointer word at [entry - 1]. PC-based fragment lookups for
   * [entry - 1] therefore miss — this is intentional: the designed
   * paths (return-address scan, code-pointer-slot scan) both start
   * from in-function PCs. Do not expand the range to cover the
   * back-pointer. */
  char *code_end = (char *)Nativeint_val(code_end_addr);
  for (uintnat i = 0; i < n_funcs; i++) {
    char *start = (char *)Nativeint_val(Field(function_entries, i));
    char *end = (i + 1 < n_funcs)
                    ? (char *)Nativeint_val(Field(function_entries, i + 1))
                    : code_end;
    u->text_ranges[2 * i] = start;
    u->text_ranges[2 * i + 1] = end;
  }

  caml_register_unloadable_unit(u);
  CAMLreturn(Val_unit);
}

CAMLprim value jit_register_unloadable_unit_bytecode(value *argv, int argn) {
  (void)argn;
  return jit_register_unloadable_unit_native(argv[0], argv[1], argv[2],
                                             argv[3], argv[4], argv[5],
                                             argv[6], argv[7]);
}
#else
CAMLprim value jit_register_unloadable_unit_native(
    value a, value b, value c, value d, value e, value f, value g, value h) {
  /* Unloadable units require runtime 5 (concurrent marker). */
  (void)a; (void)b; (void)c; (void)d; (void)e; (void)f; (void)g; (void)h;
  return Val_unit;
}

CAMLprim value jit_register_unloadable_unit_bytecode(value *argv, int argn) {
  (void)argn;
  return jit_register_unloadable_unit_native(argv[0], argv[1], argv[2],
                                             argv[3], argv[4], argv[5],
                                             argv[6], argv[7]);
}
#endif
