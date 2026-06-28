# Linux `perf` jitdump support for OxCaml's in-process JIT

## Goal

Make code produced by `external/ocaml-jit` visible to `perf record` /
`perf inject --jit` / `perf report`, on Linux x86-64 and arm64. Independent
of the GDB JIT support already in place; the two run side by side.

## Scope of v1

- Emit the **jitdump** stream (the modern format, supported by `perf inject
  --jit`). Skip the older `/tmp/perf-PID.map` text format entirely.
- Emit one `JIT_CODE_LOAD` record **per non-label symbol** in the JIT'd
  phrase, with `code_size` set to the byte distance to the next symbol (or
  end of the assembled section for the last). This gives `perf report`
  per-OCaml-function attribution rather than collapsing the whole phrase
  under one name.
- No `JIT_CODE_DEBUG_INFO`. No `JIT_CODE_UNWINDING_INFO`. (See "Deferred"
  below.)
- Linux only; on macOS the writer is a no-op (the format is Linux-perf-
  specific).

## How `perf inject --jit` discovers the dump

`perf record` captures every `mmap(2)` as a `PERF_RECORD_MMAP` event.
`perf inject --jit` later scans `perf.data` for executable mmaps of paths
matching `jit-<pid>.dump` and pulls debug info out of those files. So the
JIT must:

1. Open `$JITDUMP_DIR/jit-<pid>.dump` (defaulting to `~/.debug/jit/`),
   creating directories as needed.
2. Write the 40-byte file header.
3. `mmap(fd, header_size, PROT_READ | PROT_EXEC, MAP_PRIVATE, ...)` once,
   **before** the first JIT'd function executes, then `munmap` immediately.
   The kernel emits the `PERF_RECORD_MMAP` event at the moment of the
   `mmap(2)` syscall (it's not tied to the mapping's lifetime), so as long
   as `perf record` is already running we can release the VMA right away.
   Per the source article's footnote: *"You can unmap the JIT dump file
   immediately if you want."*
4. Append records for each JIT'd phrase as they are produced.

Steps 1–3 happen once at first phrase load (lazy init); step 4 runs
per-phrase.

## File format (per the in-tree linux/tools/perf/Documentation/jitdump-specification.txt and linux/tools/perf/util/jitdump.h)

### File header (40 bytes)

| Offset | Field        | Type     | Value                                 |
| ------ | ------------ | -------- | ------------------------------------- |
| 0      | `magic`      | u32      | `0x4A695444` (`"JiTD"`, native endian) |
| 4      | `version`    | u32      | `1`                                   |
| 8      | `total_size` | u32      | `40`                                  |
| 12     | `elf_mach`   | u32      | `EM_X86_64` (62) or `EM_AARCH64` (183) |
| 16     | `pad1`       | u32      | `0`                                   |
| 20     | `pid`        | u32      | `getpid()`                            |
| 24     | `timestamp`  | u64      | `clock_gettime(CLOCK_MONOTONIC)` ns   |
| 32     | `flags`      | u64      | `0`                                   |

### Per-record common prefix (16 bytes)

| Offset | Field        | Type | Notes                                              |
| ------ | ------------ | ---- | -------------------------------------------------- |
| 0      | `id`         | u32  | Record type (0 = LOAD, 2 = DEBUG_INFO, 4 = UNWIND) |
| 4      | `total_size` | u32  | Record size, including this prefix                 |
| 8      | `timestamp`  | u64  | `CLOCK_MONOTONIC` ns                               |

### `JIT_CODE_LOAD` (id=0) body

```
pid: u32        // == file-header pid
tid: u32        // we use pid (single-threaded JIT)
vma: u64        // runtime address (== code_addr for us)
code_addr: u64
code_size: u64
code_index: u64 // monotonic counter in Globals
name: char[N]   // null-terminated, e.g. caml<phrase>__entry
native_code: u8[code_size]  // raw relocated bytes
```

`total_size` = 16 + 40 + len(name)+1 + code_size.

### Ordering rule

`JIT_CODE_DEBUG_INFO` for a function (when we add it) **must precede** the
function's `JIT_CODE_LOAD`. v1 emits no DEBUG_INFO so this doesn't bite,
but the writer should be designed so debug records are emitted first.

## Mapping to existing code

Almost everything is already at hand inside `external/ocaml-jit/lib/jit.ml`'s
`jit_load`, at the same point where we register the GDB symfile. We emit
one record per non-label symbol:

| jitdump field      | Source                                                |
| ------------------ | ----------------------------------------------------- |
| `vma` / `code_addr` | `Address.to_int64 text.address + offset` of the symbol |
| `code_size`        | distance to the next symbol's offset, or `E.Assembled_section.size raw - offset` for the last symbol |
| `name`             | `Asm_symbol.encode` / `Asm_label.encode` of each non-label `iter_labels_and_symbols` target |
| `native_code`      | `String.sub (E.Assembled_section.contents raw) offset code_size` (assembled bytes only — GOT/PLT padding is *not* covered by any record) |
| `code_index`       | a fresh counter held inside the `Perf_jitdump.handle` |
| `pid` / `tid`      | `Unix.getpid ()` (single-threaded JIT)                |
| `timestamp`        | `clock_gettime(CLOCK_MONOTONIC)` via a tiny C stub    |
| `elf_mach`         | dispatched on `Target_system.architecture ()` by `jit.ml`, then passed to `Perf_jitdump.init` |

We have neither EH frame nor DWARF line info handy in the JIT path today —
hence the v1 scope.

## File-level changes

1. `external/ocaml-jit/lib/perf_jitdump.{ml,mli}` (new)
   - `val init : elf_mach:int -> handle option`
     - On non-Linux or when `elf_mach = 0` (architecture not supported),
       returns `None`.
     - On Linux: opens `~/.debug/jit/jit-<pid>.dump` (or
       `$JITDUMP_DIR/...`), writes the 40-byte header, briefly `mmap`s
       it `PROT_EXEC` and immediately `munmap`s, returns an opaque
       `handle` wrapping the fd and a `code_index` counter.
     - Best-effort: any error during setup yields `None`; the function
       never raises.
   - `val emit_code_load : handle -> name:string -> code_addr:int64 ->
        code_size:int -> code:string -> unit`
     - Builds and appends a `JIT_CODE_LOAD` record. (We pass the bytes
       as a `string` because `Jit_text_section.content` /
       `E.Assembled_section.contents` already return strings.)
     - On any write failure, latches a `disabled` flag in the handle so
       subsequent calls become no-ops; this avoids appending to a
       now-truncated and unparseable dump.
   - No mutex; ocaml-jit is single-threaded and we don't link against
     `threads`.
   - The fd is left open for the process lifetime; the kernel closes it at
     exit. `Unix.write` is unbuffered, so each record is flushed
     immediately.

2. `external/ocaml-jit/lib/perf_jitdump_stubs.c` (new)
   - Three C stubs the OCaml side calls:
     - `caml_perf_jitdump_is_linux` — runtime check used by the OCaml side
       to bail out on non-Linux without conditionalising the externals.
     - `caml_perf_jitdump_clock_monotonic` —
       `clock_gettime(CLOCK_MONOTONIC)` returning ns as int64.
     - `caml_perf_jitdump_mmap_marker` — `mmap(fd, size, PROT_READ |
       PROT_EXEC, MAP_PRIVATE, 0)` followed by an immediate `munmap` so
       the only effect is a single `PERF_RECORD_MMAP` event in the
       perf ring buffer.
   - File open and getpid are done from OCaml via `Unix.openfile` /
     `Unix.getpid`.
   - Linux-specific code is wrapped in `#ifdef __linux__`; non-Linux
     builds compile but never call into those branches.

3. `external/ocaml-jit/lib/globals.{ml,mli}`
   - `val perf_jitdump : Perf_jitdump.handle option option ref` — the
     outer `option` distinguishes "not yet initialised" from "init was
     attempted". This caches init failure so we don't retry on every
     phrase. `None None` is the initial state; `None (Some _)` is success;
     `None None` after init means init failed and we stop trying.

4. `external/ocaml-jit/lib/jit.ml`
   - In `register_with_gdb`'s sibling location (right after `load_text` /
     `load_sections`, before `jit_run`), call a new `register_with_perf`
     that:
     1. Lazily initialises `Globals.perf_jitdump`, dispatching `elf_mach`
        from `Target_system.architecture ()` (`X86_64 -> 0x3E`,
        `AArch64 -> 0xB7`, otherwise `0`).
     2. Iterates `E.Assembled_section.iter_labels_and_symbols` over the
        phrase's text section, drops names beginning with `.L`, sorts by
        offset, and emits one `JIT_CODE_LOAD` per symbol with
        `code_size = next_offset - this_offset` (or
        `assembled_size - offset` for the last symbol),
        `code_addr = text.address + offset`, and `code` sliced from
        `E.Assembled_section.contents`.
   - Independent of the GDB-symfile path; both run in `jit_load`.

5. `external/ocaml-jit/lib/dune`
   - `(foreign_stubs ... (names jit_stubs perf_jitdump_stubs))`.

## Out of scope (v1) — both deferred to a follow-up that needs DWARF

We will NOT emit these in v1 because both depend on DWARF the JIT
pipeline does not currently produce:

- **`JIT_CODE_DEBUG_INFO` (id=2) — source-line annotation.**
  Needs a per-function table mapping `code_addr → (file, line, discrim)`.
  The compiler's DWARF emission produces `.debug_line` for the on-disk
  path; the JIT pipeline currently calls neither
  `Emitaux.Dwarf_helpers.emit_delayed_dwarf` nor anything that materialises
  a line table for in-memory sections. Without this, `perf annotate` on
  JIT'd code shows assembly only — same as today's no-DWARF GDB symfile.

- **`JIT_CODE_UNWINDING_INFO` (id=4) — backtraces through JIT'd frames.**
  Needs an `.eh_frame` (FDE/CIE blob) and a `.eh_frame_hdr` for the JIT'd
  region. Same root cause: the JIT path doesn't emit `.eh_frame`. Without
  this, `perf record -g` stops unwinding at the first JIT'd frame; samples
  inside JIT code still attribute correctly (the LOAD records suffice for
  that), but parents in the call stack are missing.

Both will land together when the JIT path starts producing DWARF — at
which point the GDB symfile path can also gain `.debug_*` sections,
giving source-level stepping in addition to function-level symbols.

## Caveats / known gaps for v1

- **Embedding raw native bytes inflates the dump file**: roughly the same
  size as the JIT'd code itself. Acceptable for normal use; could be
  staggering for code-heavy workloads. There is no streaming alternative
  in the format.
- **`perf record` must be running before the JIT starts.** The marker is
  a one-shot `mmap`/`munmap` pair, so the `PERF_RECORD_MMAP` event is
  emitted exactly once and then gone. Attaching `perf record` to an
  already-running JIT process won't see the marker; only the dump file
  itself remains on disk. (If we ever care about attach-to-running, we'd
  switch to keeping the mapping alive so `perf` can synthesize an MMAP
  event from `/proc/<pid>/maps` on attach.)
- **`$HOME/.debug/jit/` directory**: created lazily. If `$HOME` is unset
  or unwritable, fall back to `$TMPDIR` or `/tmp`. Best-effort.
- **`perf record` clock**: must run with `-k mono` (or `--clockid
  monotonic`) for our timestamps to line up. Worth documenting in the JIT
  README later.
- **Tests**: cannot exercise on the current Mach-O dev box. v1 must be
  reviewed by inspection; runtime verification needs a Linux host.
- **GOT/PLT regions are not covered by any `JIT_CODE_LOAD`.** Per-symbol
  emission stops at the assembled section boundary; the GOT and PLT bytes
  appended afterwards by `Jit_text_section.in_memory_size` are excluded.
  Samples that land inside a PLT stub will be unattributed in `perf
  report`. This contrasts with the GDB symfile path, where `.text`'s
  `sh_size` deliberately spans the GOT/PLT padding so backtraces don't
  fall into a "no module" hole. Acceptable for a profiler in v1; revisit
  if it bites.
