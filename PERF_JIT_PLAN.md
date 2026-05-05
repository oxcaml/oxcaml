# Linux `perf` jitdump support for OxCaml's in-process JIT

## Goal

Make code produced by `external/ocaml-jit` visible to `perf record` /
`perf inject --jit` / `perf report`, on Linux x86-64 and arm64. Independent
of the GDB JIT support already in place; the two run side by side.

## Scope of v1

- Emit the **jitdump** stream (the modern format, supported by `perf inject
  --jit`). Skip the older `/tmp/perf-PID.map` text format entirely.
- Emit one `JIT_CODE_LOAD` record per JIT'd phrase. Function name +
  address + native bytes; enough for `perf report` to attribute samples to
  named OCaml functions.
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
2. Write the 48-byte file header.
3. `mmap(fd, header_size, PROT_READ | PROT_EXEC, MAP_PRIVATE, ...)` once,
   **before** the first JIT'd function executes. Never unmap (the mapping is
   the marker; if it goes away `perf inject` won't find the dump).
4. Append records for each JIT'd phrase as they are produced.

Steps 1–3 happen once at JIT startup; step 4 runs per-phrase.

## File format (per the in-tree linux/tools/perf/Documentation/jitdump-specification.txt)

### File header (48 bytes)

| Offset | Field        | Type     | Value                                 |
| ------ | ------------ | -------- | ------------------------------------- |
| 0      | `magic`      | u32      | `0x4A695444` (`"JiTD"`, native endian) |
| 4      | `version`    | u32      | `1`                                   |
| 8      | `total_size` | u32      | `48`                                  |
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
`jit_load`, at the same point where we register the GDB symfile:

| jitdump field      | Source                                                |
| ------------------ | ----------------------------------------------------- |
| `vma` / `code_addr` | `Address.to_int64 text.address`                      |
| `code_size`        | `Jit_text_section.in_memory_size (module E) text.value` (covers GOT/PLT, like the GDB symfile's `.text`) |
| `name`             | the phrase entry symbol (we already compute this for `entry_points`) |
| `native_code`      | `Jit_text_section.content (module E) text.value` — the relocated `.text` plus GOT+PLT |
| `code_index`       | a fresh counter held in `Globals`                     |
| `pid` / `tid`      | `Unix.getpid ()`                                      |
| `timestamp`        | `clock_gettime(CLOCK_MONOTONIC)` via a tiny C stub    |
| `elf_mach`         | dispatched on `Target_system.architecture ()`         |

We have neither EH frame nor DWARF line info handy in the JIT path today —
hence the v1 scope.

## File-level changes

1. `external/ocaml-jit/lib/perf_jitdump.{ml,mli}` (new)
   - `val init : unit -> handle option`
     - On non-Linux: returns `None` (whole feature disabled).
     - On Linux: opens `~/.debug/jit/jit-<pid>.dump` (or
       `$JITDUMP_DIR/...`), writes the header, mmaps it `PROT_EXEC`,
       returns a handle wrapping the `fd` and a counter ref.
   - `val emit_code_load : handle -> name:string -> code_addr:int64 ->
        code_size:int -> code:bytes -> unit`
     - Builds and appends a `JIT_CODE_LOAD` record.
   - Records and the `mmap` use a single mutex so concurrent emission is
     race-free (defensive — current JIT is single-threaded).
   - Closes the fd at exit; the mmap is left intact for the kernel to
     reclaim, so the file is still discoverable from saved perf.data after
     the process dies.

2. `external/ocaml-jit/lib/perf_jitdump_stubs.c` (new)
   - C stubs the OCaml side calls:
     - `caml_perf_jitdump_open` — opens the path with `O_CREAT|O_TRUNC|O_RDWR`.
     - `caml_perf_jitdump_mmap_marker` — `mmap` with `PROT_EXEC`.
     - `caml_perf_jitdump_clock_monotonic` — `clock_gettime(CLOCK_MONOTONIC)` ns.
     - `caml_perf_jitdump_getpid` (could just use OCaml's, but C is simpler).
   - Wrapped in `#ifdef __linux__` so the compile is a no-op on macOS;
     OCaml side checks `Sys.os_type` / `Target_system` and never calls
     them on non-Linux.

3. `external/ocaml-jit/lib/globals.{ml,mli}`
   - `val perf_jitdump : Perf_jitdump.handle option ref` — created lazily
     on first use; `None` on non-Linux.

4. `external/ocaml-jit/lib/jit.ml`
   - In `register_with_gdb`'s sibling location (right after
     `load_text` / `load_sections`, before `jit_run`), call a new
     `register_with_perf` that:
     1. Lazily inits `Globals.perf_jitdump`.
     2. For each phrase entry symbol it found, emits a `JIT_CODE_LOAD`
        with `code_addr = text.address`, `code_size =
        Jit_text_section.in_memory_size`, `code = Jit_text_section.content`,
        and a fresh `code_index`.
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
- **`mmap` marker leaks**: the `PROT_EXEC` mapping is intentionally never
  unmapped (it's the persistence cue for `perf inject --jit` after the
  process dies). Cost: one VMA per JIT process, size = header_size.
- **`$HOME/.debug/jit/` directory**: created lazily. If `$HOME` is unset
  or unwritable, fall back to `$TMPDIR` or `/tmp`. Best-effort.
- **`perf record` clock**: must run with `-k mono` (or `--clockid
  monotonic`) for our timestamps to line up. Worth documenting in the JIT
  README later.
- **Tests**: cannot exercise on the current Mach-O dev box. v1 must be
  reviewed by inspection; runtime verification needs a Linux host.
