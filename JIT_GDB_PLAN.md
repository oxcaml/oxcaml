# GDB JIT Interface for OxCaml's in-process JIT (x86-64 ELF)

## Goal

Make code produced by `external/ocaml-jit` visible to GDB via the GDB
JIT-Interface protocol, on x86-64 Linux. Cannot test on this Mach-O box;
implementation is by inspection only.

Scope of this first cut:
- x86-64 only.
- ELF symfile only (the format GDB's JIT loader consumes).
- **No DWARF.** GDB will get function names, addresses, and disassembly via
  the symtab; no source-level stepping. DWARF can be added later.
- Symfile bytes are taken to be already post-relocation (the JIT applies
  relocations to the in-memory section bytes before we hand them off).

## Protocol summary (verified against `binutils-gdb/gdb/jit.{c,h}`)

GDB looks up two well-known globals in the inferior:

- `__jit_debug_descriptor` — a `{ uint32_t version; uint32_t action_flag;
  jit_code_entry *relevant_entry; jit_code_entry *first_entry; }`. Initialised
  to `{ 1, 0, 0, 0 }`.
- `__jit_debug_register_code` — empty `noinline` function. GDB sets a
  breakpoint here.

To register a symfile: build a `jit_code_entry { next, prev, symfile_addr,
symfile_size }`, splice into the doubly-linked list rooted at
`first_entry`, set `relevant_entry`, set `action_flag = JIT_REGISTER (1)`,
call `__jit_debug_register_code()`. GDB:

1. Reads the entry from inferior memory.
2. `gdb_bfd_open_from_target_memory` reads `symfile_size` bytes at
   `symfile_addr` and parses them as a BFD object.
3. Walks `nbfd->sections`, takes `bfd_section_vma (sec)` (= ELF `sh_addr`)
   for each `SEC_ALLOC|SEC_LOAD` section, and treats those as **absolute**
   addresses. (`gdb/jit.c:786-798`.)
4. Calls `symbol_file_add_from_bfd` with `OBJF_SHARED|OBJF_NOT_FILENAME`.

GDB does not patch inferior memory. `.rela*` sections are not consulted.
Therefore for a JIT that has already applied its own relocations, we just
need to:

- Set `sh_addr` for each `SHF_ALLOC` section to the runtime address.
- Provide a symtab so symbols resolve to `sh_addr + st_value`.
- Optionally include or omit `.rela*` (we omit, since they describe
  relocations already applied to live code and are not used by GDB).

Unregister: same dance with `JIT_UNREGISTER (2)` and the entry removed from
the list.

The descriptor must be serialised across registrations; the JIT toplevel is
single-threaded so this is free.

## What we already have

- `backend/internal_assembler/` — full ELF-64 x86-64 writer (ET_REL, EM_X86_64,
  symtab/strtab/shstrtab, `.rela*` for `R_X86_64_{64,PLT32,GOTPCREL}`).
  Currently writes to disk via `Compiler_owee.Owee_buf.map_binary_write`.
- `Compiler_owee.Owee_buf.t` is a `Bigarray.Array1` of `int8_unsigned` —
  contiguous memory we can hand to GDB without copying. We can construct
  one directly with `Bigarray.Array1.create` (no need to add anything to
  Owee).
- `external/ocaml-jit/lib/jit.ml` already lays out sections at runtime
  addresses (`alloc_all`), applies relocations (`relocate_text`,
  `relocate_other`), and copies + mprotects pages (`load_text`,
  `load_sections`). At the moment `load_*` runs, every section we care about
  has a runtime address available in the `addressed` records.
- `backend/binary_emitter_intf/` — abstraction layer that lets ocaml-jit
  stay backend-agnostic. Both X86 and arm64 implement `For_jit :
  Binary_emitter_intf.S`.
- `backend/jit_backend.ml` — architecture dispatch that packs assembled
  sections into a `Packed { emitter; sections }` existential and hands them
  to ocaml-jit's callback.

## Design

A new submodule on `Binary_emitter_intf.S` named `Gdb_jit_symfile` lets
ocaml-jit stay backend-agnostic: it calls `For_jit.Gdb_jit_symfile.build`
without knowing the concrete section type. Both x86 and arm64 implement it
by delegating to a shared, generic ELF builder.

```
module type Gdb_jit_symfile = sig
  type assembled_section
  val build :
    sections:(string * assembled_section) list ->
    section_address:(string -> int64 option) ->
    section_runtime_size:(string -> int option) ->
    Compiler_owee.Owee_buf.t option
end
```

The generic builder lives in a new tiny library `backend/jit_symfile/` so
both `x86_binary_emitter` (in the main library) and `arm64_binary_emitter`
(a separate library) can depend on it without circularity. Its only inputs
are an `Assembled_section` first-class module (taking
`size`/`contents_mut`/`iter_labels_and_symbols`), the `e_machine` value, and
the per-section address/runtime-size callbacks. It allocates a
`Bigarray.Array1` of the right size, zero-fills it, and writes a complete
ET_REL ELF: ELF header, section bodies, symtab, strtab, shstrtab, section
header table. No `.rela*`, no DWARF.

The existing `Internal_assembler.assemble` (the on-disk path used by the
normal `.o`-emitting compiler driver) is unchanged.

A new module `external/ocaml-jit/lib/gdb_jit.{ml,mli}` owns the protocol.
The `Owee_buf.t` (Bigarray) is held in OCaml so it stays live; the
`jit_code_entry` itself is `malloc`'d by the C stubs (so it has a stable
address GDB can read). C side defines `__jit_debug_descriptor` and
`__jit_debug_register_code`.

`external/ocaml-jit/lib/jit.ml` calls the new path after `load_text` /
`load_sections` and stashes handles in `Globals` so they outlive the
compilation phrase.

## File-level changes

1. `backend/binary_emitter_intf/binary_emitter_intf.ml` (+ dune)
   - Add `module type Gdb_jit_symfile`.
   - Add `module Gdb_jit_symfile : Gdb_jit_symfile with type assembled_section = Assembled_section.t` to `module type S`.
   - Dune now depends on `compiler_owee` (because the signature mentions
     `Compiler_owee.Owee_buf.t`).

2. `backend/jit_symfile/` — new library.
   - `Jit_symfile.build` takes a `(module Binary_emitter_intf.Assembled_section)`,
     `e_machine`, sections, address/runtime-size callbacks; returns an
     in-memory `Owee_buf.t`.
   - Skips DWARF and relocation tables.
   - Symbols come from `iter_labels_and_symbols`; names starting `.L` are
     stripped (standard ELF convention). All emitted symbols are
     `STB_GLOBAL` with type heuristically `STT_FUNC` for text-like
     sections, `STT_OBJECT` for data-like, else `STT_NOTYPE`. (See
     "Caveats" below — this is a deliberate fidelity loss versus the
     existing `internal_assembler` path.)

3. `backend/x86_binary_emitter.ml`
   - In `For_jit`, add `module Gdb_jit_symfile` that calls
     `Jit_symfile.build (module Assembled_section) ~e_machine:0x3E …`.
   - The public `.mli` doesn't need extra signature work because
     `For_jit : Binary_emitter_intf.S` propagates the new submodule
     automatically.
   - `Internal_assembler` (the on-disk x86 ELF writer) is untouched.

4. `backend/arm64/binary_emitter/for_jit.ml` (+ dune)
   - Add `module Gdb_jit_symfile` calling
     `Jit_symfile.build (module Assembled_section) ~e_machine:0xB7 …`.
   - Dune depends on `jit_symfile`.

5. `dune` (top-level main library) — add `jit_symfile` dependency so
   `x86_binary_emitter` (which is in the main library) can use it.

6. `external/ocaml-jit/lib/jit_stubs.c`
   - Add `__jit_debug_descriptor` (version=1) and `__jit_debug_register_code`
     (`noinline` + memory-clobber asm).
   - Add `caml_jit_gdb_register` taking a Bigarray; mallocs an entry,
     splices it in at the head of the doubly-linked list, sets
     `action_flag = JIT_REGISTER`, calls the trampoline. Returns the entry
     pointer as a nativeint.
   - Add `caml_jit_gdb_unregister` for completeness.

7. `external/ocaml-jit/lib/gdb_jit.{ml,mli}` (new)
   - `type handle = { symfile : Owee_buf.t; entry : nativeint }` — keeps
     the Bigarray header live so the underlying off-heap data is not
     freed.
   - `val register : Owee_buf.t -> handle`.
   - `val unregister : handle -> unit`.

8. `external/ocaml-jit/lib/jit.ml`
   - New `register_with_gdb` runs after `load_text`/`load_sections`. Builds
     a per-section address table from `text.address` and
     `addressed_sections`, plus a single runtime-size override for `.text`
     equal to `Jit_text_section.in_memory_size` so the section spans the
     GOT and PLT padding. Calls `E.Gdb_jit_symfile.build`; if `Some buf`,
     calls `Gdb_jit.register` and stores the handle in `Globals`.

9. `external/ocaml-jit/lib/jit_text_section.{ml,mli}` — exposes
   `binary_section : ('a, _) t -> 'a` so jit.ml can pull out the underlying
   assembled section to feed to `Gdb_jit_symfile.build`.

10. `external/ocaml-jit/lib/globals.{ml,mli}`
    - Add `val gdb_jit_handles : Gdb_jit.handle list ref` so handles stay
      reachable for the process lifetime.

## Caveats (known loss-of-fidelity / leak)

- **Symbols emitted as global only.** `Jit_symfile` infers symbol type
  from the containing section's name and emits everything as
  `STB_GLOBAL`. The previous on-disk x86 path used the rich
  `X86_binary_emitter.symbol` metadata (`Sy_local`/`Sy_global`/`Sy_weak`,
  `Function`/`Object`/`None`, `sy_protected`). For OCaml code most
  user-visible symbols are global, so this is a cosmetic loss — GDB still
  finds everything by name.
- **`Globals.gdb_jit_handles` never pruned.** Every JIT phrase appends a
  handle that stays alive for the process. For a long-running JIT
  toplevel this is a slow leak. Pruning would require also tracking which
  handles correspond to which phrases, which we don't currently.
- **`Owee_elf.write_elf` writes section names into shstrtab at the
  computed `sh_name` offsets**, and `Jit_symfile` then writes the entire
  shstrtab linearly over the same region. Both produce the same bytes by
  construction; just an unimportant duplication of effort.

## Risks / unknowns

- Whether `bfd_check_format` accepts an ELF where some sections have
  `sh_addr != 0` and others (the non-loadable ones like `.symtab`,
  `.strtab`, `.shstrtab`) have `sh_addr = 0`. This is the normal case for
  ELF objects; it should be fine. Worth checking by running `objdump -h`
  on a sample symfile once we can build one.
- Whether the `e_machine` check at `gdb/jit.c:778-784` against the
  inferior's gdbarch needs `EM_X86_64` exactly (it does — already what
  `internal_assembler` emits).
- We hold the Bigarray live in OCaml, but the `jit_code_entry` is in C
  malloc. If OCaml's GC moves the Bigarray's underlying memory, GDB sees
  stale `symfile_addr`. Bigarrays are off-heap (`malloc`'d), so this is
  fine — but the Bigarray *header* in OCaml must stay live.

## Out of scope (later)

- DWARF (`.debug_*`).
- Source-level breakpoints.
- Mach-O symfile output (would need a Mach-O writer; not present today, and
  not necessary — see the lldb note below).

## Status update (after implementation)

- **x86-64 ELF**: implemented. `Jit_symfile.build` produces an in-memory ELF
  with sh_addr set; `register_with_gdb` in `external/ocaml-jit/lib/jit.ml`
  registers it with the GDB JIT protocol via `Gdb_jit.register`.
- **arm64 ELF**: implemented. Same pipeline; arm64 backend's
  `For_jit.Gdb_jit_symfile.build` calls `Jit_symfile.build` with
  `e_machine = EM_AARCH64` (0xB7).
- **lldb on macOS**: should work without further changes. Verified by reading
  `llvm-project/lldb/source/Plugins/JITLoader/GDB/JITLoaderGDB.cpp` and
  `Plugins/ObjectFile/ELF/ObjectFileELF.cpp`. Key facts:
  - `JITLoaderGDB` looks up `__jit_debug_descriptor` and
    `__jit_debug_register_code` exactly as GDB does. The plugin is registered
    via `PluginManager` and runs on any host (including Darwin).
  - `Process::ReadModuleFromMemory` constructs a `Module` with empty
    `ArchSpec()`, then `GetMemoryObjectFile` content-sniffs the bytes.
    `ObjectFileELF::CreateMemoryInstance` matches the ELF magic and parses
    the header, regardless of host platform.
  - In `JITLoaderGDB::ReadJITDescriptorImpl` (~L349), when the symfile is not
    Mach-O, lldb does `module_sp->SetLoadAddress(target, 0, true, changed)`,
    which makes sections appear at their `sh_addr` as-is. That's exactly what
    our symfile assumes.
- The `e_machine` value is taken from the symfile, not derived from the host
  triple. For an arm64-apple-darwin process, our `EM_AARCH64` ELF is what
  lldb expects to see.
