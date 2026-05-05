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
without knowing the concrete section type. X86 implements it via
`internal_assembler`; arm64 returns `None` for now.

```
module type Gdb_jit_symfile = sig
  type assembled_section
  val build :
    sections:(string * assembled_section) list ->
    section_address:(string -> int64 option) ->
    Compiler_owee.Owee_buf.t option
end
```

`internal_assembler` gains a new public entry point that:

- Takes already-assembled `X86_binary_emitter.section`s (no DWARF callback).
- Takes a `section_address : Section_name.t -> int64 option`.
- Skips `create_relocation_tables` / `make_relocation_section`.
- Sets `sh_addr` for `SHF_ALLOC` sections from `section_address`.
- Allocates a `Bigarray.Array1` of the right size and writes into it
  instead of mmap'ing a file.
- Returns the `Owee_buf.t`.

A new module `external/ocaml-jit/lib/gdb_jit.{ml,mli}` owns the protocol.
The `Owee_buf.t` (Bigarray) is held in OCaml so it stays live; the
`jit_code_entry` itself is `malloc`'d by the C stubs (so it has a stable
address GDB can read). C side defines `__jit_debug_descriptor` and
`__jit_debug_register_code`.

`external/ocaml-jit/lib/jit.ml` calls the new path after `load_text` /
`load_sections` and stashes handles in `Globals` so they outlive the
compilation phrase.

## File-level changes

1. `backend/binary_emitter_intf/binary_emitter_intf.ml`
   - Add `module type Gdb_jit_symfile`.
   - Add `module Gdb_jit_symfile : Gdb_jit_symfile with type assembled_section = Assembled_section.t` to `module type S`.

2. `backend/internal_assembler/internal_assembler.{ml,mli}`
   - Factor the inner ELF-writing logic so it can run on pre-assembled
     sections (skipping `get_sections`'s assemble + DWARF passes).
   - Add `val assemble_in_memory :
       sections:(X86_proc.Section_name.t * X86_binary_emitter.section) list ->
       section_address:(X86_proc.Section_name.t -> int64 option) ->
       Compiler_owee.Owee_buf.t`.
   - Internally allocate a Bigarray of computed total size and write into it.
   - In the SHF_ALLOC section creators, look up `section_address name` and
     use it for `sh_addr` (default `0L`).
   - Skip relocation table creation entirely on this path.

3. `backend/x86_binary_emitter.{ml,mli}`
   - In `For_jit`, add `module Gdb_jit_symfile` that converts the input
     list to the form `internal_assembler.assemble_in_memory` wants and
     calls it; returns `Some buf`.
   - The public `.mli` doesn't need extra signature work because `For_jit
     : Binary_emitter_intf.S` propagates the new submodule automatically.

4. `backend/arm64/binary_emitter/for_jit.ml`
   - Add a `Gdb_jit_symfile` stub returning `None`.

5. `external/ocaml-jit/lib/jit_stubs.c`
   - Add `__jit_debug_descriptor` (version=1) and `__jit_debug_register_code`.
   - Add `caml_jit_gdb_register` taking a Bigarray; mallocs an entry,
     splices it in, sets action_flag, calls the trampoline. Returns the
     entry pointer as a nativeint.
   - Add `caml_jit_gdb_unregister` for completeness (may not call it in
     v1).

6. `external/ocaml-jit/lib/gdb_jit.{ml,mli}` (new)
   - `type handle` — record holding `bigarray : Owee_buf.t` (root) and
     `entry_ptr : nativeint`.
   - `val register : Owee_buf.t -> handle`.
   - `val unregister : handle -> unit`.

7. `external/ocaml-jit/lib/jit.ml`
   - In `jit_load`, after `load_text`/`load_sections`, call the emitter's
     `For_jit.Gdb_jit_symfile.build`, passing:
     - `sections`: the binary-section map flattened to `(string,
       assembled_section) list` (just `.text` plus the other sections;
       relocations have already been applied).
     - `section_address`: lookup against the `addressed_text` and
       `addressed_sections` maps.
   - If `Some buf`, call `Gdb_jit.register buf`, store the handle in
     `Globals`.

8. `external/ocaml-jit/lib/globals.{ml,mli}`
   - Add a `gdb_handles : Gdb_jit.handle list ref` (or a Stack) so
     handles stay reachable.

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
