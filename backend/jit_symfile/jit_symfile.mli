(** Generic in-memory ELF symfile builder for the GDB/lldb JIT interface.

    Produces an ET_REL ELF-64 object with sections placed at the runtime
    addresses provided by [section_address]. Used by the JIT to publish
    just-in-time-compiled code so debuggers can resolve symbols and addresses.

    Section bytes are taken verbatim from [sections] and assumed to be
    post-relocation. No [.rela*] sections are emitted: the JIT has already
    applied relocations to the live code, and GDB's JIT loader does not patch
    inferior memory. No DWARF is emitted in this minimal build; symbol names
    and addresses are sufficient for [info functions], backtraces by name,
    breakpoints by symbol, and disassembly. *)

val build :
  (module Binary_emitter_intf.Assembled_section
     with type t = 'a
      and type relocation = 'r) ->
  e_machine:int ->
  sections:(string * 'a) list ->
  section_address:(string -> int64 option) ->
  section_runtime_size:(string -> int option) ->
  Compiler_owee.Owee_buf.t
