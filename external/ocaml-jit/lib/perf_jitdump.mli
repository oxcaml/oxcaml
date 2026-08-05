(** Linux [perf] jitdump support.

    Writes the streaming format consumed by [perf inject --jit] (see
    linux/tools/perf/Documentation/jitdump-specification.txt). The dump file
    lives under [$JITDUMP_DIR] (default [~/.debug/jit/]) as
    [jit-<pid>.dump]. At [init] time we briefly [mmap] the file's header
    with [PROT_READ | PROT_EXEC] and immediately [munmap]; the kernel emits
    a [PERF_RECORD_MMAP] event at the moment of the [mmap] syscall, which
    is the cue [perf inject --jit] uses to associate the dump with this
    process.

    On non-Linux platforms [init] returns [None]; the rest of the API is
    a no-op. *)

type handle

(** [init ~elf_mach] opens the jitdump file for the current process, writes
    the header, and establishes the [PROT_EXEC] mmap marker. [elf_mach] is
    the [EM_*] value (e.g. [0x3E] for x86-64, [0xB7] for arm64) that goes in
    the header. Returns [None] on non-Linux, on unsupported [elf_mach], or on
    any error during setup. Best-effort — never raises. *)
val init : elf_mach:int -> handle option

(** Append a [JIT_CODE_LOAD] record describing one JIT'd function. [code] is
    the relocated machine code at runtime address [code_addr]; its length
    must equal [code_size]. Best-effort: errors are swallowed. *)
val emit_code_load :
  handle ->
  name:string ->
  code_addr:int64 ->
  code_size:int ->
  code:string ->
  unit
