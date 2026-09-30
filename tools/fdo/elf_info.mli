(** The little we need to know about the profiled executable's ELF file. *)

type t

val read : string -> t

(** Whether the executable is position-independent (ET_DYN): loaded at an
    address chosen at run time, so that sampled addresses match its link-time
    addresses only after translation (see [Perf_script]). *)
val is_pie : t -> bool

(** The contents of the named section, if present. SHF_COMPRESSED sections are
    decompressed with the compiler's configured objcopy. *)
val section_body : t -> string -> string option

(** A byte at a link-time address in a file-backed executable load segment.
    Returns None for unavailable code, including addresses in other binaries. *)
val code_byte : t -> int64 -> int option

(** The link-time address of the byte at the given file offset, if some loadable
    segment (PT_LOAD program header) maps it. Together with the file offset and
    address of a mapping perf recorded, this translates a runtime address to a
    link-time one. *)
val address_of_offset : t -> int64 -> int64 option
