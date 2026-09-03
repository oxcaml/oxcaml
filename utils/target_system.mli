module Architecture : sig
  type t =
    | IA32
    | X86_64
    | ARM
    | AArch64
    | POWER
    | Z
    | Riscv

  val get : unit -> t

  val is_arm : unit -> bool

  val is_64_bit : unit -> bool

  (* CR mshinwell: what happens about these functions for JSIR? *)
  val is_32_bit : unit -> bool

  val to_string : t -> string
end

module System : sig
  type windows_system = private
    | MinGW
    | MSVC

  (* CR sspies: Remove some of the systems below that are a bit dated. *)
  type t = private
    | Linux
    | Windows of windows_system
    | Cygwin
    | MacOS
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Solaris
    | Dragonfly
    | GNU
    | BeOS

  val get : unit -> t

  val is_macos : unit -> bool
end

module Toolchain : sig
  type assembler =
    | GAS_like
    | MacOS
    | MASM

  val assembler : unit -> assembler

  val is_macos : unit -> bool

  val is_gas : unit -> bool

  val is_masm : unit -> bool

  val is_windows_or_cygwin : unit -> bool

  val label_prefix : unit -> string
end

module Machine_width : sig
  type t =
    | Thirty_two (* Traditional 32-bit OCaml with GC tag bit *)
    | Thirty_two_no_gc_tag_bit (* JavaScript mode with full 32-bit integers *)
    | Sixty_four (* Traditional 64-bit OCaml with GC tag bit *)

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val is_32_bit : t -> bool

  val is_64_bit : t -> bool

  val size_in_bytes : t -> int
end
