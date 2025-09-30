type architecture =
  | IA32
  | X86_64
  | ARM
  | AArch64
  | POWER
  | Z
  | Riscv

val architecture : unit -> architecture

val is_64_bit : unit -> bool

(* CR mshinwell: what happens about these functions for JSIR? *)
val is_32_bit : unit -> bool

type derived_system =
  | Linux
  | MinGW_32
  | MinGW_64
  | Win32
  | Win64
  | Cygwin
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Generic_BSD
  | Solaris
  | Dragonfly
  | GNU
  | BeOS
  | Unknown

val derived_system : unit -> derived_system

val is_windows : unit -> bool

type assembler =
  | GAS_like
  | MacOS
  | MASM

val assembler : unit -> assembler

module Machine_width : sig
  type t =
    | Thirty_two  (* Traditional 32-bit OCaml with GC tag bit *)
    | Thirty_two_no_gc_tag_bit  (* JavaScript mode with full 32-bit integers *)
    | Sixty_four  (* Traditional 64-bit OCaml with GC tag bit *)

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val is_32_bit : t -> bool

  val is_64_bit : t -> bool

  val size_in_bytes : t -> int
end

type windows_system = private
  | Cygwin
  | MinGW
  | Native

(* CR sspies: Remove some of the systems below that are a bit dated. *)
type system = private
  | Linux
  | Windows of windows_system
  | MacOS_like
  | FreeBSD
  | NetBSD
  | OpenBSD
  | Generic_BSD
  | Solaris
  | Dragonfly
  | GNU
  | BeOS
  | Unknown

val system: unit -> system

val windows : unit -> bool

val is_macos : unit -> bool

val is_gas : unit -> bool
