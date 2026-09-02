[@@@ocaml.warning "+a-4-30-40-41-42"]

module Architecture = struct
  type t =
    | IA32
    | X86_64
    | ARM
    | AArch64
    | POWER
    | Z
    | Riscv

  let get () : t =
    match Config.architecture with
    | "i386" -> IA32
    | "amd64" -> X86_64
    | "arm" -> ARM
    | "arm64" -> AArch64
    | "power" -> POWER
    | "s390x" -> Z
    | "riscv" -> Riscv
    | arch -> Misc.fatal_errorf "Unknown architecture `%s'" arch

  let is_arm () = match get () with ARM | AArch64 -> true | _ -> false

  let is_64_bit () =
    match get () with
    | X86_64 | AArch64 | POWER | Z | Riscv -> true
    | IA32 | ARM -> false

  let is_32_bit () = not (is_64_bit ())

  let to_string = function
    | IA32 -> "IA32"
    | X86_64 -> "X86_64"
    | ARM -> "ARM"
    | AArch64 -> "AArch64"
    | POWER -> "POWER"
    | Z -> "Z"
    | Riscv -> "Riscv"
end

module System = struct
  type windows_system =
    | MinGW
    | MSVC

  type t =
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

  let get () : t =
    match Config.system with
    | "linux" -> Linux
    | "mingw" | "mingw64" -> Windows MinGW
    | "win32" | "win64" -> Windows MSVC
    | "cygwin" -> Cygwin
    | "macosx" -> MacOS
    | "freebsd" -> FreeBSD
    | "netbsd" -> NetBSD
    | "openbsd" -> OpenBSD
    | "solaris" -> Solaris
    | "dragonfly" -> Dragonfly
    | "gnu" -> GNU
    | "beos" -> BeOS
    | "unknown" -> Misc.fatal_error "Unknown system type"
    | _ ->
      Misc.fatal_errorf
        "Cannot determine system type (%s): ensure `target_system.ml' matches \
         `configure'"
        Config.system

  let is_macos () =
    match get () with
    | Linux | Windows _ | Cygwin | FreeBSD | NetBSD | OpenBSD | Solaris
    | Dragonfly | GNU | BeOS ->
      false
    | MacOS -> true
end

module Assembler = struct
  type t =
    | GAS_like
    | MacOS
    | MASM

  let get () =
    match System.get () with
    | Windows MSVC -> MASM
    | MacOS -> MacOS
    | Linux
    | Windows MinGW
    | Cygwin | FreeBSD | NetBSD | OpenBSD | Solaris | Dragonfly | GNU | BeOS ->
      GAS_like

  let is_macos () = match get () with MASM | GAS_like -> false | MacOS -> true

  let is_gas () = match get () with MASM | MacOS -> false | GAS_like -> true

  let is_masm () = match get () with MacOS | GAS_like -> false | MASM -> true

  let is_windows_or_cygwin () =
    match System.get () with
    | Windows (MinGW | MSVC) | Cygwin -> true
    | Linux | MacOS | FreeBSD | NetBSD | OpenBSD | Solaris | Dragonfly | GNU
    | BeOS ->
      false

  let label_prefix () =
    match get () with MacOS -> "L" | MASM | GAS_like -> ".L"
end

module Machine_width = struct
  type t =
    | Thirty_two (* Traditional 32-bit OCaml with GC tag bit *)
    | Thirty_two_no_gc_tag_bit (* JavaScript mode with full 32-bit integers *)
    | Sixty_four (* Traditional 64-bit OCaml with GC tag bit *)

  let print ppf = function
    | Thirty_two -> Format.fprintf ppf "Thirty_two"
    | Thirty_two_no_gc_tag_bit -> Format.fprintf ppf "Thirty_two_no_gc_tag_bit"
    | Sixty_four -> Format.fprintf ppf "Sixty_four"

  let equal t1 t2 =
    match t1, t2 with
    | Thirty_two, Thirty_two
    | Thirty_two_no_gc_tag_bit, Thirty_two_no_gc_tag_bit
    | Sixty_four, Sixty_four ->
      true
    | _ -> false

  let is_32_bit = function
    | Thirty_two | Thirty_two_no_gc_tag_bit -> true
    | Sixty_four -> false

  let is_64_bit = function
    | Thirty_two | Thirty_two_no_gc_tag_bit -> false
    | Sixty_four -> true

  let size_in_bytes = function
    | Thirty_two | Thirty_two_no_gc_tag_bit -> 4
    | Sixty_four -> 8
end
