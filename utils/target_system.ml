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
end

module System = struct
  (* CR shym Maybe get rid of derived systems and return system directly? *)
  (* CR shym Why [MacOS_like] instead of simply [MacOS]? *)
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
    | Solaris
    | Dragonfly
    | GNU
    | BeOS
    | Unknown

  let derived_system () : derived_system =
    match Config.system with
    | "beos" -> BeOS
    | "cygwin" -> Cygwin
    | "dragonfly" -> Dragonfly
    | "freebsd" -> FreeBSD
    | "gnu" -> GNU
    | "linux" -> Linux
    | "macosx" -> MacOS_like
    | "mingw64" -> MinGW_64
    | "mingw" -> MinGW_32
    | "netbsd" -> NetBSD
    | "openbsd" -> OpenBSD
    | "solaris" -> Solaris
    | "win32" -> Win32
    | "win64" -> Win64
    | "unknown" -> Unknown
    | _ ->
      Misc.fatal_errorf
        "Cannot determine system type (%s): ensure `target_system.ml' matches \
         `configure'"
        Config.system

  let is_windows () =
    match derived_system () with
    | Linux | MacOS_like | FreeBSD | NetBSD | OpenBSD | Solaris | Dragonfly
    | GNU | BeOS | Unknown ->
      false
    | MinGW_32 | MinGW_64 | Win32 | Win64 | Cygwin -> true

  let is_macos () =
    match derived_system () with
    | Linux | FreeBSD | NetBSD | OpenBSD | Solaris | Dragonfly | GNU | BeOS
    | Unknown | MinGW_32 | MinGW_64 | Win32 | Win64 | Cygwin ->
      false
    | MacOS_like -> true

  type windows_system =
    | Cygwin
    | MinGW
    | Native

  type t =
    | Linux
    | Windows of windows_system
    | MacOS_like
    | FreeBSD
    | NetBSD
    | OpenBSD
    | Solaris
    | Dragonfly
    | GNU
    | BeOS
    | Unknown

  let get () : t =
    match derived_system () with
    | Linux -> Linux
    | MinGW_32 | MinGW_64 -> Windows MinGW
    | Win32 | Win64 -> Windows Native
    | Cygwin -> Windows Cygwin
    | MacOS_like -> MacOS_like
    | FreeBSD -> FreeBSD
    | NetBSD -> NetBSD
    | OpenBSD -> OpenBSD
    | Solaris -> Solaris
    | Dragonfly -> Dragonfly
    | GNU -> GNU
    | BeOS -> BeOS
    | Unknown -> Unknown
end

module Assembler = struct
  type t =
    | GAS_like
    | MacOS
    | MASM

  (* CR shym Note that [Asm_label.label_prefix] will call this function at
     initialisation *)
  let get () =
    match System.derived_system () with
    | Win32 | Win64 -> MASM
    | MacOS_like -> MacOS
    | MinGW_32 | MinGW_64 | Cygwin | Linux | FreeBSD | NetBSD | OpenBSD
    | Solaris | GNU | Dragonfly | BeOS | Unknown ->
      GAS_like

  let is_macos () = match get () with MASM | GAS_like -> false | MacOS -> true

  let is_gas () = match get () with MASM | MacOS -> false | GAS_like -> true
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
