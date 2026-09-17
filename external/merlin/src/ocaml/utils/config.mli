(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* System configuration *)

val version: string
        (* The current version number of the system *)

val as_debug_prefix_map_flag : string
(** The flag to use for assembler debug prefix map ("" if none) *)

val interface_suffix: string ref
        (* Suffix for interface file names *)

val cmi_magic_number: string
        (* Magic number for compiled interface files *)
val ast_intf_magic_number: string
        (* Magic number for file holding an interface syntax tree *)
val ast_impl_magic_number: string
        (* Magic number for file holding an implementation syntax tree *)
val cmt_magic_number: string
        (* Magic number for compiled interface files *)
val cms_magic_number: string
        (* Magic number for compiled shapes files *)
val index_magic_number: string
        (* Magic number for index files *)

val max_tag: int
        (* Biggest tag that can be stored in the header of a regular block. *)

val flat_float_array: bool

val reserved_header_bits : int
val syntax_quotations : bool
(** Whether quote [<[_]>] and splice [$] syntax is enabled. *)

val default_keyword_edition : (int * int) option * string list

type name_mangling_scheme =
  | Flat
  | Structured

exception Invalid_name_mangling_scheme of string

val name_mangling_scheme : name_mangling_scheme

(** Access to configuration values *)
val print_config : out_channel -> unit

val config_var : string -> string option
(** the configuration value of a variable, if it exists *)

(**/**)

val merlin : bool

<<<<<<< Merlin:aes-intrins
module Magic_numbers : sig
  type t =
    { cmi_magic_number : string;
      ast_intf_magic_number : string;
      ast_impl_magic_number : string;
      cmt_magic_number : string;
      cms_magic_number : string;
      index_magic_number : string
    }
||||||| Compiler:last-imported
(**/**)

val has_pclmul : bool
(* Whether the compiler was configured on a machine with PCLMUL *)

val has_popcnt : bool
(* Whether the compiler was configured on a machine with POPCNT *)

val has_lzcnt : bool
(* Whether the compiler was configured on a machine with LZCNT *)

val has_sse3 : bool
(* Whether the compiler was configured on a machine with SSE3 *)

val has_ssse3 : bool
(* Whether the compiler was configured on a machine with SSSE3 *)

val has_sse4_1 : bool
(* Whether the compiler was configured on a machine with SSE4.1 *)

val has_sse4_2 : bool
(* Whether the compiler was configured on a machine with SSE4.2 *)
=======
(**/**)

val has_pclmul : bool
(* Whether the compiler was configured on a machine with PCLMUL *)

val has_aes : bool
(* Whether the compiler was configured on a machine with AES *)

val has_popcnt : bool
(* Whether the compiler was configured on a machine with POPCNT *)

val has_lzcnt : bool
(* Whether the compiler was configured on a machine with LZCNT *)

val has_sse3 : bool
(* Whether the compiler was configured on a machine with SSE3 *)

val has_ssse3 : bool
(* Whether the compiler was configured on a machine with SSSE3 *)

val has_sse4_1 : bool
(* Whether the compiler was configured on a machine with SSE4.1 *)

val has_sse4_2 : bool
(* Whether the compiler was configured on a machine with SSE4.2 *)
>>>>>>> Compiler:HEAD

  val current : t

  val to_json : t -> Std.json
end

(**/**)
