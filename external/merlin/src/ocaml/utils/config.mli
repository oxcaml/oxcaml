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

module Magic_numbers : sig
  type t =
    { cmi_magic_number : string;
      ast_intf_magic_number : string;
      ast_impl_magic_number : string;
      cmt_magic_number : string;
      cms_magic_number : string;
      index_magic_number : string
    }

  val current : t

  val to_json : t -> Std.json
end

<<<<<<< Merlin:avx512-bitops
(**/**)
||||||| Compiler:last-imported
val has_bmi : bool
(* Whether the compiler was configured on a machine with BMI *)

val has_bmi2 : bool
(* Whether the compiler was configured on a machine with BMI2 *)

val has_avx : bool
(* Whether the compiler was configured on a machine with AVX *)

val has_avx2 : bool
(* Whether the compiler was configured on a machine with AVX2 *)

val has_f16c : bool
(* Whether the compiler was configured on a machine with F16C *)

val has_fma : bool
(* Whether the compiler was configured on a machine with FMA *)

val has_avx512f : bool
(* Whether the compiler was configured on a machine with AVX512F *)

val has_avx512dq : bool
(* Whether the compiler was configured on a machine with AVX512DQ *)

val has_avx512cd : bool
(* Whether the compiler was configured on a machine with AVX512CD *)

val has_avx512bw : bool
(* Whether the compiler was configured on a machine with AVX512BW *)

val has_avx512vl : bool
(* Whether the compiler was configured on a machine with AVX512VL *)

val has_avx512bf16 : bool
(* Whether the compiler was configured on a machine with AVX512_BF16 *)

val has_vpclmulqdq : bool
(* Whether the compiler was configured on a machine with VPCLMULQDQ *)

val oxcaml_dwarf : bool
(* Whether OxCaml DWARF is used by default *)

val default_keyword_edition : (int * int) option * string list
(** The default keyword edition to assume when -keywords is not specified. *)
=======
val has_bmi : bool
(* Whether the compiler was configured on a machine with BMI *)

val has_bmi2 : bool
(* Whether the compiler was configured on a machine with BMI2 *)

val has_avx : bool
(* Whether the compiler was configured on a machine with AVX *)

val has_avx2 : bool
(* Whether the compiler was configured on a machine with AVX2 *)

val has_f16c : bool
(* Whether the compiler was configured on a machine with F16C *)

val has_fma : bool
(* Whether the compiler was configured on a machine with FMA *)

val has_avx512f : bool
(* Whether the compiler was configured on a machine with AVX512F *)

val has_avx512dq : bool
(* Whether the compiler was configured on a machine with AVX512DQ *)

val has_avx512cd : bool
(* Whether the compiler was configured on a machine with AVX512CD *)

val has_avx512bw : bool
(* Whether the compiler was configured on a machine with AVX512BW *)

val has_avx512vl : bool
(* Whether the compiler was configured on a machine with AVX512VL *)

val has_avx512bf16 : bool
(* Whether the compiler was configured on a machine with AVX512_BF16 *)

val has_vpclmulqdq : bool
(* Whether the compiler was configured on a machine with VPCLMULQDQ *)

val has_avx512vbmi : bool
(* Whether the compiler was configured on a machine with AVX512_VBMI *)

val has_avx512vbmi2 : bool
(* Whether the compiler was configured on a machine with AVX512_VBMI2 *)

val has_vaes : bool
(* Whether the compiler was configured on a machine with VAES *)

val has_avx512bitalg : bool
(* Whether the compiler was configured on a machine with AVX512_BITALG *)

val has_avx512vpopcntdq : bool
(* Whether the compiler was configured on a machine with AVX512_VPOPCNTDQ *)

val has_avx512ifma : bool
(* Whether the compiler was configured on a machine with AVX512_IFMA *)

val oxcaml_dwarf : bool
(* Whether OxCaml DWARF is used by default *)

val default_keyword_edition : (int * int) option * string list
(** The default keyword edition to assume when -keywords is not specified. *)
>>>>>>> Compiler:HEAD
