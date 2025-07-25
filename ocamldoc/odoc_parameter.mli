(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Cambium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation and manipulation of method / function / class parameters. *)

(** Types *)

(** Representation of a simple parameter name *)
type simple_name =
  { sn_name : string;
    sn_type : Types.type_expr;
    mutable sn_text : Odoc_types.text option
  }

type param_info =
  | Simple_name of simple_name
  | Tuple of param_info list * Types.type_expr
      (** Representation of parameter names. We need it to represent parameter names in tuples.
   The value [Tuple ([], t)] stands for an anonymous parameter.*)

(** A parameter is just a param_info.*)
type parameter = param_info

(** Functions *)

(** access to the name as a string. For tuples, parentheses and commas are added. *)
val complete_name : parameter -> string

(** access to the complete type *)
val typ : parameter -> Types.type_expr

(** Update the text of a parameter using a function returning
   the optional text associated to a parameter name.*)
val update_parameter_text :
  (string -> Odoc_types.text option) -> parameter -> unit

(** access to the description of a specific name.
   @raise Not_found if no description is associated to the given name. *)
val desc_by_name : parameter -> string -> Odoc_types.text option

(** access to the list of names ; only one for a simple parameter, or
   a list for tuples. *)
val names : parameter -> string list

(** access to the type of a specific name.
   @raise Not_found if no type is associated to the given name. *)
val type_by_name : parameter -> string -> Types.type_expr

(** access to the optional description of a parameter name from an optional info structure.*)
val desc_from_info_opt :
  Odoc_types.info option -> string -> Odoc_types.text option
