(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Gabriel Scherer, projet Partout, INRIA Paris-Saclay           *)
(*          Thomas Refis, Jane Street Europe                              *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Types
open Data_types
open Typedtree

(* useful pattern auxiliary functions *)

let omega = {
  pat_desc = Tpat_any;
  pat_loc = Location.none;
  pat_extra = [];
  pat_type = Ctype.none;
  pat_env = Env.empty;
  pat_attributes = [];
  pat_unique_barrier = Unique_barrier.not_computed ();
}

let rec omegas i =
  if i <= 0 then [] else omega :: omegas (i-1)

let omega_list l = List.map (fun _ -> omega) l

module Non_empty_row = struct
  type 'a t = 'a * Typedtree.pattern list

  let of_initial = function
    | [] -> assert false
    | pat :: patl -> (pat, patl)

  let map_first f (p, patl) = (f p, patl)
end

(* "views" on patterns are polymorphic variants
   that allow to restrict the set of pattern constructors
   statically allowed at a particular place *)

module Simple = struct
  type view = [
    | `Any
    | `Constant of constant
<<<<<<< oxcaml
    | `Unboxed_unit
    | `Unboxed_bool of bool
    | `Tuple of (string option * pattern) list
    | `Unboxed_tuple of (string option * pattern * Jkind.sort) list
||||||| upstream-base
    | `Tuple of pattern list
=======
    | `Tuple of (string option * pattern) list
>>>>>>> upstream-incoming
    | `Construct of
        Longident.t loc * constructor_description * pattern list
    | `Variant of label * pattern option * row_desc ref
    | `Record of
        (Longident.t loc * label_description * pattern) list * closed_flag
<<<<<<< oxcaml
    | `Record_unboxed_product of
        (Longident.t loc * unboxed_label_description * pattern) list
        * closed_flag
    | `Array of mutability * Jkind.sort * pattern list
||||||| upstream-base
    | `Array of pattern list
=======
    | `Array of mutable_flag * pattern list
>>>>>>> upstream-incoming
    | `Lazy of pattern
  ]

  type pattern = view pattern_data

  let omega = { omega with pat_desc = `Any }
end

module Half_simple = struct
  type view = [
    | Simple.view
    | `Or of pattern * pattern * row_desc option
  ]

  type pattern = view pattern_data
end

module General = struct
  type view = [
    | Half_simple.view
<<<<<<< oxcaml
    | `Var of Ident.t * string loc * Uid.t * Jkind.Sort.t * Mode.Value.l
    | `Alias of pattern * Ident.t * string loc
                * Uid.t * Jkind.Sort.t * Mode.Value.l * Types.type_expr
||||||| upstream-base
    | `Var of Ident.t * string loc
    | `Alias of pattern * Ident.t * string loc
=======
    | `Var of Ident.t * string loc * Uid.t
    | `Alias of pattern * Ident.t * string loc * Uid.t * Types.type_expr
>>>>>>> upstream-incoming
  ]
  type pattern = view pattern_data

  let view_desc = function
    | Tpat_any ->
       `Any
<<<<<<< oxcaml
    | Tpat_var (id, str, uid, sort, mode) ->
       `Var (id, str, uid, sort, mode)
    | Tpat_alias (p, id, str, uid, sort, mode, ty) ->
       `Alias (p, id, str, uid, sort, mode, ty)
||||||| upstream-base
    | Tpat_var (id, str) ->
       `Var (id, str)
    | Tpat_alias (p, id, str) ->
       `Alias (p, id, str)
=======
    | Tpat_var (id, str, uid) ->
       `Var (id, str, uid)
    | Tpat_alias (p, id, str, uid, ty) ->
       `Alias (p, id, str, uid, ty)
>>>>>>> upstream-incoming
    | Tpat_constant cst ->
       `Constant cst
    | Tpat_unboxed_unit ->
       `Unboxed_unit
    | Tpat_unboxed_bool b ->
       `Unboxed_bool b
    | Tpat_tuple ps ->
       `Tuple ps
    | Tpat_unboxed_tuple ps ->
       `Unboxed_tuple ps
    | Tpat_construct (cstr, cstr_descr, args, _) ->
       `Construct (cstr, cstr_descr, args)
    | Tpat_variant (cstr, arg, row_desc) ->
       `Variant (cstr, arg, row_desc)
    | Tpat_record (fields, closed) ->
       `Record (fields, closed)
<<<<<<< oxcaml
    | Tpat_record_unboxed_product (fields, closed) ->
       `Record_unboxed_product (fields, closed)
    | Tpat_array (am, arg_sort, ps) -> `Array (am, arg_sort, ps)
||||||| upstream-base
    | Tpat_array ps -> `Array ps
=======
    | Tpat_array (am,ps) -> `Array (am, ps)
>>>>>>> upstream-incoming
    | Tpat_or (p, q, row_desc) -> `Or (p, q, row_desc)
    | Tpat_lazy p -> `Lazy p

  let view p : pattern =
    { p with pat_desc = view_desc p.pat_desc }

  let erase_desc = function
    | `Any -> Tpat_any
<<<<<<< oxcaml
    | `Var (id, str, uid, sort, mode) -> Tpat_var (id, str, uid, sort, mode)
    | `Alias (p, id, str, uid, sort, mode, ty) ->
       Tpat_alias (p, id, str, uid, sort, mode, ty)
||||||| upstream-base
    | `Var (id, str) -> Tpat_var (id, str)
    | `Alias (p, id, str) -> Tpat_alias (p, id, str)
=======
    | `Var (id, str, uid) -> Tpat_var (id, str, uid)
    | `Alias (p, id, str, uid, ty) -> Tpat_alias (p, id, str, uid, ty)
>>>>>>> upstream-incoming
    | `Constant cst -> Tpat_constant cst
    | `Unboxed_unit -> Tpat_unboxed_unit
    | `Unboxed_bool b -> Tpat_unboxed_bool b
    | `Tuple ps -> Tpat_tuple ps
    | `Unboxed_tuple ps -> Tpat_unboxed_tuple ps
    | `Construct (cstr, cst_descr, args) ->
       Tpat_construct (cstr, cst_descr, args, None)
    | `Variant (cstr, arg, row_desc) ->
       Tpat_variant (cstr, arg, row_desc)
    | `Record (fields, closed) ->
       Tpat_record (fields, closed)
<<<<<<< oxcaml
    | `Record_unboxed_product (fields, closed) ->
       Tpat_record_unboxed_product (fields, closed)
    | `Array (am, arg_sort, ps) -> Tpat_array (am, arg_sort, ps)
||||||| upstream-base
    | `Array ps -> Tpat_array ps
=======
    | `Array (am, ps) -> Tpat_array (am, ps)
>>>>>>> upstream-incoming
    | `Or (p, q, row_desc) -> Tpat_or (p, q, row_desc)
    | `Lazy p -> Tpat_lazy p

  let erase p : Typedtree.pattern =
    { p with pat_desc = erase_desc p.pat_desc }

  let rec strip_vars (p : pattern) : Half_simple.pattern =
    match p.pat_desc with
<<<<<<< oxcaml
    | `Alias (p, _, _, _, _, _, _) -> strip_vars (view p)
||||||| upstream-base
    | `Alias (p, _, _) -> strip_vars (view p)
=======
    | `Alias (p, _, _, _, _) -> strip_vars (view p)
>>>>>>> upstream-incoming
    | `Var _ -> { p with pat_desc = `Any }
    | #Half_simple.view as view -> { p with pat_desc = view }
end

(* the head constructor of a simple pattern *)

module Head : sig
  type desc =
    | Any
    | Construct of constructor_description
    | Constant of constant
<<<<<<< oxcaml
    | Unboxed_unit
    | Unboxed_bool of bool
    | Tuple of string option list
    | Unboxed_tuple of (string option * Jkind.sort) list
||||||| upstream-base
    | Tuple of int
=======
    | Tuple of string option list
>>>>>>> upstream-incoming
    | Record of label_description list
    | Record_unboxed_product of unboxed_label_description list
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
<<<<<<< oxcaml
    | Array of mutability * Jkind.sort * int
||||||| upstream-base
    | Array of int
=======
    | Array of mutable_flag * int
>>>>>>> upstream-incoming
    | Lazy

  type t = desc pattern_data

  val arity : t -> int

  (** [deconstruct p] returns the head of [p] and the list of sub patterns. *)
  val deconstruct : Simple.pattern -> t * pattern list

  (** reconstructs a pattern, putting wildcards as sub-patterns. *)
  val to_omega_pattern : t -> pattern

  val omega : t
end = struct
  type desc =
    | Any
    | Construct of constructor_description
    | Constant of constant
<<<<<<< oxcaml
    | Unboxed_unit
    | Unboxed_bool of bool
    | Tuple of string option list
    | Unboxed_tuple of (string option * Jkind.sort) list
||||||| upstream-base
    | Tuple of int
=======
    | Tuple of string option list
>>>>>>> upstream-incoming
    | Record of label_description list
    | Record_unboxed_product of unboxed_label_description list
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
          (* the row of the type may evolve if [close_variant] is called,
             hence the (unit -> ...) delay *)
<<<<<<< oxcaml
    | Array of mutability * Jkind.sort * int
||||||| upstream-base
    | Array of int
=======
    | Array of mutable_flag * int
>>>>>>> upstream-incoming
    | Lazy

  type t = desc pattern_data

  let deconstruct (q : Simple.pattern) =
    let deconstruct_desc = function
      | `Any -> Any, []
      | `Constant c -> Constant c, []
      | `Unboxed_unit -> Unboxed_unit, []
      | `Unboxed_bool b -> Unboxed_bool b, []
      | `Tuple args ->
          Tuple (List.map fst args), (List.map snd args)
<<<<<<< oxcaml
      | `Unboxed_tuple args ->
          let labels_and_sorts = List.map (fun (l, _, s) -> l, s) args in
          let pats = List.map (fun (_, p, _) -> p) args in
          Unboxed_tuple labels_and_sorts, pats
||||||| upstream-base
          Tuple (List.length args), args
=======
>>>>>>> upstream-incoming
      | `Construct (_, c, args) ->
          Construct c, args
      | `Variant (tag, arg, cstr_row) ->
          let has_arg, pats =
            match arg with
            | None -> false, []
            | Some a -> true, [a]
          in
          let type_row () =
            match get_desc (Ctype.expand_head q.pat_env q.pat_type) with
            | Tvariant type_row -> type_row
            | _ -> assert false
          in
          Variant {tag; has_arg; cstr_row; type_row}, pats
<<<<<<< oxcaml
      | `Array (am, arg_sort, args) ->
          Array (am, arg_sort, List.length args), args
||||||| upstream-base
      | `Array args ->
          Array (List.length args), args
=======
      | `Array (am, args) ->
          Array (am, List.length args), args
>>>>>>> upstream-incoming
      | `Record (largs, _) ->
          let lbls = List.map (fun (_,lbl,_) -> lbl) largs in
          let pats = List.map (fun (_,_,pat) -> pat) largs in
          Record lbls, pats
      | `Record_unboxed_product (largs, _) ->
          let lbls = List.map (fun (_,lbl,_) -> lbl) largs in
          let pats = List.map (fun (_,_,pat) -> pat) largs in
          Record_unboxed_product lbls, pats
      | `Lazy p ->
          Lazy, [p]
    in
    let desc, pats = deconstruct_desc q.pat_desc in
    { q with pat_desc = desc }, pats

  let arity t =
    match t.pat_desc with
      | Any -> 0
      | Constant _ -> 0
      | Construct c -> c.cstr_arity
<<<<<<< oxcaml
      | Unboxed_unit -> 0
      | Unboxed_bool _ -> 0
      | Tuple l -> List.length l
      | Unboxed_tuple l -> List.length l
      | Array (_, _, n) -> n
||||||| upstream-base
      | Tuple n | Array n -> n
=======
      | Tuple l -> List.length l
      | Array (_, n) -> n
>>>>>>> upstream-incoming
      | Record l -> List.length l
      | Record_unboxed_product l -> List.length l
      | Variant { has_arg; _ } -> if has_arg then 1 else 0
      | Lazy -> 1

  let to_omega_pattern t =
    let pat_desc =
      let mkloc x = Location.mkloc x t.pat_loc in
      match t.pat_desc with
      | Any -> Tpat_any
      | Lazy -> Tpat_lazy omega
      | Constant c -> Tpat_constant c
<<<<<<< oxcaml
      | Unboxed_unit -> Tpat_unboxed_unit
      | Unboxed_bool b -> Tpat_unboxed_bool b
      | Tuple lbls ->
          Tpat_tuple (List.map (fun lbl -> lbl, omega) lbls)
      | Unboxed_tuple lbls_and_sorts ->
          Tpat_unboxed_tuple
            (List.map (fun (lbl, sort) -> lbl, omega, sort) lbls_and_sorts)
      | Array (am, arg_sort, n) -> Tpat_array (am, arg_sort, omegas n)
||||||| upstream-base
      | Tuple n -> Tpat_tuple (omegas n)
      | Array n -> Tpat_array (omegas n)
=======
      | Tuple lbls ->
          Tpat_tuple (List.map (fun lbl -> lbl, omega) lbls)
      | Array (am, n) -> Tpat_array (am, omegas n)
>>>>>>> upstream-incoming
      | Construct c ->
          let lid_loc = mkloc (Longident.Lident c.cstr_name) in
          Tpat_construct (lid_loc, c, omegas c.cstr_arity, None)
      | Variant { tag; has_arg; cstr_row } ->
          let arg_opt = if has_arg then Some omega else None in
          Tpat_variant (tag, arg_opt, cstr_row)
      | Record lbls ->
          let lst =
            List.map (fun lbl ->
              let lid_loc = mkloc (Longident.Lident lbl.lbl_name) in
              (lid_loc, lbl, omega)
            ) lbls
          in
          Tpat_record (lst, Closed)
      | Record_unboxed_product lbls ->
          let lst =
            List.map (fun lbl ->
              let lid_loc = mkloc (Longident.Lident lbl.lbl_name) in
              (lid_loc, lbl, omega)
            ) lbls
          in
          Tpat_record_unboxed_product (lst, Closed)
    in
    { t with
      pat_desc;
      pat_extra = [];
    }

  let omega = { omega with pat_desc = Any }
end
