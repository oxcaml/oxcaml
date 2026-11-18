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

module Slice = struct
  type kind =
    | Closed of int
    | Open of { prefix : int; suffix : int }

  type elements =
    | Elements_closed of pattern list
    | Elements_open of { prefix : pattern list; suffix : pattern list }

  let of_typed = function
    | Tslice_closed ps -> Elements_closed ps
    | Tslice_open { prefix; rest = _; suffix } ->
        Elements_open { prefix; suffix }

  let to_typed = function
    | Elements_closed ps -> Tslice_closed ps
    | Elements_open { prefix; suffix } ->
        Tslice_open { prefix; rest = None; suffix }

  let kind = function
    | Elements_closed ps -> Closed (List.length ps)
    | Elements_open { prefix; suffix } ->
        Open { prefix = List.length prefix; suffix = List.length suffix }

  let patterns = function
    | Elements_closed ps -> ps
    | Elements_open { prefix; suffix } -> prefix @ suffix

  let arity = function
    | Closed n -> n
    | Open { prefix; suffix } -> prefix + suffix

  let of_patterns kind ps =
    match kind with
    | Closed _ -> Elements_closed ps
    | Open { prefix; _ } ->
        Elements_open
          { prefix = List.filteri (fun i _ -> i < prefix) ps;
            suffix = List.filteri (fun i _ -> i >= prefix) ps }

  let split kinds =
    let max_closed, max_prefix, max_suffix =
      List.fold_left
        (fun (c, p, s) kind ->
           match kind with
           | Closed n -> max c n, p, s
           | Open { prefix; suffix } -> c, max p prefix, max s suffix)
        (0, 0, 0) kinds
    in
    let max_prefix =
      if max_closed + 1 >= max_prefix + max_suffix
      then max_closed + 1 - max_suffix
      else max_prefix
    in
    List.init (max_prefix + max_suffix) (fun n -> Closed n)
    @ [Open { prefix = max_prefix; suffix = max_suffix }]

  let covers d h =
    match d, h with
    | Closed n, Closed m -> n = m
    | Open _, Closed _ -> false
    | Closed n, Open { prefix; suffix } -> n >= prefix + suffix
    | Open { prefix = p; suffix = s }, Open { prefix; suffix } ->
        prefix <= p && suffix <= s

  let args d h ps =
    match h with
    | Closed _ -> ps
    | Open { prefix; suffix } ->
        let missing = arity d - prefix - suffix in
        let prefix_ps = List.filteri (fun i _ -> i < prefix) ps in
        let suffix_ps = List.filteri (fun i _ -> i >= prefix) ps in
        prefix_ps @ omegas missing @ suffix_ps

  let common_args e1 e2 =
    let k1 = kind e1 and k2 = kind e2 in
    List.filter_map
      (fun d ->
         if covers d k1 && covers d k2
         then Some (d, args d k1 (patterns e1), args d k2 (patterns e2))
         else None)
      (split [k1; k2])

  let missing kinds =
    let first_missing_below l =
      List.init l Fun.id
      |> List.find_opt (fun n -> not (List.mem (Closed n) kinds))
      |> Option.map (fun n -> Closed n)
    in
    match List.find_opt (function Open _ -> true | Closed _ -> false) kinds with
    | Some open_kind -> first_missing_below (arity open_kind)
    | None ->
        let l = 1 + List.fold_left (fun m kind -> max m (arity kind)) 0 kinds in
        match first_missing_below l with
        | Some _ as missing -> missing
        | None -> Some (Open { prefix = l; suffix = 0 })
end

(* "views" on patterns are polymorphic variants
   that allow to restrict the set of pattern constructors
   statically allowed at a particular place *)

module Simple = struct
  type view = [
    | `Any
    | `Constant of constant
    | `Unboxed_unit
    | `Unboxed_bool of bool
    | `Tuple of (string option * pattern * Jkind.sort) list
    | `Unboxed_tuple of (string option * pattern * Jkind.sort) list
    | `Construct of
        Longident.t loc * constructor_description * constructor_representation
        * (Jkind.sort * pattern) list
    | `Variant of label * pattern option * row_desc ref
    | `Record of
        (Longident.t loc * label_description * pattern) list
        * record_representation * closed_flag
    | `Record_unboxed_product of
        (Longident.t loc * unboxed_label_description * pattern) list
        * record_unboxed_product_representation * closed_flag
    | `Array of mutability * Jkind.sort * pattern list
    | `Addr of mutability * Jkind.sort * pattern
    | `Slice of mutability * Jkind.sort * Slice.elements
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
    | `Var of
        Ident.t * string loc * Uid.t * Jkind.Sort.t * Mode.With_regionality.l
    | `Fun_layout of Ident.t * string loc * Uid.t
                   * Jkind.Sort.t * Mode.With_regionality.l * Types.Lpoly.t
                   * locality_mode_r
    | `Alias of
        pattern
        * Ident.t
        * string loc
        * Uid.t
        * Jkind.Sort.t
        * Mode.With_regionality.l
        * Types.type_expr
    | `Deref of
        Ident.t
        * string loc
        * Uid.t
        * Jkind.Sort.t
        * Mode.With_regionality.l
        * Types.type_expr
    | `Alias_deref of
        pattern
        * Ident.t
        * string loc
        * Uid.t
        * Jkind.Sort.t
        * Mode.With_regionality.l
        * Types.type_expr
    | `Slice_rest of
        mutability * Jkind.sort * pattern list * slice_rest_var * pattern list
  ]
  type pattern = view pattern_data

  let view_desc = function
    | Tpat_any ->
       `Any
    | Tpat_var { id; name = str; uid; sort; mode } ->
       `Var (id, str, uid, sort, mode)
    | Tpat_fun_layout { id; name = str; uid; sort; mode; lpoly;
                        env_locality_mode } ->
       `Fun_layout (id, str, uid, sort, mode, lpoly, env_locality_mode)
    | Tpat_alias { pattern = p; id; name = str; uid; sort; mode;
                   type_expr = ty } ->
       `Alias (p, id, str, uid, sort, mode, ty)
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
    | Tpat_construct (cstr, cstr_descr, cstr_repr, args, _) ->
       `Construct (cstr, cstr_descr, cstr_repr, args)
    | Tpat_variant (cstr, arg, row_desc) ->
       `Variant (cstr, arg, row_desc)
    | Tpat_record (fields, repr, closed) ->
       `Record (fields, repr, closed)
    | Tpat_record_unboxed_product (fields, repr, closed) ->
       `Record_unboxed_product (fields, repr, closed)
    | Tpat_array (am, arg_sort, ps) -> `Array (am, arg_sort, ps)
    | Tpat_or (p, q, row_desc) -> `Or (p, q, row_desc)
    | Tpat_addr (mut, arg_sort, p) -> `Addr (mut, arg_sort, p)
    | Tpat_slice
        { mutability; sort;
          elements = Tslice_open { prefix; rest = Some rest; suffix } } ->
       `Slice_rest (mutability, sort, prefix, rest, suffix)
    | Tpat_slice { mutability; sort; elements } ->
       `Slice (mutability, sort, Slice.of_typed elements)
    | Tpat_deref { id; name = str; uid; sort; mode; type_expr = ty } ->
       `Deref (id, str, uid, sort, mode, ty)
    | Tpat_alias_deref
        { pattern = p; id; name = str; uid; sort; mode; type_expr = ty } ->
       `Alias_deref (p, id, str, uid, sort, mode, ty)
    | Tpat_lazy p -> `Lazy p

  let view p : pattern =
    { p with pat_desc = view_desc p.pat_desc }

  let erase_desc = function
    | `Any -> Tpat_any
    | `Var (id, str, uid, sort, mode) ->
       Tpat_var { id; name = str; uid; sort; mode }
    | `Fun_layout (id, str, uid, sort, mode, lpoly, env_locality_mode) ->
       Tpat_fun_layout { id; name = str; uid; sort; mode; lpoly;
                         env_locality_mode }
    | `Alias (p, id, str, uid, sort, mode, ty) ->
       Tpat_alias { pattern = p; id; name = str; uid; sort; mode;
                    type_expr = ty }
    | `Constant cst -> Tpat_constant cst
    | `Unboxed_unit -> Tpat_unboxed_unit
    | `Unboxed_bool b -> Tpat_unboxed_bool b
    | `Tuple ps -> Tpat_tuple ps
    | `Unboxed_tuple ps -> Tpat_unboxed_tuple ps
    | `Construct (cstr, cst_descr, cst_repr, args) ->
       Tpat_construct (cstr, cst_descr, cst_repr, args, None)
    | `Variant (cstr, arg, row_desc) ->
       Tpat_variant (cstr, arg, row_desc)
    | `Record (fields, repr, closed) ->
       Tpat_record (fields, repr, closed)
    | `Record_unboxed_product (fields, repr, closed) ->
       Tpat_record_unboxed_product (fields, repr, closed)
    | `Array (am, arg_sort, ps) -> Tpat_array (am, arg_sort, ps)
    | `Or (p, q, row_desc) -> Tpat_or (p, q, row_desc)
    | `Addr (mut, arg_sort, p) -> Tpat_addr (mut, arg_sort, p)
    | `Slice (mutability, sort, elements) ->
       Tpat_slice { mutability; sort; elements = Slice.to_typed elements }
    | `Slice_rest (mutability, sort, prefix, rest, suffix) ->
       Tpat_slice
         { mutability; sort;
           elements = Tslice_open { prefix; rest = Some rest; suffix } }
    | `Deref (id, str, uid, sort, mode, ty) ->
       Tpat_deref { id; name = str; uid; sort; mode; type_expr = ty }
    | `Alias_deref (p, id, str, uid, sort, mode, ty) ->
       Tpat_alias_deref
         { pattern = p; id; name = str; uid; sort; mode; type_expr = ty }
    | `Lazy p -> Tpat_lazy p

  let erase p : Typedtree.pattern =
    { p with pat_desc = erase_desc p.pat_desc }

  let rec strip_vars (p : pattern) : Half_simple.pattern =
    match p.pat_desc with
    | `Alias (p, _, _, _, _, _, _) | `Alias_deref (p, _, _, _, _, _, _) ->
        strip_vars (view p)
    | `Var _ | `Fun_layout _ | `Deref _ ->
        { p with pat_desc = `Any }
    | `Slice_rest (mutability, sort, prefix, _, suffix) ->
        let elements = Slice.Elements_open { prefix; suffix } in
        { p with pat_desc = `Slice (mutability, sort, elements) }
    | #Half_simple.view as view -> { p with pat_desc = view }
end

(* the head constructor of a simple pattern *)

module Head : sig
  type desc =
    | Any
    | Construct of
        constructor_description * constructor_representation * Jkind.sort list
    | Constant of constant
    | Unboxed_unit
    | Unboxed_bool of bool
    | Tuple of (string option * Jkind.sort) list
    | Unboxed_tuple of (string option * Jkind.sort) list
    | Record of
        label_description list * record_representation
    | Record_unboxed_product of
        unboxed_label_description list
        * record_unboxed_product_representation
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
    | Array of mutability * Jkind.sort * int
    | Addr of mutability * Jkind.sort
    | Slice of { mutability : mutability; sort : Jkind.sort;
                 kind : Slice.kind }
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
    | Construct of
        constructor_description * constructor_representation * Jkind.sort list
    | Constant of constant
    | Unboxed_unit
    | Unboxed_bool of bool
    | Tuple of (string option * Jkind.sort) list
    | Unboxed_tuple of (string option * Jkind.sort) list
    | Record of
        label_description list * record_representation
    | Record_unboxed_product of
        unboxed_label_description list
        * record_unboxed_product_representation
    | Variant of
        { tag: label; has_arg: bool;
          cstr_row: row_desc ref;
          type_row : unit -> row_desc; }
          (* the row of the type may evolve if [close_variant] is called,
             hence the (unit -> ...) delay *)
    | Array of mutability * Jkind.sort * int
    | Addr of mutability * Jkind.sort
    | Slice of { mutability : mutability; sort : Jkind.sort;
                 kind : Slice.kind }
    | Lazy

  type t = desc pattern_data

  let deconstruct (q : Simple.pattern) =
    let deconstruct_desc = function
      | `Any -> Any, []
      | `Constant c -> Constant c, []
      | `Unboxed_unit -> Unboxed_unit, []
      | `Unboxed_bool b -> Unboxed_bool b, []
      | `Tuple args ->
          let labels_and_sorts = List.map (fun (l, _, s) -> l, s) args in
          let pats = List.map (fun (_, p, _) -> p) args in
          Tuple labels_and_sorts, pats
      | `Unboxed_tuple args ->
          let labels_and_sorts = List.map (fun (l, _, s) -> l, s) args in
          let pats = List.map (fun (_, p, _) -> p) args in
          Unboxed_tuple labels_and_sorts, pats
      | `Construct (_, c, r, args) ->
          Construct (c, r, List.map fst args), List.map snd args
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
      | `Array (am, arg_sort, args) ->
          Array (am, arg_sort, List.length args), args
      | `Record (largs, repr, _) ->
          let lbls = List.map (fun (_,lbl,_) -> lbl) largs in
          let pats = List.map (fun (_,_,pat) -> pat) largs in
          Record (lbls, repr), pats
      | `Record_unboxed_product (largs, repr, _) ->
          let lbls = List.map (fun (_,lbl,_) -> lbl) largs in
          let pats = List.map (fun (_,_,pat) -> pat) largs in
          Record_unboxed_product (lbls, repr), pats
      | `Addr (mut, arg_sort, p) ->
          Addr (mut, arg_sort), [p]
      | `Slice (mutability, sort, elements) ->
          Slice { mutability; sort; kind = Slice.kind elements },
          Slice.patterns elements
      | `Lazy p ->
          Lazy, [p]
    in
    let desc, pats = deconstruct_desc q.pat_desc in
    { q with pat_desc = desc }, pats

  let arity t =
    match t.pat_desc with
      | Any -> 0
      | Constant _ -> 0
      | Construct (c, _, _) -> c.cstr_arity
      | Unboxed_unit -> 0
      | Unboxed_bool _ -> 0
      | Tuple l -> List.length l
      | Unboxed_tuple l -> List.length l
      | Array (_, _, n) -> n
      | Record (l, _) -> List.length l
      | Record_unboxed_product (l, _) -> List.length l
      | Variant { has_arg; _ } -> if has_arg then 1 else 0
      | Addr _ -> 1
      | Slice { kind; _ } -> Slice.arity kind
      | Lazy -> 1

  let to_omega_pattern t =
    let pat_desc =
      let mkloc x = Location.mkloc x t.pat_loc in
      match t.pat_desc with
      | Any -> Tpat_any
      | Addr (mut, arg_sort) -> Tpat_addr (mut, arg_sort, omega)
      | Slice { mutability; sort; kind } ->
          let elements = Slice.of_patterns kind (omegas (Slice.arity kind)) in
          Tpat_slice { mutability; sort; elements = Slice.to_typed elements }
      | Lazy -> Tpat_lazy omega
      | Constant c -> Tpat_constant c
      | Unboxed_unit -> Tpat_unboxed_unit
      | Unboxed_bool b -> Tpat_unboxed_bool b
      | Tuple lbls ->
          Tpat_tuple (List.map (fun (lbl, sort) -> lbl, omega, sort) lbls)
      | Unboxed_tuple lbls_and_sorts ->
          Tpat_unboxed_tuple
            (List.map (fun (lbl, sort) -> lbl, omega, sort) lbls_and_sorts)
      | Array (am, arg_sort, n) -> Tpat_array (am, arg_sort, omegas n)
      | Construct (c, r, sorts) ->
          let lid_loc = mkloc (Longident.Lident c.cstr_name) in
          let pats = omegas c.cstr_arity in
          Tpat_construct (lid_loc, c, r, List.combine sorts pats, None)
      | Variant { tag; has_arg; cstr_row } ->
          let arg_opt = if has_arg then Some omega else None in
          Tpat_variant (tag, arg_opt, cstr_row)
      | Record (lbls, repr) ->
          let lst =
            List.map (fun lbl ->
              let lid_loc = mkloc (Longident.Lident lbl.lbl_name) in
              (lid_loc, lbl, omega)
            ) lbls
          in
          Tpat_record (lst, repr, Closed)
      | Record_unboxed_product (lbls, repr) ->
          let lst =
            List.map (fun lbl ->
              let lid_loc = mkloc (Longident.Lident lbl.lbl_name) in
              (lid_loc, lbl, omega)
            ) lbls
          in
          Tpat_record_unboxed_product (lst, repr, Closed)
    in
    { t with
      pat_desc;
      pat_extra = [];
    }

  let omega = { omega with pat_desc = Any }
end
