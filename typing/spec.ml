(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Jane Street Group LLC                              *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes

type constant =
    Const_int of int
  | Const_char of char
  | Const_untagged_char of int
  | Const_string of string * Location.t * string option
  | Const_float of string
  | Const_float32 of string
  | Const_unboxed_float of string
  | Const_unboxed_float32 of string
  | Const_int8 of int
  | Const_int16 of int
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
  | Const_untagged_int of int
  | Const_untagged_int8 of int
  | Const_untagged_int16 of int
  | Const_unboxed_int32 of int32
  | Const_unboxed_int64 of int64
  | Const_unboxed_nativeint of nativeint

type sort = Jkind_types.Sort.Const.t

type locality = Mode.Locality.Const.t

type 'ty expression =
  { sexp_desc : 'ty expression_desc;
    sexp_type : 'ty;
    sexp_loc : Location.t;
  }

and 'ty expression_desc =
  | Sexp_var of Ident.t
  | Sexp_global of Path.t
  | Sexp_constant of constant
  | Sexp_apply of
      { funct : 'ty expression;
        args : (arg_label * 'ty argument) list;
        ret_mode : locality;
      }

and 'ty argument =
  | Arg of 'ty expression * sort
  | Omitted


(* Traversals *)

type ('a, 'b) mapper =
  { ty : 'a -> 'b;
    value_path : Path.t -> Path.t;
  }

let rec map m e =
  { sexp_desc = map_desc m e.sexp_desc;
    sexp_type = m.ty e.sexp_type;
    sexp_loc = e.sexp_loc;
  }

and map_desc m = function
  | Sexp_var id -> Sexp_var id
  | Sexp_global p -> Sexp_global (m.value_path p)
  | Sexp_constant c -> Sexp_constant c
  | Sexp_apply { funct; args; ret_mode } ->
      Sexp_apply
        { funct = map m funct;
          args = List.map (fun (lbl, arg) -> (lbl, map_argument m arg)) args;
          ret_mode }

and map_argument m = function
  | Arg (e, sort) -> Arg (map m e, sort)
  | Omitted -> Omitted

type namespace = Value

let paths e =
  let paths = ref [] in
  let path ns p = paths := (ns, p) :: !paths; p in
  ignore
    (map
       { ty = Fun.id;
         value_path = path Value }
       e
     : _ expression);
  List.rev !paths

let map ~ty ~value_path e = map { ty; value_path } e

let map_types ty e = map ~ty ~value_path:Fun.id e

let map_paths ~value_path e = map ~ty:Fun.id ~value_path e

let iter_types f e =
  ignore (map ~ty:(fun t -> f t) ~value_path:Fun.id e : unit expression)

(* Alpha-equivalence *)

(* [vars] maps the variables bound so far in the first expression to the
   corresponding ones of the second, most recent binding first. *)

let lookup vars id =
  Option.map snd (List.find_opt (fun (id', _) -> Ident.same id id') vars)

let same_var vars id1 id2 =
  match lookup vars id1 with
  | Some id2' -> Ident.same id2 id2'
  | None -> false

let same_constant (c1 : constant) (c2 : constant) =
  (* By representation, which distinguishes [0.] from [-0.]. Literals
     cannot denote NaNs. *)
  let same_float f1 f2 =
    match float_of_string f1, float_of_string f2 with
    | x1, x2 -> Int64.equal (Int64.bits_of_float x1) (Int64.bits_of_float x2)
    | exception Failure _ -> String.equal f1 f2
  in
  match c1, c2 with
  | Const_string (s1, _, _), Const_string (s2, _, _) ->
      (* The quotation delimiters do not matter. *)
      String.equal s1 s2
  | Const_float f1, Const_float f2
  | Const_float32 f1, Const_float32 f2
  | Const_unboxed_float f1, Const_unboxed_float f2
  | Const_unboxed_float32 f1, Const_unboxed_float32 f2 ->
      same_float f1 f2
  | _ -> c1 = c2

let same_arg_label (l1 : arg_label) (l2 : arg_label) =
  match l1, l2 with
  | Nolabel, Nolabel -> true
  | Labelled s1, Labelled s2 | Optional s1, Optional s2 -> String.equal s1 s2
  | (Nolabel | Labelled _ | Optional _), _ -> false

let rec for_all2 f l1 l2 =
  match l1, l2 with
  | [], [] -> true
  | x1 :: l1, x2 :: l2 -> f x1 x2 && for_all2 f l1 l2
  | [], _ :: _ | _ :: _, [] -> false

let rec alpha_equal_exp sp vars e1 e2 =
  match e1.sexp_desc, e2.sexp_desc with
  | Sexp_var id1, Sexp_var id2 -> same_var vars id1 id2
  | Sexp_global p1, Sexp_global p2 -> sp Value p1 p2
  | Sexp_constant c1, Sexp_constant c2 -> same_constant c1 c2
  | Sexp_apply a1, Sexp_apply a2 ->
      alpha_equal_exp sp vars a1.funct a2.funct
      && for_all2
           (fun (l1, arg1) (l2, arg2) ->
              same_arg_label l1 l2
              && (match arg1, arg2 with
                  | Arg (e1, _), Arg (e2, _) -> alpha_equal_exp sp vars e1 e2
                  | Omitted, Omitted -> true
                  | Arg _, Omitted | Omitted, Arg _ -> false))
           a1.args a2.args
  | ( Sexp_var _ | Sexp_global _ | Sexp_constant _ | Sexp_apply _ ), _ ->
      false

let alpha_equal ~same_path ~vars e1 e2 =
  alpha_equal_exp same_path vars e1 e2
