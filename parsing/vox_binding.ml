(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Copyright 2026 Jane Street Group LLC                                  *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Parsetree

(* The whole module is a free-occurrence check for one name at a time.  The
   walk raises [Found] at the first free occurrence of the name inside a
   refinement predicate; shadowing is implemented by not descending into
   the scope of a binder for the same name. *)

exception Found

(* Does [pat] bind [name]?  (Or-patterns bind the same set on both sides,
   so scanning every variable is correct.) *)
let pattern_binds name pat =
  let exception Binds in
  let it =
    { Ast_iterator.default_iterator with
      pat =
        (fun sub p ->
          (match p.ppat_desc with
           | Ppat_var s | Ppat_alias (_, s) when s.txt = name -> raise Binds
           | _ -> ());
          Ast_iterator.default_iterator.pat sub p);
      (* Binding is a matter of the pattern itself; do not look at
         attribute payloads or interior expressions/types. *)
      attribute = (fun _ _ -> ());
      expr = (fun _ _ -> ());
      typ = (fun _ _ -> ())
    }
  in
  match it.pat it pat with () -> false | exception Binds -> true

(* [walk_type] looks for refinements and checks their predicates;
   occurrences of the name outside a refinement predicate do not count.
   [walk_pred] looks for free occurrences of the name, we are already
   inside a predicate. *)

let rec walk_type name ty =
  match ty.ptyp_desc with
  | Ptyp_refine (payload, predicate) ->
      walk_type name payload;
      walk_pred name predicate
  | Ptyp_arrow (arg, domain, codomain, _, _) -> (
      match arg with
      | Pan_name s when s.txt = name ->
          (* If the name occurs free in a refinement in [domain] or
             [codomain], this nested arrow binds it there; either way no
             occurrence escapes to the enclosing scope. *)
          ()
      | Pan_tilde s when s.txt = name ->
          (* [~x:] names the value of its own argument inside the
             argument's refinements, but does not scope over the
             codomain. *)
          walk_type name codomain
      | Pan_nolabel | Pan_name _ | Pan_tilde _ | Pan_optional _ ->
          walk_type name domain;
          walk_type name codomain)
  | _ -> default_walk_type name ty

and default_walk_type name ty =
  let it = child_walker name in
  Ast_iterator.default_iterator.typ it ty

and walk_pred name e =
  match e.pexp_desc with
  | Pexp_ident { txt = Longident.Lident s; _ } -> if s = name then raise Found
  | Pexp_let (_, rec_flag, vbs, body) ->
      let bound = List.exists (fun vb -> pattern_binds name vb.pvb_pat) vbs in
      let walk_bound_expr vb =
        (* Constraint types inside the pattern are walked in either case:
           they are types, and refinements within them can mention the
           name. *)
        default_walk_pat name vb.pvb_pat;
        match rec_flag with
        | Nonrecursive -> walk_pred name vb.pvb_expr
        | Recursive -> if not bound then walk_pred name vb.pvb_expr
      in
      List.iter walk_bound_expr vbs;
      if not bound then walk_pred name body
  | Pexp_function (params, constraint_, body) ->
      let bound =
        List.fold_left
          (fun bound param ->
            match param.pparam_desc with
            | Pparam_val (_, default, pat) ->
                (* The default expression is evaluated outside the scope of
                   the parameters bound so far only in principle; being
                   conservative here is fine because optional-argument
                   defaults are not part of the predicate sublanguage. *)
                (match default with
                 | Some d when not bound -> walk_pred name d
                 | _ -> ());
                default_walk_pat name pat;
                bound || pattern_binds name pat
            | Pparam_newtype _ -> bound)
          false params
      in
      (match constraint_.ret_type_constraint with
       | Some (Pconstraint t) -> walk_type name t
       | Some (Pcoerce (t_opt, t)) ->
           Option.iter (walk_type name) t_opt;
           walk_type name t
       | None -> ());
      if not bound then
        match body with
        | Pfunction_body body -> walk_pred name body
        | Pfunction_cases (cases, _, _) -> List.iter (walk_case name) cases
      else ()
  | Pexp_match (scrutinee, cases) | Pexp_try (scrutinee, cases) ->
      walk_pred name scrutinee;
      List.iter (walk_case name) cases
  | Pexp_constraint (inner, ty_opt, _) ->
      walk_pred name inner;
      Option.iter (walk_type name) ty_opt
  | _ ->
      let it = child_walker name in
      Ast_iterator.default_iterator.expr it e

and walk_case name case =
  default_walk_pat name case.pc_lhs;
  if not (pattern_binds name case.pc_lhs) then begin
    Option.iter (walk_pred name) case.pc_guard;
    walk_pred name case.pc_rhs
  end

(* Walk only the interior types of a pattern (constraint annotations),
   where nested refinements can occur. *)
and default_walk_pat name pat =
  let it = child_walker name in
  Ast_iterator.default_iterator.pat it pat

and child_walker name =
  { Ast_iterator.default_iterator with
    typ = (fun _ t -> walk_type name t);
    expr = (fun _ e -> walk_pred name e);
    pat = (fun _ p -> default_walk_pat name p);
    (* Attribute and extension payloads are not part of the type. *)
    attribute = (fun _ _ -> ());
    extension = (fun _ _ -> ())
  }

let name_used_in_refinement name tys =
  match List.iter (walk_type name) tys with
  | () -> false
  | exception Found -> true

let name_used_in_predicate name pred =
  match walk_pred name pred with
  | () -> false
  | exception Found -> true
