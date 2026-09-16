(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open! Flambda.Import
module PTA = Points_to_analysis

module Applications = struct
  type bounds =
    { known : int option;
      unknown : int list option
    }

  type t = bounds Code_id_or_name.Map.t

  let empty = Code_id_or_name.Map.empty

  let union_option f a b =
    match a, b with None, x | x, None -> x | Some a, Some b -> Some (f a b)

  let rec max_widths a b =
    match a, b with
    | [], widths | widths, [] -> widths
    | a :: rest_a, b :: rest_b -> max a b :: max_widths rest_a rest_b

  let union_bounds a b =
    { known = union_option max a.known b.known;
      unknown = union_option max_widths a.unknown b.unknown
    }

  let add_apply t apply =
    match Apply.call_kind apply, Apply.callee apply with
    | Function { function_call }, Some callee ->
      Simple.pattern_match callee
        ~const:(fun _ -> t)
        ~name:(fun name ~coercion:_ ->
          let bounds =
            match function_call with
            | Direct _ | Indirect_known_arity _ ->
              { known = Some (List.length (Apply.args apply)); unknown = None }
            | Indirect_unknown_arity ->
              let groups =
                Flambda_arity.group_by_parameter (Apply.args_arity apply)
                  (Apply.args apply)
              in
              { known = None; unknown = Some (List.map List.length groups) }
          in
          Code_id_or_name.Map.update
            (Code_id_or_name.name name)
            (fun previous ->
              Some
                (match previous with
                | None -> bounds
                | Some previous -> union_bounds previous bounds))
            t)
    | Function _, None | (C_call _ | Method _ | Effect _), _ -> t

  let union a b =
    Code_id_or_name.Map.union (fun _ a b -> Some (union_bounds a b)) a b
end

(* We use unit maps instead of sets, because it allows reuse of the tables
   stored in the Datalog database without copying. *)
type t =
  { has_usage : unit Code_id_or_name.Map.t;
    has_source : unit Code_id_or_name.Map.t;
    field_of_constructor_is_used : unit Field.Map.t Code_id_or_name.Map.t;
    directly_called : Code_id.Set.t Or_unknown.t Code_id_or_name.Map.t;
    known_masks : PTA.keep_or_delete list Code_id_or_name.Map.t;
    unknown_masks : PTA.keep_or_delete list list Code_id_or_name.Map.t
  }

let empty =
  { has_usage = Code_id_or_name.Map.empty;
    has_source = Code_id_or_name.Map.empty;
    field_of_constructor_is_used = Code_id_or_name.Map.empty;
    directly_called = Code_id_or_name.Map.empty;
    known_masks = Code_id_or_name.Map.empty;
    unknown_masks = Code_id_or_name.Map.empty
  }

let create db ~applications =
  let t =
    { empty with
      has_usage = Datalog.get_table PTA.Relations.has_usage_table db;
      has_source = Datalog.get_table PTA.Relations.has_source_table db;
      field_of_constructor_is_used =
        Datalog.get_table PTA.Relations.field_of_constructor_is_used_tbl db
    }
  in
  (* The argument queries only inspect argument positions, so they are run over
     unit placeholders at the maximal recorded width; call sites slice the
     resulting masks down to their own arguments. *)
  let dummy_args width = List.init width (fun _ -> ()) in
  Code_id_or_name.Map.fold
    (fun callee ({ known; unknown } : Applications.bounds) t ->
      let t =
        match known with
        | None -> t
        | Some width ->
          let name =
            Code_id_or_name.pattern_match' callee
              ~name:(fun name -> name)
              ~code_id:(fun _ ->
                Misc.fatal_errorf
                  "Rebuild_queries: expected a named callee, found %a"
                  Code_id_or_name.print callee)
          in
          let directly_called = PTA.code_id_actually_directly_called db name in
          let mask =
            PTA.arguments_used_by_known_arity_call db callee (dummy_args width)
            |> List.map snd
          in
          { t with
            directly_called =
              Code_id_or_name.Map.add callee directly_called t.directly_called;
            known_masks = Code_id_or_name.Map.add callee mask t.known_masks
          }
      in
      match unknown with
      | None -> t
      | Some widths ->
        let masks =
          PTA.arguments_used_by_unknown_arity_call db callee
            (List.map dummy_args widths)
          |> List.map (List.map snd)
        in
        { t with
          unknown_masks = Code_id_or_name.Map.add callee masks t.unknown_masks
        })
    applications t

let has_use t id = Code_id_or_name.Map.mem id t.has_usage

let has_source t id = Code_id_or_name.Map.mem id t.has_source

let field_used t id field =
  match Code_id_or_name.Map.find_opt id t.field_of_constructor_is_used with
  | None -> false
  | Some fields -> Field.Map.mem field fields

let find_answer map callee query =
  match Code_id_or_name.Map.find_opt callee map with
  | Some answer -> answer
  | None ->
    Misc.fatal_errorf "Rebuild_queries: no %s request for callee %a" query
      Code_id_or_name.print callee

let code_id_actually_directly_called t name =
  find_answer t.directly_called
    (Code_id_or_name.name name)
    "direct-call targets"

(* [mask] was computed at the maximal width recorded for [callee], so a call
   site with fewer arguments uses a prefix of it. *)
let rec apply_mask callee query mask args =
  match args, mask with
  | [], _ -> []
  | arg :: args, keep :: mask ->
    (arg, keep) :: apply_mask callee query mask args
  | _ :: _, [] ->
    Misc.fatal_errorf
      "Rebuild_queries: insufficient %s argument width for callee %a" query
      Code_id_or_name.print callee

let arguments_used_by_known_arity_call t callee args =
  let mask = find_answer t.known_masks callee "known-arity" in
  apply_mask callee "known-arity" mask args

let arguments_used_by_unknown_arity_call t callee args =
  let masks = find_answer t.unknown_masks callee "unknown-arity" in
  let rec apply_groups masks args =
    match args, masks with
    | [], _ -> []
    | args :: rest, mask :: masks ->
      apply_mask callee "unknown-arity" mask args :: apply_groups masks rest
    | _ :: _, [] ->
      Misc.fatal_errorf
        "Rebuild_queries: insufficient unknown-arity argument groups for \
         callee %a"
        Code_id_or_name.print callee
  in
  apply_groups masks args
