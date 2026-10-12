(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
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

module L = Lambda

(* CR yusumez: We need to check whether this is a good way to identify
   functions *)
module Leading_tokens = struct
  (* A compact spelling of a primitive.

     CR yusumez: do we need to discriminate primitives further? *)
  let primitive_token (prim : L.primitive) =
    match[@ocaml.warning "-4"] prim with
    | Pccall { prim_name; _ } -> prim_name
    | _ -> Printlambda.name_of_primitive prim

  let leading_tokens ~params body =
    let limit = 5 in
    let tokens = ref [] in
    let exception Enough in
    let mention token =
      if not (List.mem token !tokens)
      then (
        tokens := token :: !tokens;
        if List.length !tokens >= limit then raise Enough)
    in
    let ident id = mention (Ident.name id) in
    (* A value of another compilation unit, named by unit and offset. The match
       is non-exhaustive since the shape is specific. *)
    let global_field (lam : L.lambda) =
      match[@ocaml.warning "-4"] lam with
      | Lprim
          ( Pfield (n, _ptr, _sem),
            [Lprim (Pgetglobal (cu, _layout), [], _loc)],
            _loc' ) ->
        Some (Compilation_unit.name_as_string cu ^ "." ^ string_of_int n)
      | _ -> None
    in
    let rec term (lam : L.lambda) =
      match global_field lam with
      | Some token -> mention token
      | None ->
        (match lam with
        | Lvar id -> ident id
        | Lmutvar id -> ident id
        | Llet (_let_kind, _layout, id, _debug_uid, _arg, _body) -> ident id
        | Lmutlet (_layout, id, _debug_uid, _arg, _body) -> ident id
        | Ltrywith (_body, id, _debug_uid, _handler, _layout) -> ident id
        | Lassign (id, _arg) -> ident id
        | Lifused (id, _arg) -> ident id
        | Lfor { for_id; _ } -> ident for_id
        | Lfunction { params; _ } ->
          List.iter (fun (p : L.lparam) -> ident p.name) params
        | Lletrec (bindings, _body) ->
          List.iter (fun (b : L.rec_binding) -> ident b.id) bindings
        | Lstaticcatch (_body, (_label, params), _handler, _pop_region, _layout)
          ->
          List.iter (fun (id, _debug_uid, _layout) -> ident id) params
        | Lprim (prim, _args, _loc) -> mention (primitive_token prim)
        | Lconst (Const_base (Const_int n)) -> mention (string_of_int n)
        | Lconst
            (Const_base
               ( Const_char _ | Const_untagged_char _ | Const_string _
               | Const_float _ | Const_float32 _ | Const_unboxed_float _
               | Const_unboxed_float32 _ | Const_int8 _ | Const_int16 _
               | Const_int32 _ | Const_int64 _ | Const_nativeint _
               | Const_untagged_int _ | Const_untagged_int8 _
               | Const_untagged_int16 _ | Const_unboxed_int32 _
               | Const_unboxed_int64 _ | Const_unboxed_nativeint _ ))
        | Lconst
            ( Const_block _ | Const_mixed_block _ | Const_float_array _
            | Const_immstring _ | Const_float_block _ | Const_null )
        | Lapply _ | Lswitch _ | Lstringswitch _ | Lstaticraise _
        | Lifthenelse _ | Lsequence _ | Lwhile _ | Lsend _ | Levent _
        | Lregion _ | Lexclave _ | Lsplice _ | Lkindtemplate _
        | Lkindinstantiate _ | Ltemplate _ | Linstantiate _ ->
          ());
        L.shallow_iter ~tail:term ~non_tail:term lam
    in
    (try
       List.iter ident params;
       term body
     with Enough -> ());
    List.rev !tokens
end

type t =
  { definition_id : Fdo_counter.Definition_id.t;
    path : string
  }

let of_function ~fresh_discriminator ~enclosing_path (fn : L.lfunction) =
  let { L.loc; params; body; _ } = fn in
  let params = List.map (fun (p : L.lparam) -> p.name) params in
  let path, anonymous =
    match loc with
    | Loc_known { scopes; loc = _ } ->
      let anonymous =
        match scopes with
        | Cons
            { item = Sc_anonymous_function | Sc_partial_or_eta_wrapper | Sc_lazy;
              _
            } ->
          true
        | Cons
            { item =
                ( Sc_value_definition | Sc_module_definition
                | Sc_class_definition | Sc_method_definition );
              _
            }
        | Empty ->
          false
      in
      ( Debuginfo.Scoped_location.string_of_scopes ~include_zero_alloc:false
          scopes,
        anonymous )
    | Loc_unknown -> enclosing_path ^ ".(fun)", true
  in
  let path =
    if anonymous
    then
      path ^ "@"
      ^ String.concat "," (Leading_tokens.leading_tokens ~params body)
    else path
  in
  let discriminator = fresh_discriminator ~path in
  { definition_id =
      Fdo_counter.Definition_id.create_original ~demangled_name:path
        ~discriminator;
    path
  }

let definition_id t = t.definition_id

let path t = t.path

let leading_tokens = Leading_tokens.leading_tokens

let entry_annotation t =
  Fdo_annotation.of_list
    [ { Fdo_counter.position =
          Fdo_counter.Position.create_function_entry t.definition_id;
        inlining_stack = []
      } ]
