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
module FC = Fdo_counter

(* CR yusumez: We need to check whether this is a good way to identify
   functions *)
module Leading_tokens = struct
  (* CR yusumez: As above. These are chosen mostly for stability across
     edits. *)
  let primitive_token (prim : L.primitive) =
    match[@ocaml.warning "-4"] prim with
    | Pccall p -> p.prim_name
    | Pgetglobal (cu, _) -> Compilation_unit.name_as_string cu
    | Pgetpredef id -> Ident.name id
    | _ -> Printlambda.name_of_primitive prim

  let leading_tokens ?(limit = 5) ~params body =
    let tokens = ref [] in
    let exception Enough in
    let mention token =
      if not (List.mem token !tokens)
      then (
        tokens := token :: !tokens;
        if List.length !tokens >= limit then raise Enough)
    in
    let ident id = mention (Ident.name id) in
    let rec term (lam : L.lambda) =
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
      (* CR yusumez: Other types of constants? *)
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
      | Lapply _ | Lswitch _ | Lstringswitch _ | Lstaticraise _ | Lifthenelse _
      | Lsequence _ | Lwhile _ | Lsend _ | Levent _ | Lregion _ | Lexclave _
      | Lsplice _ | Lkindtemplate _ | Lkindinstantiate _ | Ltemplate _
      | Linstantiate _ ->
        ());
      L.iter_head_constructor term lam
    in
    (try
       List.iter ident params;
       term body
     with Enough -> ());
    List.rev !tokens
end

module Naming_context = struct
  type t =
    { enclosing_path : string;
      occurrences : int Misc.Stdlib.String.Tbl.t
          (** The number of functions already given each path in the unit,
              shared by all of its contexts. *)
    }

  let toplevel compilation_unit =
    { enclosing_path = Compilation_unit.full_path_as_string compilation_unit;
      occurrences = Misc.Stdlib.String.Tbl.create 64
    }

  let fresh_discriminator t ~path =
    let index =
      Option.value
        (Misc.Stdlib.String.Tbl.find_opt t.occurrences path)
        ~default:0
    in
    Misc.Stdlib.String.Tbl.replace t.occurrences path (index + 1);
    index
end

type t = { function_ : FC.Function.t }

let of_function (context : Naming_context.t) (fn : L.lfunction) =
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
    | Loc_unknown -> context.enclosing_path ^ ".(fun)", true
  in
  let path =
    if anonymous
    then
      path ^ "@"
      ^ String.concat "," (Leading_tokens.leading_tokens ~params body)
    else path
  in
  let function_ =
    let discriminator = Naming_context.fresh_discriminator context ~path in
    let name = FC.Name.create ~name_str:path ~discriminator in
    FC.Function.create_original name
  in
  { function_ }, { context with enclosing_path = path }

let function_ t = t.function_

let leading_tokens = Leading_tokens.leading_tokens

let entry_annotation t =
  let counter =
    t.function_ |> FC.Position.create_function_entry |> FC.of_position
  in
  Fdo_annotation.of_list [counter]
