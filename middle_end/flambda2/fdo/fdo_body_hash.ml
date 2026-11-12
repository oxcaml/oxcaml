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
module H = Fdo_prehash

(* The body hash guards the counters of a function body (see
   [Fdo_counter_identity]). Two bodies with the same hash must:

   1. number their calls and branches alike so that the counters of one refer to
   the same constructs in the other;

   2. connect them by the same explicit control flow, so that their counts
   relate alike.

   So, it hashes the "skeleton" of the Lambda tree, keeping only the following
   constructs, with what is listed for each:

   - [Lapply] and [Lsend];

   - [Lswitch]: its keys, [sw_numconsts], [sw_numblocks] and whether it has a
   default, which number its arms;

   - [Lstringswitch]: its keys and whether it has a default, which determine its
   tests;

   - [Lifthenelse], [&&] and [||]: the shape of the condition through [&&],
   [||], [not] and nested [if]s, and which conditions and arms are the constants
   [0] and [1], which determine the switches it becomes;

   - [Lwhile] and [Lfor];

   - [Pmakearray_dynamic] and [Parrayblit]: their array kinds, which determine
   their loops;

   - [Praise];

   - [Lstaticraise]: its handler, as the number of handlers between the raise
   and it.

   Everything else is left out: other constants and primitives, names, layouts,
   modes, attributes, locations, and nested functions, which number their own
   counters. Changing these can make a profile stale, but doesn't change what
   the counters refer to or how they relate.

   The hash is taken before the translation to Flambda, which creates the
   counters while rewriting the Lambda (in [Lambda_to_lambda_transforms],
   [Matching.expand_stringswitch] and [Lambda_to_flambda.cps_switch]). Any
   change to these that affects the numbering needs a matching change here, and
   a case in [oxcaml/tests/fdo/body_hash.ml]. *)

module Tag = struct
  let empty = 0

  let call = 1

  let send = 2

  let switch = 3

  let string_switch = 4

  let if_then_else = 5

  let while_ = 6

  let for_ = 7

  let makearray_dynamic = 8

  let arrayblit = 9

  let raise = 10

  let staticraise = 11

  (* Conditions and arms of [Lifthenelse] *)

  let false_ = 12

  let true_ = 13

  let and_ = 14

  let or_ = 15

  let not_ = 16

  let nested_if = 17

  let test = 18

  (* Other nodes *)

  let let_ = 19

  let prim = 20

  let sequence = 21

  let staticcatch = 22

  let trywith = 23
end

(* Given hashed children (or [None] if they're not included), hash a node.
   [None] if no children are included. *)
let node ~tag children =
  if List.for_all Option.is_none children
  then None
  else Some (H.node ~tag (List.map H.of_option children))

let construct ~tag components = Some (H.node ~tag components)

(* [handlers] are the static handlers in scope, innermost first. *)
let handler_index handlers label =
  match List.find_index (Static_label.equal label) handlers with
  | Some index -> index
  | None ->
    Misc.fatal_errorf "Fdo_body_hash: unbound static label %a"
      Static_label.format label

let bool_constant (lam : L.lambda) =
  match lam with
  | Lconst (Const_base (Const_int 0)) -> Some false
  | Lconst (Const_base (Const_int 1)) -> Some true
  | Lconst
      ( Const_base
          ( Const_int _ | Const_char _ | Const_untagged_char _ | Const_string _
          | Const_float _ | Const_float32 _ | Const_unboxed_float _
          | Const_unboxed_float32 _ | Const_int8 _ | Const_int16 _
          | Const_int32 _ | Const_int64 _ | Const_nativeint _
          | Const_untagged_int _ | Const_untagged_int8 _
          | Const_untagged_int16 _ | Const_unboxed_int32 _
          | Const_unboxed_int64 _ | Const_unboxed_nativeint _ )
      | Const_block _ | Const_mixed_block _ | Const_float_array _
      | Const_immstring _ | Const_float_block _ | Const_null )
  | Lvar _ | Lmutvar _ | Lapply _ | Lfunction _ | Llet _ | Lmutlet _ | Lletrec _
  | Lprim _ | Lswitch _ | Lstringswitch _ | Lstaticraise _ | Lstaticcatch _
  | Ltrywith _ | Lifthenelse _ | Lsequence _ | Lwhile _ | Lfor _ | Lassign _
  | Lsend _ | Levent _ | Lifused _ | Lregion _ | Lexclave _ | Lkindtemplate _
  | Lkindinstantiate _ | Ltemplate _ | Linstantiate _ ->
    None

let bool_node b = H.node ~tag:(if b then Tag.true_ else Tag.false_) []

(* We thread [handlers] through since we want to hash the destination of an
   [Lstaticraise] as an index as opposed to a flaky [Static_label.t] *)
let rec lambda handlers (lam : L.lambda) =
  match lam with
  | Lvar _ | Lmutvar _ | Lconst _ | Lfunction _ -> None
  | Lapply
      { ap_func;
        ap_args;
        ap_result_layout = _;
        ap_region_close = _;
        ap_mode = _;
        ap_yielding = _;
        ap_loc = _;
        ap_tailcall = _;
        ap_inlined = _;
        ap_specialised = _;
        ap_probe = _
      } ->
    construct ~tag:Tag.call
      [H.of_option (lambda handlers ap_func); arguments handlers ap_args]
  | Lsend (_kind, met, obj, args, _, _, _, _, _) ->
    construct ~tag:Tag.send
      [ H.of_option (lambda handlers met);
        H.of_option (lambda handlers obj);
        arguments handlers args ]
  | Lswitch (arg, sw, _loc, _layout) -> switch handlers arg sw
  | Lstringswitch (arg, cases, default, _loc, _layout) ->
    string_switch handlers arg cases default
  | Lifthenelse (cond, ifso, ifnot, _layout) ->
    if_then_else handlers ~cond ~ifso ~ifnot
  | Lwhile { wh_cond; wh_body } ->
    construct ~tag:Tag.while_
      [ H.of_option (lambda handlers wh_cond);
        H.of_option (lambda handlers wh_body) ]
  | Lfor
      { for_from;
        for_to;
        for_body;
        for_id = _;
        for_debug_uid = _;
        for_dir = _;
        for_loc = _
      } ->
    construct ~tag:Tag.for_
      [ H.of_option (lambda handlers for_from);
        H.of_option (lambda handlers for_to);
        H.of_option (lambda handlers for_body) ]
  | Lprim (prim, args, _loc) -> primitive handlers prim args
  | Llet (_, _, _, _, arg, body) | Lmutlet (_, _, _, arg, body) ->
    node ~tag:Tag.let_ [lambda handlers arg; lambda handlers body]
  | Lletrec (_bindings, body) -> lambda handlers body
  | Lsequence (first, second) ->
    node ~tag:Tag.sequence [lambda handlers first; lambda handlers second]
  | Lstaticraise (label, args) ->
    construct ~tag:Tag.staticraise
      [H.of_int (handler_index handlers label); arguments handlers args]
  | Lstaticcatch (body, (label, _params), handler, _pop_region, _layout) ->
    let handlers = label :: handlers in
    node ~tag:Tag.staticcatch [lambda handlers body; lambda handlers handler]
  | Ltrywith (body, _id, _debug_uid, handler, _layout) ->
    node ~tag:Tag.trywith [lambda handlers body; lambda handlers handler]
  | Lassign (_, e) | Lifused (_, e) | Levent (e, _) | Lregion (e, _) ->
    lambda handlers e
  | Lexclave e -> lambda handlers e
  (* Removed by [Slambda.eval] before the translation to Flambda. *)
  | Lkindtemplate _ | Lkindinstantiate _ | Ltemplate _ | Linstantiate _ ->
    L.fatal_error_invalid_constructor lam

and arguments handlers args =
  args |> List.map (fun lam -> lambda handlers lam |> H.of_option) |> H.of_list

and default handlers action =
  action
  |> Option.map (fun action -> H.of_option (lambda handlers action))
  |> H.of_option

and switch handlers arg
    ({ sw_numconsts; sw_consts; sw_numblocks; sw_blocks; sw_failaction } :
      L.lambda_switch) =
  let cases cases =
    cases
    |> List.map (fun (key, case) ->
        H.combine (H.of_int key) (H.of_option (lambda handlers case)))
    |> H.of_list
  in
  construct ~tag:Tag.switch
    [ H.of_option (lambda handlers arg);
      H.of_int sw_numconsts;
      cases sw_consts;
      H.of_int sw_numblocks;
      cases sw_blocks;
      default handlers sw_failaction ]

and string_switch handlers arg cases default_action =
  let cases =
    cases
    |> List.map (fun (key, case) ->
        H.combine (H.of_string key) (H.of_option (lambda handlers case)))
    |> H.of_list
  in
  construct ~tag:Tag.string_switch
    [H.of_option (lambda handlers arg); cases; default handlers default_action]

and primitive handlers (prim : L.primitive) args =
  match[@ocaml.warning "-fragile-match"] prim, args with
  | Psequand, [a; b] ->
    if_then_else handlers ~cond:a ~ifso:b
      ~ifnot:(Lconst (Const_base (Const_int 0)) : L.lambda)
  | Psequor, [a; b] ->
    if_then_else handlers ~cond:a
      ~ifso:(Lconst (Const_base (Const_int 1)) : L.lambda)
      ~ifnot:b
  | Pmakearray_dynamic (kind, _mode, initializer_), _ ->
    construct ~tag:Tag.makearray_dynamic
      [ H.of_string (Printlambda.array_kind kind);
        H.of_int
          (match initializer_ with With_initializer -> 1 | Uninitialized -> 0);
        arguments handlers args ]
  | Parrayblit { src_mutability = _; dst_array_set_kind }, _ ->
    construct ~tag:Tag.arrayblit
      [ H.of_string
          (Printlambda.array_kind
             (L.array_kind_of_array_set_kind dst_array_set_kind));
        arguments handlers args ]
  | Praise _kind, _ -> construct ~tag:Tag.raise [arguments handlers args]
  | _, _ -> args |> List.map (lambda handlers) |> node ~tag:Tag.prim

and if_then_else handlers ~cond ~ifso ~ifnot =
  construct ~tag:Tag.if_then_else
    [condition handlers cond; if_arm handlers ifso; if_arm handlers ifnot]

and condition handlers (cond : L.lambda) =
  match bool_constant cond with
  | Some b -> bool_node b
  | None -> (
    match cond with
    | Lprim (prim, args, _loc) -> condition_primitive handlers prim args ~cond
    | Lifthenelse (a, b, c, _layout) -> (
      (* As in [switch_for_if_then_else]: [if a then b else false] is [a && b]
         and [if a then true else b] is [a || b]. *)
      match bool_constant b, bool_constant c with
      | _, Some false ->
        H.node ~tag:Tag.and_ [condition handlers a; condition handlers b]
      | Some true, _ ->
        H.node ~tag:Tag.or_ [condition handlers a; condition handlers c]
      | (None | Some false), (None | Some true) ->
        H.node ~tag:Tag.nested_if
          [condition handlers a; condition handlers b; condition handlers c])
    | Lvar _ | Lmutvar _ | Lconst _ | Lapply _ | Lfunction _ | Llet _
    | Lmutlet _ | Lletrec _ | Lswitch _ | Lstringswitch _ | Lstaticraise _
    | Lstaticcatch _ | Ltrywith _ | Lsequence _ | Lwhile _ | Lfor _ | Lassign _
    | Lsend _ | Levent _ | Lifused _ | Lregion _ | Lexclave _ | Lkindtemplate _
    | Lkindinstantiate _ | Ltemplate _ | Linstantiate _ ->
      test handlers cond)

and condition_primitive handlers (prim : L.primitive) args ~cond =
  match[@ocaml.warning "-fragile-match"] prim, args with
  | Psequand, [a; b] ->
    H.node ~tag:Tag.and_ [condition handlers a; condition handlers b]
  | Psequor, [a; b] ->
    H.node ~tag:Tag.or_ [condition handlers a; condition handlers b]
  | Pnot, [c] -> H.node ~tag:Tag.not_ [condition handlers c]
  | _, _ -> test handlers cond

and test handlers cond =
  H.node ~tag:Tag.test [H.of_option (lambda handlers cond)]

and if_arm handlers arm =
  match bool_constant arm with
  | Some b -> bool_node b
  | None -> H.of_option (lambda handlers arm)

let to_body_hash skeleton =
  Option.value skeleton ~default:(H.node ~tag:Tag.empty [])
  |> H.to_int32 |> FC.Body_hash.of_int32

let of_function (fn : L.lfunction) = fn.body |> lambda [] |> to_body_hash

let of_toplevel module_initializer =
  module_initializer |> lambda [] |> to_body_hash
