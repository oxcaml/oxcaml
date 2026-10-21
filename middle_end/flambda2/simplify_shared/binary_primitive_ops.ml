(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2026 OCamlPro SAS                                    *)
(*   Copyright 2014--2026 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module A = Number_adjuncts
module K = Flambda_kind
module P = Flambda_primitive
module T = Flambda2_types

type 'a binary_arith_outcome_for_one_side_only =
  | Exactly of 'a
  | The_other_side
  | Negation_of_the_other_side
  | Float_negation_of_the_other_side of Flambda_primitive.float_bitwidth
  | Cannot_simplify
  | Invalid

module type Binary_arith_like_sig = sig
  module Lhs : Container_types.S

  module Rhs : Container_types.S

  module Pair : Container_types.S with type t = Lhs.t * Rhs.t

  module Result : Container_types.S

  val cross_product : Lhs.Set.t -> Rhs.Set.t -> Pair.Set.t

  val arg_kind : K.Standard_int_or_float.t

  val result_kind : K.t

  val term : Result.t -> Flambda.Named.t

  val prover_lhs : T.Typing_env.t -> T.t -> Lhs.Set.t T.meet_shortcut

  val prover_rhs : T.Typing_env.t -> T.t -> Rhs.Set.t T.meet_shortcut

  type op

  val unknown : machine_width:Target_system.Machine_width.t -> op -> T.t

  val these : Result.Set.t -> T.t * Simple.t option

  val op :
    machine_width:Target_system.Machine_width.t ->
    op ->
    Lhs.t ->
    Rhs.t ->
    Result.t option

  val op_lhs_unknown :
    machine_width:Target_system.Machine_width.t ->
    op ->
    rhs:Rhs.t ->
    Result.t binary_arith_outcome_for_one_side_only

  val op_rhs_unknown :
    machine_width:Target_system.Machine_width.t ->
    op ->
    lhs:Lhs.t ->
    Result.t binary_arith_outcome_for_one_side_only
end

module Int_ops_for_binary_arith (I : A.Int_number_kind) : sig
  include
    Binary_arith_like_sig
      with type op = P.binary_int_arith_op
       and type Lhs.t = I.Num.t
       and type Rhs.t = I.Num.t
       and type Result.t = I.Num.t
end = struct
  module Lhs = I.Num
  module Rhs = I.Num
  module Result = I.Num

  type op = P.binary_int_arith_op

  let arg_kind = I.standard_int_or_float_kind

  let result_kind = K.Standard_int_or_float.to_kind arg_kind

  let prover_lhs = I.unboxed_prover

  let prover_rhs = I.unboxed_prover

  let unknown ~machine_width:_ _ =
    match arg_kind with
    | Tagged_immediate -> T.any_tagged_immediate
    | Naked_immediate -> T.any_naked_immediate
    | Naked_float32 -> T.any_naked_float32
    | Naked_float -> T.any_naked_float
    | Naked_int8 -> T.any_naked_int8
    | Naked_int16 -> T.any_naked_int16
    | Naked_int32 -> T.any_naked_int32
    | Naked_int64 -> T.any_naked_int64
    | Naked_nativeint -> T.any_naked_nativeint

  let these s =
    let ty = I.these_unboxed s in
    let simple_opt =
      match I.Num.Set.get_singleton s with
      | None -> None
      | Some i -> Some (Simple.const (I.Num.to_const i))
    in
    ty, simple_opt

  let term = I.term_unboxed

  module Pair = I.Num.Pair

  let cross_product = I.Num.cross_product

  let op ~machine_width:_ (op : P.binary_int_arith_op) n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Add -> always_some I.Num.add
    | Sub -> always_some I.Num.sub
    | Mul -> always_some I.Num.mul
    | Div Signed -> I.Num.div n1 n2
    | Mod Signed -> I.Num.mod_ n1 n2
    | Div Unsigned -> I.Num.unsigned_div n1 n2
    | Mod Unsigned -> I.Num.unsigned_mod n1 n2
    | And -> always_some I.Num.and_
    | Or -> always_some I.Num.or_
    | Xor -> always_some I.Num.xor

  type symmetric_op =
    | Add
    | Mul
    | And
    | Or
    | Xor

  module Num = I.Num

  let symmetric_op_one_side_unknown ~machine_width (op : symmetric_op)
      ~this_side : Num.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add ->
      if Num.equal this_side (Num.zero machine_width)
      then The_other_side
      else Cannot_simplify
    | Mul ->
      if Num.equal this_side (Num.zero machine_width)
      then Exactly (Num.zero machine_width)
      else if Num.equal this_side (Num.one machine_width)
      then The_other_side
      else if Num.equal this_side (Num.minus_one machine_width)
      then Negation_of_the_other_side
      else Cannot_simplify
    | And ->
      if Num.equal this_side (Num.minus_one machine_width)
      then The_other_side
      else if Num.equal this_side (Num.zero machine_width)
      then Exactly (Num.zero machine_width)
      else Cannot_simplify
    | Or ->
      if Num.equal this_side (Num.minus_one machine_width)
      then Exactly (Num.minus_one machine_width)
      else if Num.equal this_side (Num.zero machine_width)
      then The_other_side
      else Cannot_simplify
    | Xor ->
      if Num.equal this_side (Num.zero machine_width)
      then The_other_side
      else Cannot_simplify

  let op_lhs_unknown ~machine_width (op : P.binary_int_arith_op) ~rhs :
      Num.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add -> symmetric_op_one_side_unknown ~machine_width Add ~this_side:rhs
    | Mul -> symmetric_op_one_side_unknown ~machine_width Mul ~this_side:rhs
    | And -> symmetric_op_one_side_unknown ~machine_width And ~this_side:rhs
    | Or -> symmetric_op_one_side_unknown ~machine_width Or ~this_side:rhs
    | Xor -> symmetric_op_one_side_unknown ~machine_width Xor ~this_side:rhs
    | Sub ->
      if Num.equal rhs (Num.zero machine_width)
      then The_other_side
      else Cannot_simplify
    | Div signedness ->
      (* Division ("safe" division, strictly speaking, in Lambda terminology) is
         translated to a conditional on the denominator followed by an unsafe
         division (the "Div" seen here) on the way into Flambda 2. So if the
         denominator turns out to be zero here, via the typing or whatever, then
         we're in unreachable code. *)
      (* CR-someday mshinwell: Should we expose unsafe division to the user? *)
      if Num.equal rhs (Num.zero machine_width)
      then Invalid
      else if Num.equal rhs (Num.one machine_width)
      then The_other_side
      else if
        Num.equal rhs (Num.minus_one machine_width)
        && match signedness with Signed -> true | Unsigned -> false
      then
        Negation_of_the_other_side (* CR mshinwell: Add 0 / x = 0 when x <> 0 *)
      else Cannot_simplify
    | Mod signedness ->
      (* CR mshinwell: We could be more clever for Mod and And *)
      if Num.equal rhs (Num.zero machine_width)
      then Invalid
      else if Num.equal rhs (Num.one machine_width)
      then Exactly (Num.zero machine_width)
      else if
        Num.equal rhs (Num.minus_one machine_width)
        && match signedness with Signed -> true | Unsigned -> false
      then Exactly (Num.zero machine_width)
      else Cannot_simplify

  let op_rhs_unknown ~machine_width (op : P.binary_int_arith_op) ~lhs :
      Num.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add -> symmetric_op_one_side_unknown ~machine_width Add ~this_side:lhs
    | Mul -> symmetric_op_one_side_unknown ~machine_width Mul ~this_side:lhs
    | And -> symmetric_op_one_side_unknown ~machine_width And ~this_side:lhs
    | Or -> symmetric_op_one_side_unknown ~machine_width Or ~this_side:lhs
    | Xor -> symmetric_op_one_side_unknown ~machine_width Xor ~this_side:lhs
    | Sub ->
      if Num.equal lhs (Num.zero machine_width)
      then Negation_of_the_other_side
      else Cannot_simplify
    | Div (Signed | Unsigned) | Mod (Signed | Unsigned) -> Cannot_simplify
end
[@@inline always]

module Int_ops_for_binary_shift (I : A.Int_number_kind) : sig
  include
    Binary_arith_like_sig
      with type op = P.int_shift_op
       and type Lhs.t = I.Num.t
       and type Rhs.t = Target_ocaml_int.t
       and type Result.t = I.Num.t
end = struct
  module Lhs = I.Num
  module Rhs = Target_ocaml_int
  module Result = I.Num

  type op = P.int_shift_op

  let arg_kind = I.standard_int_or_float_kind

  let result_kind = K.Standard_int_or_float.to_kind arg_kind

  let prover_lhs = I.unboxed_prover

  let prover_rhs = T.meet_naked_immediates

  let unknown ~machine_width:_ _ =
    match arg_kind with
    | Tagged_immediate -> T.any_tagged_immediate
    | Naked_immediate -> T.any_naked_immediate
    | Naked_float32 -> T.any_naked_float32
    | Naked_float -> T.any_naked_float
    | Naked_int8 -> T.any_naked_int8
    | Naked_int16 -> T.any_naked_int16
    | Naked_int32 -> T.any_naked_int32
    | Naked_int64 -> T.any_naked_int64
    | Naked_nativeint -> T.any_naked_nativeint

  let these s =
    let ty = I.these_unboxed s in
    let simple_opt =
      match I.Num.Set.get_singleton s with
      | None -> None
      | Some i -> Some (Simple.const (I.Num.to_const i))
    in
    ty, simple_opt

  let term = I.term_unboxed

  (* CR-someday mshinwell: One day this should maybe be in a standard library *)
  module Pair = struct
    type nonrec t = Lhs.t * Rhs.t

    include Container_types.Make_pair (Lhs) (Rhs)
  end

  let cross_product set1 set2 =
    Lhs.Set.fold
      (fun elt1 result ->
        Rhs.Set.fold
          (fun elt2 result -> Pair.Set.add (elt1, elt2) result)
          set2 result)
      set1 Pair.Set.empty

  module Num = I.Num

  let op ~machine_width:_ (op : P.int_shift_op) n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Lsl -> always_some Num.shift_left
    | Lsr -> always_some Num.shift_right_logical
    | Asr -> always_some Num.shift_right

  let op_lhs_unknown ~machine_width (op : P.int_shift_op) ~rhs :
      Num.t binary_arith_outcome_for_one_side_only =
    let module O = Target_ocaml_int in
    let rhs = rhs in
    match op with
    | Lsl | Lsr | Asr ->
      (* Shifting either way by [Targetint_32_64.size] or above, or by a
         negative amount, is undefined.

         However note that we cannot produce [Invalid] unless the code is type
         unsafe, which it is not here. (Otherwise a GADT match might be reduced
         to only one possible case which it would be wrong to take.) *)
      if O.equal rhs (O.zero machine_width)
      then The_other_side
      else Cannot_simplify

  let op_rhs_unknown ~machine_width (op : P.int_shift_op) ~lhs :
      Num.t binary_arith_outcome_for_one_side_only =
    (* In these cases we are giving a semantics for some cases where the
       right-hand side may be less than zero or greater than or equal to
       [Targetint_32_64.size]. These cases have undefined semantics, as above;
       however, it seems fine to give them a semantics since there is benefit to
       doing so in this particular case. (This is not the case for the situation
       in [op_lhs_unknown], above, where there would be no such benefit.) *)
    match op with
    | Lsl | Lsr ->
      if Num.equal lhs (Num.zero machine_width)
      then Exactly (Num.zero machine_width)
      else Cannot_simplify
    | Asr ->
      if Num.equal lhs (Num.zero machine_width)
      then Exactly (Num.zero machine_width)
      else if Num.equal lhs (Num.minus_one machine_width)
      then Exactly (Num.minus_one machine_width)
      else Cannot_simplify
end
[@@inline always]

module Int_ops_for_binary_comp (I : A.Int_number_kind) : sig
  include
    Binary_arith_like_sig
      with type op = P.signed_or_unsigned P.comparison_behaviour
       and type Lhs.t = I.Num.t
       and type Rhs.t = I.Num.t
       and type Result.t = Target_ocaml_int.t
end = struct
  module Lhs = I.Num
  module Rhs = I.Num
  module Result = Target_ocaml_int

  type op = P.signed_or_unsigned P.comparison_behaviour

  let arg_kind = I.standard_int_or_float_kind

  let result_kind = K.naked_immediate

  let prover_lhs = I.unboxed_prover

  let prover_rhs = I.unboxed_prover

  let unknown ~machine_width (op : op) =
    match op with
    | Yielding_bool _ ->
      T.these_naked_immediates (Target_ocaml_int.all_bools machine_width)
    | Yielding_int_like_compare_functions _signedness ->
      T.these_naked_immediates
        (Target_ocaml_int.zero_one_and_minus_one machine_width)

  let these s =
    let ty = T.these_naked_immediates s in
    let simple_opt =
      match Target_ocaml_int.Set.get_singleton s with
      | None -> None
      | Some i -> Some (Simple.const (Reg_width_const.naked_immediate i))
    in
    ty, simple_opt

  let term imm : Flambda.Named.t =
    Flambda.Named.create_simple
      (Simple.const (Reg_width_const.naked_immediate imm))

  module Pair = I.Num.Pair

  let cross_product = I.Num.cross_product

  module Num = I.Num

  let op ~machine_width (op : P.signed_or_unsigned P.comparison_behaviour) n1 n2
      =
    match op with
    | Yielding_bool op -> (
      let bool b = Target_ocaml_int.bool machine_width b in
      match op with
      | Eq -> Some (bool (Num.compare n1 n2 = 0))
      | Neq -> Some (bool (Num.compare n1 n2 <> 0))
      | Lt Signed -> Some (bool (Num.compare n1 n2 < 0))
      | Gt Signed -> Some (bool (Num.compare n1 n2 > 0))
      | Le Signed -> Some (bool (Num.compare n1 n2 <= 0))
      | Ge Signed -> Some (bool (Num.compare n1 n2 >= 0))
      | Lt Unsigned -> Some (bool (Num.compare_unsigned n1 n2 < 0))
      | Gt Unsigned -> Some (bool (Num.compare_unsigned n1 n2 > 0))
      | Le Unsigned -> Some (bool (Num.compare_unsigned n1 n2 <= 0))
      | Ge Unsigned -> Some (bool (Num.compare_unsigned n1 n2 >= 0)))
    | Yielding_int_like_compare_functions signed_or_unsigned -> (
      match signed_or_unsigned with
      | Signed ->
        let int i = Target_ocaml_int.of_int machine_width i in
        let c = Num.compare n1 n2 in
        if c < 0
        then Some (int (-1))
        else if c = 0
        then Some (int 0)
        else Some (int 1)
      | Unsigned ->
        let int i = Target_ocaml_int.of_int machine_width i in
        let c = Num.compare_unsigned n1 n2 in
        if c < 0
        then Some (int (-1))
        else if c = 0
        then Some (int 0)
        else Some (int 1))

  let op_lhs_unknown ~machine_width:_ _op ~rhs:_ = Cannot_simplify

  let op_rhs_unknown ~machine_width:_ _op ~lhs:_ = Cannot_simplify
end
[@@inline always]

(* Constant folding of integer binary primitives on known constants, for use
   outside the simplifier (e.g. by [Closure_conversion] in classic mode, where
   the simplifier does not run). This reuses the evaluation functions above so
   that the semantics cannot diverge from those of the simplifier. *)
module Fold_binary_int_prims (I : A.Int_number_kind) : sig
  val arith :
    machine_width:Target_system.Machine_width.t ->
    P.binary_int_arith_op ->
    Reg_width_const.t ->
    Reg_width_const.t ->
    Reg_width_const.t option

  val shift :
    machine_width:Target_system.Machine_width.t ->
    P.int_shift_op ->
    Reg_width_const.t ->
    Reg_width_const.t ->
    Reg_width_const.t option

  val comp :
    machine_width:Target_system.Machine_width.t ->
    P.signed_or_unsigned P.comparison_behaviour ->
    Reg_width_const.t ->
    Reg_width_const.t ->
    Reg_width_const.t option
end = struct
  module Arith = Int_ops_for_binary_arith (I)
  module Shift = Int_ops_for_binary_shift (I)
  module Comp = Int_ops_for_binary_comp (I)

  let arith ~machine_width op c1 c2 =
    Option.bind (I.Num.of_const c1) (fun n1 ->
        Option.bind (I.Num.of_const c2) (fun n2 ->
            Option.map I.Num.to_const (Arith.op ~machine_width op n1 n2)))

  let shift ~machine_width op c1 c2 =
    Option.bind (I.Num.of_const c1) (fun n1 ->
        Option.bind (Reg_width_const.is_naked_immediate c2) (fun n2 ->
            Option.map I.Num.to_const (Shift.op ~machine_width op n1 n2)))

  let comp ~machine_width op c1 c2 =
    Option.bind (I.Num.of_const c1) (fun n1 ->
        Option.bind (I.Num.of_const c2) (fun n2 ->
            Option.map Reg_width_const.naked_immediate
              (Comp.op ~machine_width op n1 n2)))
end

module Fold_tagged_immediate = Fold_binary_int_prims (A.For_tagged_immediates)
module Fold_naked_immediate = Fold_binary_int_prims (A.For_naked_immediates)
module Fold_int8 = Fold_binary_int_prims (A.For_int8s)
module Fold_int16 = Fold_binary_int_prims (A.For_int16s)
module Fold_int32 = Fold_binary_int_prims (A.For_int32s)
module Fold_int64 = Fold_binary_int_prims (A.For_int64s)
module Fold_nativeint = Fold_binary_int_prims (A.For_nativeints)

let fold_binary_int_primitive ~machine_width (prim : P.binary_primitive) c1 c2 =
  match[@warning "-fragile-match"] prim with
  | Int_arith (kind, op) ->
    let arith =
      match kind with
      | Tagged_immediate -> Fold_tagged_immediate.arith
      | Naked_immediate -> Fold_naked_immediate.arith
      | Naked_int8 -> Fold_int8.arith
      | Naked_int16 -> Fold_int16.arith
      | Naked_int32 -> Fold_int32.arith
      | Naked_int64 -> Fold_int64.arith
      | Naked_nativeint -> Fold_nativeint.arith
    in
    arith ~machine_width op c1 c2
  | Int_shift (kind, op) ->
    let shift =
      match kind with
      | Tagged_immediate -> Fold_tagged_immediate.shift
      | Naked_immediate -> Fold_naked_immediate.shift
      | Naked_int8 -> Fold_int8.shift
      | Naked_int16 -> Fold_int16.shift
      | Naked_int32 -> Fold_int32.shift
      | Naked_int64 -> Fold_int64.shift
      | Naked_nativeint -> Fold_nativeint.shift
    in
    shift ~machine_width op c1 c2
  | Int_comp (kind, op) ->
    let comp =
      match kind with
      | Tagged_immediate -> Fold_tagged_immediate.comp
      | Naked_immediate -> Fold_naked_immediate.comp
      | Naked_int8 -> Fold_int8.comp
      | Naked_int16 -> Fold_int16.comp
      | Naked_int32 -> Fold_int32.comp
      | Naked_int64 -> Fold_int64.comp
      | Naked_nativeint -> Fold_nativeint.comp
    in
    comp ~machine_width op c1 c2
  | _ -> None
