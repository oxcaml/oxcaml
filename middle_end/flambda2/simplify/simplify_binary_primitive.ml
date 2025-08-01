(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import
module A = Number_adjuncts

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

  val ok_to_evaluate : DE.t -> bool

  val cross_product : Lhs.Set.t -> Rhs.Set.t -> Pair.Set.t

  val arg_kind : K.Standard_int_or_float.t

  val result_kind : K.t

  val term : Result.t -> Named.t

  val prover_lhs : T.Typing_env.t -> T.t -> Lhs.Set.t T.meet_shortcut

  val prover_rhs : T.Typing_env.t -> T.t -> Rhs.Set.t T.meet_shortcut

  type op

  val unknown : op -> T.t

  val these : Result.Set.t -> T.t * Simple.t option

  val op : op -> Lhs.t -> Rhs.t -> Result.t option

  val op_lhs_unknown :
    op -> rhs:Rhs.t -> Result.t binary_arith_outcome_for_one_side_only

  val op_rhs_unknown :
    op -> lhs:Lhs.t -> Result.t binary_arith_outcome_for_one_side_only
end

module Binary_arith_like (N : Binary_arith_like_sig) : sig
  val simplify :
    N.op ->
    DA.t ->
    original_term:Named.t ->
    Debuginfo.t ->
    arg1:Simple.t ->
    arg1_ty:Flambda2_types.t ->
    arg2:Simple.t ->
    arg2_ty:Flambda2_types.t ->
    result_var:Bound_var.t ->
    SPR.t
end = struct
  module Possible_result = struct
    type t =
      | Simple of Simple.t
      | Prim of P.t
      | Exactly of N.Result.t

    (* This signature aims to constrain the size of the [Set] module block,
       since this is duplicated a lot via inlining in the rest of this file. *)
    module Set : sig
      type elt = t

      type t

      val empty : t

      val add : elt -> t -> t

      val is_empty : t -> bool

      val cardinal : t -> int

      val get_singleton : t -> elt option

      val elements : t -> elt list
    end = Container_types.Make_set [@inlined hint] (struct
      type nonrec t = t

      let compare t1 t2 =
        match t1, t2 with
        | Simple simple1, Simple simple2 -> Simple.compare simple1 simple2
        | Prim prim1, Prim prim2 -> P.compare prim1 prim2
        | Exactly i1, Exactly i2 -> N.Result.compare i1 i2
        | Simple _, (Prim _ | Exactly _) -> -1
        | Prim _, Simple _ -> 1
        | Prim _, Exactly _ -> -1
        | Exactly _, (Simple _ | Prim _) -> 1

      let print _ _ = Misc.fatal_error "Not implemented"
    end)
  end

  (* CR-someday mshinwell: Add command-line flag to control this *)
  let max_num_possible_results = 10

  let simplify op dacc ~original_term dbg ~arg1 ~arg1_ty ~arg2 ~arg2_ty
      ~result_var =
    let module PR = Possible_result in
    let denv = DA.denv dacc in
    let typing_env = DE.typing_env denv in
    let proof1 = N.prover_lhs typing_env arg1_ty in
    let proof2 = N.prover_rhs typing_env arg2_ty in
    let kind = N.result_kind in
    let[@inline always] result_unknown () =
      let dacc = DA.add_variable dacc result_var (N.unknown op) in
      SPR.create original_term ~try_reify:false dacc
    in
    let[@inline always] result_invalid () =
      let dacc = DA.add_variable dacc result_var (T.bottom kind) in
      SPR.create_invalid dacc
    in
    let check_possible_results ~possible_results =
      if PR.Set.is_empty possible_results
      then result_invalid ()
      else
        let named =
          match PR.Set.get_singleton possible_results with
          | Some (Exactly i) -> N.term i
          | Some (Prim prim) -> Named.create_prim prim dbg
          | Some (Simple simple) -> Named.create_simple simple
          | None -> original_term
        in
        let ty, simple_opt =
          let is =
            List.filter_map
              (fun (possible_result : PR.t) ->
                match possible_result with
                | Exactly i -> Some i
                | Prim _ | Simple _ -> None)
              (PR.Set.elements possible_results)
          in
          if List.length is = PR.Set.cardinal possible_results
          then N.these (N.Result.Set.of_list is)
          else
            match PR.Set.get_singleton possible_results with
            | Some (Simple simple) -> T.alias_type_of kind simple, Some simple
            | Some (Exactly _) | Some (Prim _) | None -> N.unknown op, None
        in
        let dacc = DA.add_variable dacc result_var ty in
        match simple_opt with
        | None -> SPR.create named ~try_reify:false dacc
        | Some simple ->
          let named = Named.create_simple simple in
          SPR.create named ~try_reify:false dacc
    in
    let only_one_side_known op nums ~folder ~other_side =
      let possible_results =
        folder
          (fun i possible_results ->
            match possible_results with
            | None -> None
            | Some possible_results -> (
              match op i with
              | Exactly result ->
                Some (PR.Set.add (Exactly result) possible_results)
              | The_other_side ->
                Some (PR.Set.add (Simple other_side) possible_results)
              | Negation_of_the_other_side ->
                let standard_int_kind : K.Standard_int.t =
                  match N.arg_kind with
                  | Tagged_immediate -> Tagged_immediate
                  | Naked_immediate -> Naked_immediate
                  | Naked_int8 -> Naked_int8
                  | Naked_int16 -> Naked_int16
                  | Naked_int32 -> Naked_int32
                  | Naked_int64 -> Naked_int64
                  | Naked_nativeint -> Naked_nativeint
                  | Naked_float | Naked_float32 ->
                    Misc.fatal_error
                      "Cannot use [Negation_of_the_other_side] with floats; \
                       use the float version instead"
                in
                let zero =
                  Simple.const_int_of_kind
                    (K.Standard_int.to_kind standard_int_kind)
                    0
                in
                let prim : P.t =
                  Binary (Int_arith (standard_int_kind, Sub), zero, other_side)
                in
                Some (PR.Set.add (Prim prim) possible_results)
              | Float_negation_of_the_other_side width ->
                let prim : P.t = Unary (Float_arith (width, Neg), other_side) in
                Some (PR.Set.add (Prim prim) possible_results)
              | Cannot_simplify -> None
              | Invalid -> Some possible_results))
          nums (Some PR.Set.empty)
      in
      match possible_results with
      | Some results -> check_possible_results ~possible_results:results
      | None -> result_unknown ()
    in
    match proof1, proof2 with
    | Known_result nums1, Known_result nums2 when N.ok_to_evaluate denv ->
      assert (not (N.Lhs.Set.is_empty nums1));
      assert (not (N.Rhs.Set.is_empty nums2));
      if N.Lhs.Set.cardinal nums1 > max_num_possible_results
         || N.Rhs.Set.cardinal nums2 > max_num_possible_results
      then result_unknown ()
      else
        let all_pairs = N.cross_product nums1 nums2 in
        let possible_results =
          N.Pair.Set.fold
            (fun (i1, i2) possible_results ->
              match N.op op i1 i2 with
              | None -> possible_results
              | Some result -> PR.Set.add (Exactly result) possible_results)
            all_pairs PR.Set.empty
        in
        check_possible_results ~possible_results
    | Known_result nums1, Need_meet when N.ok_to_evaluate denv ->
      assert (not (N.Lhs.Set.is_empty nums1));
      only_one_side_known
        (fun i -> N.op_rhs_unknown op ~lhs:i)
        nums1 ~folder:N.Lhs.Set.fold ~other_side:arg2
    | Need_meet, Known_result nums2 when N.ok_to_evaluate denv ->
      assert (not (N.Rhs.Set.is_empty nums2));
      only_one_side_known
        (fun i -> N.op_lhs_unknown op ~rhs:i)
        nums2 ~folder:N.Rhs.Set.fold ~other_side:arg1
    | (Known_result _ | Need_meet), (Known_result _ | Need_meet) ->
      result_unknown ()
    | Invalid, _ | _, Invalid -> result_invalid ()
end
[@@inline always]

module Int_ops_for_binary_arith (I : A.Int_number_kind) : sig
  include Binary_arith_like_sig with type op = P.binary_int_arith_op
end = struct
  module Lhs = I.Num
  module Rhs = I.Num
  module Result = I.Num

  type op = P.binary_int_arith_op

  (* There are never any restrictions on the constant propagation of integers,
     unlike for floats. *)
  let ok_to_evaluate _env = true

  let arg_kind = I.standard_int_or_float_kind

  let result_kind = K.Standard_int_or_float.to_kind arg_kind

  let prover_lhs = I.unboxed_prover

  let prover_rhs = I.unboxed_prover

  let unknown _ =
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

  let op (op : P.binary_int_arith_op) n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Add -> always_some I.Num.add
    | Sub -> always_some I.Num.sub
    | Mul -> always_some I.Num.mul
    | Div -> I.Num.div n1 n2
    | Mod -> I.Num.mod_ n1 n2
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

  let symmetric_op_one_side_unknown (op : symmetric_op) ~this_side :
      Num.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add ->
      if Num.equal this_side Num.zero then The_other_side else Cannot_simplify
    | Mul ->
      if Num.equal this_side Num.zero
      then Exactly Num.zero
      else if Num.equal this_side Num.one
      then The_other_side
      else if Num.equal this_side Num.minus_one
      then Negation_of_the_other_side
      else Cannot_simplify
    | And ->
      if Num.equal this_side Num.minus_one
      then The_other_side
      else if Num.equal this_side Num.zero
      then Exactly Num.zero
      else Cannot_simplify
    | Or ->
      if Num.equal this_side Num.minus_one
      then Exactly Num.minus_one
      else if Num.equal this_side Num.zero
      then The_other_side
      else Cannot_simplify
    | Xor ->
      if Num.equal this_side Num.zero then The_other_side else Cannot_simplify

  let op_lhs_unknown (op : P.binary_int_arith_op) ~rhs :
      Num.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:rhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:rhs
    | And -> symmetric_op_one_side_unknown And ~this_side:rhs
    | Or -> symmetric_op_one_side_unknown Or ~this_side:rhs
    | Xor -> symmetric_op_one_side_unknown Xor ~this_side:rhs
    | Sub -> if Num.equal rhs Num.zero then The_other_side else Cannot_simplify
    | Div ->
      (* Division ("safe" division, strictly speaking, in Lambda terminology) is
         translated to a conditional on the denominator followed by an unsafe
         division (the "Div" seen here) on the way into Flambda 2. So if the
         denominator turns out to be zero here, via the typing or whatever, then
         we're in unreachable code. *)
      (* CR-someday mshinwell: Should we expose unsafe division to the user? *)
      if Num.equal rhs Num.zero
      then Invalid
      else if Num.equal rhs Num.one
      then The_other_side
      else if Num.equal rhs Num.minus_one
      then
        Negation_of_the_other_side
        (* CR mshinwell: Add 0 / x = 0 when x <> 0 *)
      else Cannot_simplify
    | Mod ->
      (* CR mshinwell: We could be more clever for Mod and And *)
      if Num.equal rhs Num.zero
      then Invalid
      else if Num.equal rhs Num.one
      then Exactly Num.zero
      else if Num.equal rhs Num.minus_one
      then Exactly Num.zero
      else Cannot_simplify

  let op_rhs_unknown (op : P.binary_int_arith_op) ~lhs :
      Num.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:lhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:lhs
    | And -> symmetric_op_one_side_unknown And ~this_side:lhs
    | Or -> symmetric_op_one_side_unknown Or ~this_side:lhs
    | Xor -> symmetric_op_one_side_unknown Xor ~this_side:lhs
    | Sub ->
      if Num.equal lhs Num.zero
      then Negation_of_the_other_side
      else Cannot_simplify
    | Div | Mod -> Cannot_simplify
end
[@@inline always]

module Int_ops_for_binary_arith_tagged_immediate =
  Int_ops_for_binary_arith (A.For_tagged_immediates)
module Int_ops_for_binary_arith_naked_immediate =
  Int_ops_for_binary_arith (A.For_naked_immediates)
module Int_ops_for_binary_arith_int8 = Int_ops_for_binary_arith (A.For_int8s)
module Int_ops_for_binary_arith_int16 = Int_ops_for_binary_arith (A.For_int16s)
module Int_ops_for_binary_arith_int32 = Int_ops_for_binary_arith (A.For_int32s)
module Int_ops_for_binary_arith_int64 = Int_ops_for_binary_arith (A.For_int64s)
module Int_ops_for_binary_arith_nativeint =
  Int_ops_for_binary_arith (A.For_nativeints)
module Binary_int_arith_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_arith_tagged_immediate)
module Binary_int_arith_naked_immediate =
  Binary_arith_like (Int_ops_for_binary_arith_naked_immediate)
module Binary_int_arith_int8 = Binary_arith_like (Int_ops_for_binary_arith_int8)
module Binary_int_arith_int16 =
  Binary_arith_like (Int_ops_for_binary_arith_int16)
module Binary_int_arith_int32 =
  Binary_arith_like (Int_ops_for_binary_arith_int32)
module Binary_int_arith_int64 =
  Binary_arith_like (Int_ops_for_binary_arith_int64)
module Binary_int_arith_nativeint =
  Binary_arith_like (Int_ops_for_binary_arith_nativeint)

module Int_ops_for_binary_shift (I : A.Int_number_kind) : sig
  include Binary_arith_like_sig with type op = P.int_shift_op
end = struct
  module Lhs = I.Num
  module Rhs = Targetint_31_63
  module Result = I.Num

  type op = P.int_shift_op

  let arg_kind = I.standard_int_or_float_kind

  let result_kind = K.Standard_int_or_float.to_kind arg_kind

  let ok_to_evaluate _env = true

  let prover_lhs = I.unboxed_prover

  let prover_rhs = T.meet_naked_immediates

  let unknown _ =
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

  let op (op : P.int_shift_op) n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Lsl -> always_some Num.shift_left
    | Lsr -> always_some Num.shift_right_logical
    | Asr -> always_some Num.shift_right

  let op_lhs_unknown (op : P.int_shift_op) ~rhs :
      Num.t binary_arith_outcome_for_one_side_only =
    let module O = Targetint_31_63 in
    let rhs = rhs in
    match op with
    | Lsl | Lsr | Asr ->
      (* Shifting either way by [Targetint_32_64.size] or above, or by a
         negative amount, is undefined.

         However note that we cannot produce [Invalid] unless the code is type
         unsafe, which it is not here. (Otherwise a GADT match might be reduced
         to only one possible case which it would be wrong to take.) *)
      if O.equal rhs O.zero then The_other_side else Cannot_simplify

  let op_rhs_unknown (op : P.int_shift_op) ~lhs :
      Num.t binary_arith_outcome_for_one_side_only =
    (* In these cases we are giving a semantics for some cases where the
       right-hand side may be less than zero or greater than or equal to
       [Targetint_32_64.size]. These cases have undefined semantics, as above;
       however, it seems fine to give them a semantics since there is benefit to
       doing so in this particular case. (This is not the case for the situation
       in [op_lhs_unknown], above, where there would be no such benefit.) *)
    match op with
    | Lsl | Lsr ->
      if Num.equal lhs Num.zero then Exactly Num.zero else Cannot_simplify
    | Asr ->
      if Num.equal lhs Num.zero
      then Exactly Num.zero
      else if Num.equal lhs Num.minus_one
      then Exactly Num.minus_one
      else Cannot_simplify
end
[@@inline always]

module Int_ops_for_binary_shift_tagged_immediate =
  Int_ops_for_binary_shift (A.For_tagged_immediates)
module Int_ops_for_binary_shift_naked_immediate =
  Int_ops_for_binary_shift (A.For_naked_immediates)
module Int_ops_for_binary_shift_int8 = Int_ops_for_binary_shift (A.For_int8s)
module Int_ops_for_binary_shift_int16 = Int_ops_for_binary_shift (A.For_int16s)
module Int_ops_for_binary_shift_int32 = Int_ops_for_binary_shift (A.For_int32s)
module Int_ops_for_binary_shift_int64 = Int_ops_for_binary_shift (A.For_int64s)
module Int_ops_for_binary_shift_nativeint =
  Int_ops_for_binary_shift (A.For_nativeints)
module Binary_int_shift_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_shift_tagged_immediate)
module Binary_int_shift_naked_immediate =
  Binary_arith_like (Int_ops_for_binary_shift_naked_immediate)
module Binary_int_shift_int8 = Binary_arith_like (Int_ops_for_binary_shift_int8)
module Binary_int_shift_int16 =
  Binary_arith_like (Int_ops_for_binary_shift_int16)
module Binary_int_shift_int32 =
  Binary_arith_like (Int_ops_for_binary_shift_int32)
module Binary_int_shift_int64 =
  Binary_arith_like (Int_ops_for_binary_shift_int64)
module Binary_int_shift_nativeint =
  Binary_arith_like (Int_ops_for_binary_shift_nativeint)

module Int_ops_for_binary_comp (I : A.Int_number_kind) : sig
  include
    Binary_arith_like_sig
      with type op = P.signed_or_unsigned P.comparison_behaviour
end = struct
  module Lhs = I.Num
  module Rhs = I.Num
  module Result = Targetint_31_63

  type op = P.signed_or_unsigned P.comparison_behaviour

  let arg_kind = I.standard_int_or_float_kind

  let result_kind = K.naked_immediate

  let ok_to_evaluate _env = true

  let prover_lhs = I.unboxed_prover

  let prover_rhs = I.unboxed_prover

  let unknown (op : op) =
    match op with
    | Yielding_bool _ -> T.these_naked_immediates Targetint_31_63.all_bools
    | Yielding_int_like_compare_functions _signedness ->
      T.these_naked_immediates Targetint_31_63.zero_one_and_minus_one

  let these s =
    let ty = T.these_naked_immediates s in
    let simple_opt =
      match Targetint_31_63.Set.get_singleton s with
      | None -> None
      | Some i -> Some (Simple.const (Reg_width_const.naked_immediate i))
    in
    ty, simple_opt

  let term imm : Named.t =
    Named.create_simple (Simple.const (Reg_width_const.naked_immediate imm))

  module Pair = I.Num.Pair

  let cross_product = I.Num.cross_product

  module Num = I.Num

  let op (op : P.signed_or_unsigned P.comparison_behaviour) n1 n2 =
    match op with
    | Yielding_bool op -> (
      let bool b = Targetint_31_63.bool b in
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
        let int i = Targetint_31_63.of_int i in
        let c = Num.compare n1 n2 in
        if c < 0
        then Some (int (-1))
        else if c = 0
        then Some (int 0)
        else Some (int 1)
      | Unsigned ->
        let int i = Targetint_31_63.of_int i in
        let c = Num.compare_unsigned n1 n2 in
        if c < 0
        then Some (int (-1))
        else if c = 0
        then Some (int 0)
        else Some (int 1))

  let op_lhs_unknown _op ~rhs:_ = Cannot_simplify

  let op_rhs_unknown _op ~lhs:_ = Cannot_simplify
end
[@@inline always]

module Int_ops_for_binary_comp_tagged_immediate =
  Int_ops_for_binary_comp (A.For_tagged_immediates)
module Int_ops_for_binary_comp_naked_immediate =
  Int_ops_for_binary_comp (A.For_naked_immediates)
module Int_ops_for_binary_comp_int8 = Int_ops_for_binary_comp (A.For_int8s)
module Int_ops_for_binary_comp_int16 = Int_ops_for_binary_comp (A.For_int16s)
module Int_ops_for_binary_comp_int32 = Int_ops_for_binary_comp (A.For_int32s)
module Int_ops_for_binary_comp_int64 = Int_ops_for_binary_comp (A.For_int64s)
module Int_ops_for_binary_comp_nativeint =
  Int_ops_for_binary_comp (A.For_nativeints)
module Binary_int_comp_tagged_immediate =
  Binary_arith_like (Int_ops_for_binary_comp_tagged_immediate)
module Binary_int_comp_naked_immediate =
  Binary_arith_like (Int_ops_for_binary_comp_naked_immediate)
module Binary_int_comp_int8 = Binary_arith_like (Int_ops_for_binary_comp_int8)
module Binary_int_comp_int16 = Binary_arith_like (Int_ops_for_binary_comp_int16)
module Binary_int_comp_int32 = Binary_arith_like (Int_ops_for_binary_comp_int32)
module Binary_int_comp_int64 = Binary_arith_like (Int_ops_for_binary_comp_int64)
module Binary_int_comp_nativeint =
  Binary_arith_like (Int_ops_for_binary_comp_nativeint)

module Float_ops_for_binary_arith_gen (FP : sig
  module F : Numeric_types.Float_by_bit_pattern

  val width : Flambda_primitive.float_bitwidth

  val arg_kind : K.Standard_int_or_float.t

  val result_kind : Flambda_kind.t

  val prover : T.Typing_env.t -> T.t -> F.Set.t T.meet_shortcut

  val unknown : T.t

  val these : F.Set.t -> T.t

  val const : F.t -> Const.t
end) : sig
  include Binary_arith_like_sig with type op = P.binary_float_arith_op
end = struct
  module F = FP.F
  module Lhs = F
  module Rhs = F
  module Result = F

  type op = P.binary_float_arith_op

  let arg_kind = FP.arg_kind

  let result_kind = FP.result_kind

  let ok_to_evaluate denv = DE.propagating_float_consts denv

  let prover_lhs = FP.prover

  let prover_rhs = FP.prover

  let unknown _ = FP.unknown

  let these s =
    let ty = FP.these s in
    let simple_opt =
      match F.Set.get_singleton s with
      | None -> None
      | Some f -> Some (Simple.const (FP.const f))
    in
    ty, simple_opt

  let term f = Named.create_simple (Simple.const (FP.const f))

  module Pair = F.Pair

  let cross_product = F.cross_product

  let op (op : op) n1 n2 =
    let always_some f = Some (f n1 n2) in
    match op with
    | Add -> always_some F.IEEE_semantics.add
    | Sub -> always_some F.IEEE_semantics.sub
    | Mul -> always_some F.IEEE_semantics.mul
    | Div -> always_some F.IEEE_semantics.div

  type symmetric_op =
    | Add
    | Mul

  (* To be certain of correctness we restrict identities on floating-point
     numbers to those that preserve the _bit pattern_. *)

  let symmetric_op_one_side_unknown (op : symmetric_op) ~this_side :
      F.t binary_arith_outcome_for_one_side_only =
    match op with
    | Add ->
      (* You might think that "x + 0" has the same representation as "x".
         However it doesn't in the case where that constant zero is +0 and x is
         equal to -0. *)
      Cannot_simplify
    | Mul ->
      if F.equal this_side F.one
      then
        The_other_side
        [@z3 check_float_binary_neutral `Mul 1.0 `Right]
        [@z3 check_float_binary_neutral `Mul 1.0 `Left]
      else if F.equal this_side F.minus_one
      then
        Float_negation_of_the_other_side FP.width
        [@z3 check_float_binary_opposite `Mul (-1.0) `Left]
        [@z3 check_float_binary_opposite `Mul (-1.0) `Right]
      else Cannot_simplify

  let op_lhs_unknown (op : op) ~rhs : F.t binary_arith_outcome_for_one_side_only
      =
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:rhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:rhs
    | Sub -> Cannot_simplify
    | Div ->
      if F.equal rhs F.one
      then The_other_side [@z3 check_float_binary_neutral `Div 1.0 `Right]
      else if F.equal rhs F.minus_one
      then
        Float_negation_of_the_other_side FP.width
        [@z3 check_float_binary_opposite `Div (-1.0) `Right]
      else Cannot_simplify

  let op_rhs_unknown (op : op) ~lhs : F.t binary_arith_outcome_for_one_side_only
      =
    match op with
    | Add -> symmetric_op_one_side_unknown Add ~this_side:lhs
    | Mul -> symmetric_op_one_side_unknown Mul ~this_side:lhs
    | Sub -> Cannot_simplify
    | Div -> Cannot_simplify
end

module Float_ops_for_binary_arith = Float_ops_for_binary_arith_gen (struct
  module F = Numeric_types.Float_by_bit_pattern

  let width = Flambda_primitive.Float64

  let arg_kind = K.Standard_int_or_float.Naked_float

  let result_kind = K.naked_float

  let prover = T.meet_naked_floats

  let unknown = T.any_naked_float

  let these = T.these_naked_floats

  let const = Reg_width_const.naked_float
end)

module Float32_ops_for_binary_arith = Float_ops_for_binary_arith_gen (struct
  module F = Numeric_types.Float32_by_bit_pattern

  let width = Flambda_primitive.Float32

  let arg_kind = K.Standard_int_or_float.Naked_float32

  let result_kind = K.naked_float32

  let prover = T.meet_naked_float32s

  let unknown = T.any_naked_float32

  let these = T.these_naked_float32s

  let const = Reg_width_const.naked_float32
end)

module Binary_float_arith = Binary_arith_like (Float_ops_for_binary_arith)
module Binary_float32_arith = Binary_arith_like (Float32_ops_for_binary_arith)

module Float_ops_for_binary_comp_gen (FP : sig
  module F : Numeric_types.Float_by_bit_pattern

  val arg_kind : K.Standard_int_or_float.t

  val prover : T.Typing_env.t -> T.t -> F.Set.t T.meet_shortcut
end) : sig
  include Binary_arith_like_sig with type op = unit P.comparison_behaviour
end = struct
  module F = FP.F
  module Lhs = F
  module Rhs = F
  module Result = Targetint_31_63

  type op = unit P.comparison_behaviour

  let arg_kind = FP.arg_kind

  let result_kind = K.naked_immediate

  let ok_to_evaluate denv = DE.propagating_float_consts denv

  let prover_lhs = FP.prover

  let prover_rhs = FP.prover

  let unknown (op : op) =
    match op with
    | Yielding_bool _ -> T.these_naked_immediates Targetint_31_63.all_bools
    | Yielding_int_like_compare_functions () ->
      T.these_naked_immediates Targetint_31_63.zero_one_and_minus_one

  let these s =
    let ty = T.these_naked_immediates s in
    let simple_opt =
      match Targetint_31_63.Set.get_singleton s with
      | None -> None
      | Some i -> Some (Simple.const (Reg_width_const.naked_immediate i))
    in
    ty, simple_opt

  let term imm : Named.t =
    Named.create_simple (Simple.const (Reg_width_const.naked_immediate imm))

  module Pair = F.Pair

  let cross_product = F.cross_product

  let op (op : op) n1 n2 =
    match op with
    | Yielding_bool op -> (
      let has_nan = F.is_any_nan n1 || F.is_any_nan n2 in
      let bool b = Targetint_31_63.bool b in
      match op with
      | Eq -> Some (bool (F.IEEE_semantics.equal n1 n2))
      | Neq -> Some (bool (not (F.IEEE_semantics.equal n1 n2)))
      | Lt () ->
        if has_nan
        then Some (bool false)
        else Some (bool (F.IEEE_semantics.compare n1 n2 < 0))
      | Gt () ->
        if has_nan
        then Some (bool false)
        else Some (bool (F.IEEE_semantics.compare n1 n2 > 0))
      | Le () ->
        if has_nan
        then Some (bool false)
        else Some (bool (F.IEEE_semantics.compare n1 n2 <= 0))
      | Ge () ->
        if has_nan
        then Some (bool false)
        else Some (bool (F.IEEE_semantics.compare n1 n2 >= 0)))
    | Yielding_int_like_compare_functions () ->
      let int i = Targetint_31_63.of_int i in
      let c = F.IEEE_semantics.compare n1 n2 in
      if c < 0
      then Some (int (-1))
      else if c = 0
      then Some (int 0)
      else Some (int 1)

  let result_of_comparison_with_nan (op : unit P.comparison) =
    match op with
    | Neq -> Exactly Targetint_31_63.bool_true
    | Eq | Lt () | Gt () | Le () | Ge () -> Exactly Targetint_31_63.bool_false

  let op_lhs_unknown (op : op) ~rhs : _ binary_arith_outcome_for_one_side_only =
    match op with
    | Yielding_bool op ->
      if F.is_any_nan rhs
      then result_of_comparison_with_nan op
      else Cannot_simplify
    | Yielding_int_like_compare_functions () -> Cannot_simplify

  let op_rhs_unknown (op : op) ~lhs : _ binary_arith_outcome_for_one_side_only =
    match op with
    | Yielding_bool op ->
      if F.is_any_nan lhs
      then result_of_comparison_with_nan op
      else Cannot_simplify
    | Yielding_int_like_compare_functions () -> Cannot_simplify
end

module Float_ops_for_binary_comp = Float_ops_for_binary_comp_gen (struct
  module F = Numeric_types.Float_by_bit_pattern

  let arg_kind = K.Standard_int_or_float.Naked_float

  let prover = T.meet_naked_floats
end)

module Float32_ops_for_binary_comp = Float_ops_for_binary_comp_gen (struct
  module F = Numeric_types.Float32_by_bit_pattern

  let arg_kind = K.Standard_int_or_float.Naked_float32

  let prover = T.meet_naked_float32s
end)

module Binary_float_comp = Binary_arith_like (Float_ops_for_binary_comp)
module Binary_float32_comp = Binary_arith_like (Float32_ops_for_binary_comp)

let simplify_phys_equal (op : P.equality_comparison) dacc ~original_term _dbg
    ~arg1:_ ~arg1_ty ~arg2:_ ~arg2_ty ~result_var =
  (* This primitive is only used for arguments of kind [Value]. *)
  let typing_env = DA.typing_env dacc in
  (* Note: We don't compare the arguments themselves for equality. Instead, we
     know that [simplify_simple] always returns alias types, so we let the
     prover do the matching. *)
  match T.prove_physical_equality typing_env arg1_ty arg2_ty with
  | Proved bool ->
    let result = match op with Eq -> bool | Neq -> not bool in
    let dacc =
      DA.add_variable dacc result_var
        (T.this_naked_immediate (Targetint_31_63.bool result))
    in
    SPR.create
      (Named.create_simple (Simple.untagged_const_bool result))
      ~try_reify:false dacc
  | Unknown ->
    let dacc =
      DA.add_variable dacc result_var
        (T.these_naked_immediates Targetint_31_63.all_bools)
    in
    SPR.create original_term ~try_reify:false dacc

let simplify_array_load (array_kind : P.Array_kind.t)
    (array_load_kind : P.Array_load_kind.t) mutability dacc ~original_term:_ dbg
    ~arg1:array ~arg1_ty:array_ty ~arg2:index ~arg2_ty:index_ty ~result_var =
  let result_kind =
    match array_load_kind with
    | Immediates -> (* CR mshinwell: use the subkind *) K.value
    | Values -> K.value
    | Naked_floats -> K.naked_float
    | Naked_float32s -> K.naked_float32
    | Naked_int32s -> K.naked_int32
    | Naked_int64s -> K.naked_int64
    | Naked_nativeints -> K.naked_nativeint
    | Naked_vec128s -> K.naked_vec128
    | Naked_vec256s -> K.naked_vec256
    | Naked_vec512s -> K.naked_vec512
  in
  let array_kind =
    Simplify_common.specialise_array_kind dacc array_kind ~array_ty
  in
  (* CR-someday mshinwell: should do a meet on the new value too *)
  match array_kind with
  | Bottom ->
    let ty = T.bottom result_kind in
    let dacc = DA.add_variable dacc result_var ty in
    SPR.create_invalid dacc
  | Ok array_kind -> (
    let prim : P.t =
      Binary (Array_load (array_kind, array_load_kind, mutability), array, index)
    in
    let[@inline] return_given_type ty ~try_reify =
      let named = Named.create_prim prim dbg in
      let dacc = DA.add_variable dacc result_var ty in
      SPR.create named ~try_reify dacc
    in
    let[@inline] contents_unknown () =
      return_given_type (T.unknown (P.result_kind' prim)) ~try_reify:false
    in
    (* CR mshinwell/vlaviron: if immutable array accesses were consistently
       setting [mutability] to [Immutable], we could restrict the following code
       to immutable loads only and use [T.meet_is_immutable_array] instead. *)
    match T.prove_is_immutable_array (DA.typing_env dacc) array_ty with
    | Unknown -> contents_unknown ()
    | Proved (elt_kind, fields, _mode) -> (
      match elt_kind with
      | Unknown | Bottom -> contents_unknown ()
      | Ok elt_kind -> (
        if not (K.equal (K.With_subkind.kind elt_kind) result_kind)
        then contents_unknown ()
        else
          match
            T.prove_equals_tagged_immediates (DA.typing_env dacc) index_ty
          with
          | Unknown -> contents_unknown ()
          | Proved imms -> (
            match Targetint_31_63.Set.get_singleton imms with
            | None -> contents_unknown ()
            | Some imm ->
              if Targetint_31_63.( < ) imm Targetint_31_63.zero
                 || Targetint_31_63.( >= ) imm
                      (Array.length fields |> Targetint_31_63.of_int)
              then SPR.create_invalid dacc
              else
                return_given_type
                  fields.(Targetint_31_63.to_int imm)
                  ~try_reify:true))))

let simplify_string_or_bigstring_load _string_like_value _string_accessor_width
    ~original_prim dacc ~original_term _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_
    ~arg2_ty:_ ~result_var =
  (* CR mshinwell: This could evaluate loads from known strings. *)
  SPR.create_unknown dacc ~result_var
    (P.result_kind' original_prim)
    ~original_term

let simplify_bigarray_load _num_dimensions _bigarray_kind _bigarray_layout
    ~original_prim dacc ~original_term _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_
    ~arg2_ty:_ ~result_var =
  SPR.create_unknown dacc ~result_var
    (P.result_kind' original_prim)
    ~original_term

let simplify_bigarray_get_alignment _align ~original_prim dacc ~original_term
    _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~result_var =
  SPR.create_unknown dacc ~result_var
    (P.result_kind' original_prim)
    ~original_term

let simplify_atomic_load_field ~original_prim dacc ~original_term _dbg ~arg1:_
    ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~result_var =
  SPR.create_unknown dacc ~result_var
    (P.result_kind' original_prim)
    ~original_term

let simplify_block_set _block_access_kind _init_or_assign ~field:_ dacc
    ~original_term _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_ ~arg2_ty:_ ~result_var =
  SPR.create_unit dacc ~result_var ~original_term

let simplify_poke dacc ~original_term _dbg ~arg1:_ ~arg1_ty:_ ~arg2:_ ~arg2_ty:_
    ~result_var =
  SPR.create_unit dacc ~result_var ~original_term

let simplify_binary_primitive0 dacc original_prim (prim : P.binary_primitive)
    ~arg1 ~arg1_ty ~arg2 ~arg2_ty dbg ~result_var =
  let original_term = Named.create_prim original_prim dbg in
  let simplifier =
    match prim with
    | Block_set { kind; init; field } -> simplify_block_set kind init ~field
    | Array_load (array_kind, width, mutability) ->
      simplify_array_load array_kind width mutability
    | Int_arith (kind, op) -> (
      match kind with
      | Tagged_immediate -> Binary_int_arith_tagged_immediate.simplify op
      | Naked_immediate -> Binary_int_arith_naked_immediate.simplify op
      | Naked_int8 -> Binary_int_arith_int8.simplify op
      | Naked_int16 -> Binary_int_arith_int16.simplify op
      | Naked_int32 -> Binary_int_arith_int32.simplify op
      | Naked_int64 -> Binary_int_arith_int64.simplify op
      | Naked_nativeint -> Binary_int_arith_nativeint.simplify op)
    | Int_shift (kind, op) -> (
      match kind with
      | Tagged_immediate -> Binary_int_shift_tagged_immediate.simplify op
      | Naked_immediate -> Binary_int_shift_naked_immediate.simplify op
      | Naked_int8 -> Binary_int_shift_int8.simplify op
      | Naked_int16 -> Binary_int_shift_int16.simplify op
      | Naked_int32 -> Binary_int_shift_int32.simplify op
      | Naked_int64 -> Binary_int_shift_int64.simplify op
      | Naked_nativeint -> Binary_int_shift_nativeint.simplify op)
    | Int_comp (kind, op) -> (
      match kind with
      | Tagged_immediate -> Binary_int_comp_tagged_immediate.simplify op
      | Naked_immediate -> Binary_int_comp_naked_immediate.simplify op
      | Naked_int8 -> Binary_int_comp_int8.simplify op
      | Naked_int16 -> Binary_int_comp_int16.simplify op
      | Naked_int32 -> Binary_int_comp_int32.simplify op
      | Naked_int64 -> Binary_int_comp_int64.simplify op
      | Naked_nativeint -> Binary_int_comp_nativeint.simplify op)
    | Float_arith (Float64, op) -> Binary_float_arith.simplify op
    | Float_comp (Float64, op) -> Binary_float_comp.simplify op
    (* Note: despite the fact that all float32s are representable as float64s,
       float32 arithmetic operations need to be performed in 32-bit precision to
       preserve rounding behavior. Such 32-bit operations are implemented by
       flambda2_floats. *)
    | Float_arith (Float32, op) -> Binary_float32_arith.simplify op
    | Float_comp (Float32, op) -> Binary_float32_comp.simplify op
    | Phys_equal op -> simplify_phys_equal op
    | String_or_bigstring_load (string_like_value, string_accessor_width) ->
      simplify_string_or_bigstring_load string_like_value string_accessor_width
        ~original_prim
    | Bigarray_load (num_dimensions, bigarray_kind, bigarray_layout) ->
      simplify_bigarray_load num_dimensions bigarray_kind bigarray_layout
        ~original_prim
    | Bigarray_get_alignment align ->
      simplify_bigarray_get_alignment align ~original_prim
    | Atomic_load_field _ -> simplify_atomic_load_field ~original_prim
    | Poke _ -> simplify_poke
  in
  simplifier dacc ~original_term dbg ~arg1 ~arg1_ty ~arg2 ~arg2_ty ~result_var

let recover_comparison_primitive dacc (prim : P.binary_primitive) ~arg1 ~arg2 =
  match prim with
  | Block_set _ | Array_load _ | Int_arith _ | Int_shift _
  | Int_comp (_, Yielding_int_like_compare_functions _)
  | Float_arith _ | Float_comp _ | Phys_equal _ | String_or_bigstring_load _
  | Bigarray_load _ | Bigarray_get_alignment _ | Atomic_load_field _ | Poke _ ->
    None
  | Int_comp (kind, Yielding_bool op) -> (
    match kind with
    | Naked_immediate | Naked_int8 | Naked_int16 | Naked_int32 | Naked_int64
    | Naked_nativeint ->
      None
    | Tagged_immediate -> (
      let try_one_direction left right op =
        Simple.pattern_match right
          ~name:(fun _ ~coercion:_ -> None)
          ~const:(fun const ->
            match[@warning "-fragile-match"] Const.descr const with
            | Tagged_immediate i when Targetint_31_63.(equal i zero) ->
              Simple.pattern_match' left
                ~const:(fun _ -> None)
                ~symbol:(fun _ ~coercion:_ -> None)
                ~var:(fun var ~coercion:_ ->
                  match DE.find_comparison_result (DA.denv dacc) var with
                  | None -> None
                  | Some comp ->
                    Some
                      (Comparison_result.convert_result_compared_to_tagged_zero
                         comp op))
            | _ -> None)
      in
      match try_one_direction arg1 arg2 op with
      | Some p -> Some p
      | None ->
        let op : _ P.comparison =
          match op with
          | Eq -> Eq
          | Neq -> Neq
          (* Note that this is not handling a negation of an inequality, it is
             simply a pattern match for when the inequality appears the other
             way around. So e.g. [Lt] maps to [Gt], not [Ge]. *)
          | Lt s -> Gt s
          | Gt s -> Lt s
          | Le s -> Ge s
          | Ge s -> Le s
        in
        try_one_direction arg2 arg1 op))

let simplify_binary_primitive dacc original_prim (prim : P.binary_primitive)
    ~arg1 ~arg1_ty ~arg2 ~arg2_ty dbg ~result_var =
  let original_prim, prim, arg1, arg1_ty, arg2, arg2_ty =
    match[@warning "-fragile-match"]
      recover_comparison_primitive dacc prim ~arg1 ~arg2
    with
    | None -> original_prim, prim, arg1, arg1_ty, arg2, arg2_ty
    | Some (Binary (new_prim, new_arg1, new_arg2) as new_original_prim) -> (
      let min_name_mode = Bound_var.name_mode result_var in
      let arg1_ty_opt =
        S.simplify_simple_if_in_scope dacc new_arg1 ~min_name_mode
      in
      let arg2_ty_opt =
        S.simplify_simple_if_in_scope dacc new_arg2 ~min_name_mode
      in
      match arg1_ty_opt, arg2_ty_opt with
      | Some new_arg1_ty, Some new_arg2_ty ->
        ( new_original_prim,
          new_prim,
          new_arg1,
          new_arg1_ty,
          new_arg2,
          new_arg2_ty )
      | None, _ | _, None -> original_prim, prim, arg1, arg1_ty, arg2, arg2_ty)
    | Some other_prim ->
      Misc.fatal_errorf "Recovered primitive %a is not a comparison"
        Flambda_primitive.print other_prim
  in
  simplify_binary_primitive0 dacc original_prim prim ~arg1 ~arg1_ty ~arg2
    ~arg2_ty dbg ~result_var
