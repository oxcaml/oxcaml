(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module C = Cfg
module Dll = Doubly_linked_list
module Loc_map = Reg.UsingLocEquality.Map

(* Maximum immediate value used when a [Switch] is rewritten as an [Int_test]
   below. Selection normally guarantees, via `is_immediate`, that `Int_test`
   immediates are encodable on the target; this rewrite bypasses selection, so
   it conservatively stays within what all targets can encode (arm64's `cmp`
   immediate is limited to 0..4095, and `emit_cmpimm` does not split larger
   values). *)
let max_int_test_imm = 0xFFF

(* Convert simple [Switch] to branches. *)
let simplify_switch (block : C.basic_block) labels =
  let len = Array.length labels in
  if len < 1
  then Misc.fatal_error "Malformed terminator: switch with empty arms";
  (* Count continuous repeated occurrences of labels *)
  let labels_with_counts =
    Array.fold_right
      (fun l acc ->
        if List.compare_length_with acc 3 > 0
        then acc
        else
          match acc with
          | [] -> [l, 1]
          | (hd, n) :: tl ->
            if Label.equal hd l then (hd, n + 1) :: tl else (l, 1) :: acc)
      labels []
  in
  match labels_with_counts with
  | [(l, _)] ->
    (* All labels are the same and equal to l *)
    block.terminator
      <- { block.terminator with desc = Always l; arg = [||]; res = [||] }
  | [(l0, n); (ln, k)] when n <= max_int_test_imm ->
    assert (Label.equal labels.(0) l0);
    assert (Label.equal labels.(n) ln);
    assert (len = n + k);
    let desc =
      C.Int_test
        { is_signed = Unsigned; imm = Some n; lt = l0; eq = ln; gt = ln }
    in
    block.terminator <- { block.terminator with desc }
  | [(l0, m); (l1, 1); (l2, n)] when Label.equal l0 l2 && m <= max_int_test_imm
    ->
    assert (Label.equal labels.(0) l0);
    assert (Label.equal labels.(m) l1);
    assert (Label.equal labels.(m + 1) l2);
    assert (len = m + 1 + n);
    let desc =
      C.Int_test
        { is_signed = Unsigned; imm = Some m; lt = l0; eq = l1; gt = l0 }
    in
    block.terminator <- { block.terminator with desc }
  | [(l0, 1); (l1, 1); (l2, n)] ->
    assert (Label.equal labels.(0) l0);
    assert (Label.equal labels.(1) l1);
    assert (Label.equal labels.(2) l2);
    assert (len = n + 2);
    let desc =
      C.Int_test
        { is_signed = Unsigned; imm = Some 1; lt = l0; eq = l1; gt = l2 }
    in
    block.terminator <- { block.terminator with desc }
  | _ -> ()

(* CR-soon xclerc for xclerc: extend to other constants. *)
type known_value =
  | Const_int of nativeint
  | Const_float32 of int32
  | Const_float of int64

let equal_known_value (left : known_value) (right : known_value) : bool =
  match left, right with
  | Const_int left, Const_int right -> Nativeint.equal left right
  | Const_float32 left, Const_float32 right -> Int32.equal left right
  | Const_float left, Const_float right -> Int64.equal left right
  | Const_int _, (Const_float32 _ | Const_float _)
  | Const_float32 _, (Const_int _ | Const_float _)
  | Const_float _, (Const_int _ | Const_float32 _) ->
    false

(* Tells whether the materialization of a constant is expensive enough that
   replacing it with a register-to-register move is worthwhile. Constants that
   fit in 32 bits (signed) are materialized by a single cheap instruction on all
   supported targets (and rewriting the materialization of zero would defeat
   idioms such as `xor` on amd64); wider constants require more expensive
   sequences (e.g. `movabsq` on amd64, `movz`/`movk` chains on arm64). *)
let is_expensive_constant (c : nativeint) : bool =
  not (Nativeint.equal c (Nativeint.of_int32 (Nativeint.to_int32 c)))

let eval_int_op op (left : nativeint) (right : nativeint) : nativeint option =
  let is_valid_shift =
    Nativeint.compare right 0n >= 0
    && Nativeint.compare right (Nativeint.of_int Nativeint.size) < 0
  in
  match (op : Operation.integer_operation) with
  | Iadd -> Some (Nativeint.add left right)
  | Isub -> Some (Nativeint.sub left right)
  | Iand -> Some (Nativeint.logand left right)
  | Ior -> Some (Nativeint.logor left right)
  | Ixor -> Some (Nativeint.logxor left right)
  | Ilsl ->
    if is_valid_shift
    then Some (Nativeint.shift_left left (Nativeint.to_int right))
    else None
  | Ilsr ->
    if is_valid_shift
    then Some (Nativeint.shift_right_logical left (Nativeint.to_int right))
    else None
  | Iasr ->
    if is_valid_shift
    then Some (Nativeint.shift_right left (Nativeint.to_int right))
    else None
  (* CR xclerc for xclerc: some of the following operations could be supported
     in the future; care is needed as some may clobber registers beyond
     [res.(0)] on certain targets (e.g. [Imul] may not always lower to a form
     writing only to the destination). *)
  | Imul | Imulh _ | Idiv _ | Imod _ | Iclz | Ictz | Ipopcnt | Icomp _ -> None

let eval_float_op op (left : float) (right : float option) : float option =
  match (op : Operation.float_operation) with
  | Iaddf -> Option.map (Float.add left) right
  | Isubf -> Option.map (Float.sub left) right
  | Imulf -> Option.map (Float.mul left) right
  | Idivf -> Option.map (Float.div left) right
  | Inegf -> Some (Float.neg left)
  | Iabsf -> Some (Float.abs left)
  | Icompf _ -> None

(* CR-someday xclerc for xclerc: consider moving to `Misc`. *)
let find_unique_index : 'a array -> f:('a -> bool) -> int option =
 fun arr ~f ->
  let rec find arr idx f acc =
    if idx < 0
    then acc
    else
      begin if f (Array.unsafe_get arr idx)
      then
        begin match acc with
        | None -> find arr (idx - 1) f (Some idx)
        | Some _ -> None
        end
      else find arr (idx - 1) f acc
      end
  in
  find arr (Array.length arr - 1) f None

(* Removes from `known_values` the entries corresponding to the registers
   destroyed by the passed instruction. *)
let remove_destroyed_at_basic (known_values : known_value Loc_map.t)
    (instr : Cfg.basic Cfg.instruction) : known_value Loc_map.t =
  Array.fold_left
    (fun known_values reg -> Loc_map.remove reg known_values)
    known_values
    (Proc.destroyed_at_basic instr.desc)

(* Returns the statically-known result of the passed instruction, when it is a
   (pure) integer operation whose operands are statically known. *)
(* CR-someday xclerc for xclerc: some results are determined by a single known
   operand (e.g. `and` with 0, or `or` with -1); such cases are currently not
   folded, since the result is only computed when all the operands are
   known. *)
let known_int_op_result (known_values : known_value Loc_map.t)
    (instr : Cfg.basic Cfg.instruction) : nativeint option =
  let known_int (reg : Reg.t) : nativeint option =
    match Loc_map.find_opt reg known_values with
    | Some (Const_int v) -> Some v
    | Some (Const_float32 _ | Const_float _) | None -> None
  in
  begin[@ocaml.warning "-4"] match instr.desc with
  | Op (Intop_imm (op, imm)) ->
    begin match known_int instr.arg.(0) with
    | Some left -> eval_int_op op left (Nativeint.of_int imm)
    | None -> None
    end
  | Op (Intop op) ->
    if Operation.is_unary_integer_operation op
    then None
    else
      begin match known_int instr.arg.(0), known_int instr.arg.(1) with
      | Some left, Some right -> eval_int_op op left right
      | (Some _ | None), (Some _ | None) -> None
      end
  | _ -> None
  end

(* Returns the known values after the execution of the passed (basic)
   instruction, given the known values before it. Currently only tracks constant
   values, moves between registers, basic integer arithmetic, and basic float64
   arithmetic over known values. *)
let interpret_basic (known_values : known_value Loc_map.t)
    (instr : Cfg.basic Cfg.instruction) : known_value Loc_map.t =
  let replace (reg : Reg.t) (value : known_value)
      (known_values : known_value Loc_map.t) =
    match reg.loc with
    | Unknown ->
      Misc.fatal_errorf "unexpected unknown location (%a)" Printreg.reg reg
    | Stack (Domainstate _) ->
      (* The domain state is also read and written by the runtime system and by
         callees (it contains e.g. the extra-parameters area): do not track
         values written to it. *)
      Loc_map.remove reg known_values
    | Stack (Local _ | Incoming _ | Outgoing _) | Reg _ ->
      Loc_map.add reg value known_values
  in
  let apply_float_op op right_opt =
    let result_opt =
      match Loc_map.find_opt instr.arg.(0) known_values with
      | Some (Const_float left_bits) ->
        let left = Int64.float_of_bits left_bits in
        Option.map Int64.bits_of_float (eval_float_op op left right_opt)
      | Some (Const_int _ | Const_float32 _) | None -> None
    in
    let known_values =
      match result_opt with
      | Some bits -> replace instr.res.(0) (Const_float bits) known_values
      | None -> Loc_map.remove instr.res.(0) known_values
    in
    remove_destroyed_at_basic known_values instr
  in
  match instr.desc with
  | Op (Const_int c) -> replace instr.res.(0) (Const_int c) known_values
  | Op (Const_float32 c) ->
    if !Oxcaml_flags.cfg_value_propagation_float
    then replace instr.res.(0) (Const_float32 c) known_values
    else known_values
  | Op (Const_float c) ->
    if !Oxcaml_flags.cfg_value_propagation_float
    then replace instr.res.(0) (Const_float c) known_values
    else known_values
  | Op Move -> (
    (* The machtype guard below makes the tracking robust to the per-type
       encodings of moves in `Emit`: `Emit`'s move performs no conversions and
       rejects moves between differing types (except within the bit-preserving
       {Int, Val, Addr} and {Vec128, Valx2} groups, which the guard
       conservatively also rejects), and every same-component move faithfully
       copies exactly the bits the tracked value describes (e.g. a `Float32`
       move copies the 32-bit payload, which is all that `Const_float32`
       asserts). *)
    match Loc_map.find_opt instr.arg.(0) known_values with
    | Some value
      when Cmm.equal_machtype_component instr.res.(0).typ instr.arg.(0).typ ->
      replace instr.res.(0) value known_values
    | Some _ | None -> Loc_map.remove instr.res.(0) known_values)
  | Op (Intop_imm _ | Intop _) ->
    let known_values =
      match known_int_op_result known_values instr with
      | Some result -> replace instr.res.(0) (Const_int result) known_values
      | None -> Loc_map.remove instr.res.(0) known_values
    in
    remove_destroyed_at_basic known_values instr
  | Op (Floatop (Float64, op)) ->
    if !Oxcaml_flags.cfg_value_propagation_float
    then
      let right_opt =
        match (op : Operation.float_operation) with
        | Inegf | Iabsf -> None
        | Iaddf | Isubf | Imulf | Idivf | Icompf _ -> (
          match Loc_map.find_opt instr.arg.(1) known_values with
          | Some (Const_float bits) -> Some (Int64.float_of_bits bits)
          | Some (Const_int _ | Const_float32 _) | None -> None)
      in
      apply_float_op op right_opt
    else
      let known_values =
        Array.fold_left
          (fun known_values reg -> Loc_map.remove reg known_values)
          known_values instr.res
      in
      remove_destroyed_at_basic known_values instr
  | Op (Stackoffset _) | Pushtrap _ | Poptrap _ ->
    (* The stack pointer changes: the addresses of the outgoing slots move. *)
    let known_values =
      Loc_map.filter
        (fun (reg : Reg.t) (_ : known_value) ->
          match reg.loc with
          | Stack (Outgoing _) -> false
          | Stack (Local _ | Incoming _ | Domainstate _) | Reg _ | Unknown ->
            true)
        known_values
    in
    let known_values =
      Array.fold_left
        (fun known_values reg -> Loc_map.remove reg known_values)
        known_values instr.res
    in
    remove_destroyed_at_basic known_values instr
  | Op
      ( Spill | Reload | Const_symbol _ | Const_vec128 _ | Const_vec256 _
      | Const_vec512 _ | Const_mask _ | Load _ | Store _ | Int128op _
      | Intop_atomic _
      | Floatop (Float32, _)
      | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
      | Opaque | Begin_region | End_region | Specific _ | Name_for_debugger _
      | Dls_get | Poll | Pause | Alloc _ | Tls_get | Domain_index )
  | Reloadretaddr | Prologue | Epilogue | Stack_check _ ->
    let known_values =
      Array.fold_left
        (fun known_values reg -> Loc_map.remove reg known_values)
        known_values instr.res
    in
    remove_destroyed_at_basic known_values instr

(* Returns the known values after the execution of the passed terminator, along
   its normal successor edges, given the known values before it. Note that this
   function is only useful when values are propagated across blocks: the
   registers written or destroyed by the terminator (e.g. by a call) must then
   be forgotten. *)
let interpret_terminator (known_values : known_value Loc_map.t)
    (term : Cfg.terminator Cfg.instruction) : known_value Loc_map.t =
  let known_values =
    (* The callee is allowed to write to its incoming argument area, which is
       the caller's outgoing area, and both callees and the runtime system use
       the domain state: forget the values in the corresponding slots when
       control is transferred to other code. (The values in `Local` and
       `Incoming` slots cannot be written by other code.) *)
    match term.desc with
    | Call _ | Call_no_return _ | Prim _ | Invalid _ | Tailcall_self _
    | Tailcall_func _ ->
      Loc_map.filter
        (fun (reg : Reg.t) (_ : known_value) ->
          match reg.loc with
          | Stack (Outgoing _ | Domainstate _) -> false
          | Stack (Local _ | Incoming _) | Reg _ | Unknown -> true)
        known_values
    | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
    | Int_test _ | Switch _ | Return | Raise _ ->
      known_values
  in
  let known_values =
    Array.fold_left
      (fun known_values reg -> Loc_map.remove reg known_values)
      known_values term.res
  in
  Array.fold_left
    (fun known_values reg -> Loc_map.remove reg known_values)
    known_values
    (Proc.destroyed_at_terminator term.desc)

module Dataflow = struct
  module Domain = struct
    (* The analysis is a "must" analysis: a value is known at a program point
       only if it is known on all paths leading to that point. [join] is
       accordingly the intersection of the known-value maps (keeping only the
       bindings present on both sides with equal values), with [Unreachable] (no
       known path yet) as its identity. Ascending chains are bounded since a
       reachable state can only lose bindings. *)
    type t =
      | Unreachable
      | Reachable of known_value Loc_map.t

    let bot = Unreachable

    let join (left : t) (right : t) : t =
      match left, right with
      | Unreachable, other | other, Unreachable -> other
      | Reachable left, Reachable right ->
        Reachable
          (Loc_map.merge
             (fun _reg left right ->
               match left, right with
               | Some left, Some right when equal_known_value left right ->
                 Some left
               | (Some _ | None), (Some _ | None) -> None)
             left right)

    let less_equal (left : t) (right : t) : bool =
      (* `left <= right` iff `join left right = right`, i.e. iff every binding
         of `right` is also a binding of `left`. *)
      match left, right with
      | Unreachable, (Unreachable | Reachable _) -> true
      | Reachable _, Unreachable -> false
      | Reachable left, Reachable right ->
        Loc_map.for_all
          (fun reg right_value ->
            match Loc_map.find_opt reg left with
            | Some left_value -> equal_known_value left_value right_value
            | None -> false)
          right
  end

  module Transfer = struct
    type domain = Domain.t

    type context = unit

    type image =
      { normal : domain;
        exceptional : domain
      }

    let basic (domain : domain) (instr : Cfg.basic Cfg.instruction) () : domain
        =
      match domain with
      | Unreachable -> Domain.Unreachable
      | Reachable known_values ->
        Domain.Reachable (interpret_basic known_values instr)

    let terminator (domain : domain) (term : Cfg.terminator Cfg.instruction) ()
        : image =
      match domain with
      | Unreachable ->
        { normal = Domain.Unreachable; exceptional = Domain.Unreachable }
      | Reachable known_values ->
        (* No known values are propagated to exception handlers. *)
        { normal = Domain.Reachable (interpret_terminator known_values term);
          exceptional = Domain.Reachable Loc_map.empty
        }
  end

  include Cfg_dataflow.Forward (Domain) (Transfer)
end

(* Iterates over the passed instructions, and updates `known_values` so that it
   contains a map from registers to known values after the instructions have
   been executed, starting from the `init` values at the beginning of the block
   (`init` is only non-empty when values are propagated across blocks by the
   dataflow analysis above). Deletes moves deemed to be useless given the
   information in `known_values`, and rewrites the materialization of a constant
   into a register-to-register move when the constant is statically known to be
   already available in another register; integer operations whose result is
   statically known are similarly deleted or rewritten (into a materialization
   of the result, or a register-to-register move). *)
let collect_known_values (cfg : Cfg.t) (block : Cfg.basic_block)
    ~(init : known_value Loc_map.t) : known_value Loc_map.t =
  let known_values = ref init in
  let replace (reg : Reg.t) value =
    match reg.loc with
    | Unknown ->
      Misc.fatal_errorf "unexpected unknown location (%a)" Printreg.reg reg
    | Stack (Domainstate _) ->
      (* Consistently with `interpret_basic`, values in the domain state are not
         tracked (it is also read and written by the runtime system and by
         callees). *)
      known_values := Loc_map.remove reg !known_values
    | Stack (Local _ | Incoming _ | Outgoing _) | Reg _ ->
      known_values := Loc_map.add reg value !known_values
  in
  (* Deletes the instruction in [cell] if all it does is write [value] to [dst]
     while [dst] is already known to contain [value]; returns whether the
     instruction was deleted. Only integer constants are considered. Deleting
     the instruction makes the (unchanged) `live` fields an under-approximation
     of actual liveness: the destination register now carries its value from the
     previous write of the constant, across instructions whose `live` sets do
     not mention it. This is safe for the current consumers of `live`: the
     register is an integer register holding a compile-time integer constant, so
     omitting it from the frame descriptors of the GC points it now crosses is
     harmless (its value is never a heap pointer), and its liveness cannot
     change the decision to save SIMD registers at such points. Extending the
     deletion to other kinds of constants requires revisiting this reasoning. *)
  let delete_if_redundant (cell : Cfg.basic Cfg.instruction Dll.cell)
      (dst : Reg.t) (value : known_value) : bool =
    match value with
    | Const_int c ->
      begin match Loc_map.find_opt dst !known_values with
      | Some (Const_int c') when Nativeint.equal c c' ->
        Dll.delete_curr cell;
        true
      | Some (Const_int _ | Const_float32 _ | Const_float _) | None -> false
      end
    | Const_float32 _ | Const_float _ -> false
  in
  (* Looks for a hardware register, other than [dst] and with the same machtype
     component, statically known to hold the constant [c]; such a register can
     be used as the source of a move instead of materializing [c] into [dst].
     The linear scan of the map is acceptable because it only runs for the
     materializations of expensive constants, which are rare. *)
  (* CR-soon xclerc for xclerc: double check the complexity change (from the
     reverse-map lookup of the previous commit to a linear scan of the known
     values) is not problematic, e.g. on functions with many known values and
     many materializations of expensive constants. *)
  let find_move_source (c : nativeint) ~(dst : Reg.t) : Reg.t option =
    if is_expensive_constant c && Reg.is_reg dst
    then
      Loc_map.fold
        (fun (reg : Reg.t) (value : known_value) acc ->
          match acc, value with
          | Some _, (Const_int _ | Const_float32 _ | Const_float _) -> acc
          | None, Const_int c'
            when Nativeint.equal c c' && Reg.is_reg reg
                 && (not (Reg.same_loc reg dst))
                 && Cmm.equal_machtype_component reg.typ dst.typ ->
            Some reg
          | None, (Const_int _ | Const_float32 _ | Const_float _) -> None)
        !known_values None
    else None
  in
  let infer_known_values_from_predecessor () =
    (* When there is only one predecessor, we can sometimes infer the value of a
       temporary from the predecessor's terminator. For instance, if the
       terminator is a truth test and we are in the "ifnot" block, then we can
       infer the tested temporary is equal to zero at the start of the block. *)
    (* CR-someday xclerc for xclerc: that could be extended to multiple
    predecessors, if all lead to the same inference. *)
    (* A trap handler is entered through exceptional edges, on which the
       predecessor's terminator has not been executed: no fact can be inferred
       from that terminator. *)
    begin match Label.Set.cardinal block.predecessors with
    | 1 when not block.is_trap_handler ->
      let predecessor_block =
        Cfg.get_block_exn cfg (Label.Set.choose block.predecessors)
      in
      let predecessor_terminator = predecessor_block.terminator in
      (* The terminator may clobber registers after having read its arguments
         (e.g. `Switch` on amd64 uses rax and rdx as temporaries, and its
         argument may itself live in one of them): a fact about a register
         destroyed by the terminator does not hold at the start of the block. *)
      let replace_unless_destroyed (reg : Reg.t) (value : known_value) =
        let destroyed =
          Proc.destroyed_at_terminator predecessor_terminator.desc
        in
        if not (Array.exists (fun (r : Reg.t) -> Reg.same_loc r reg) destroyed)
        then replace reg value
      in
      begin[@ocaml.warning "-4"] match predecessor_terminator.desc with
      | Truth_test { ifso; ifnot } ->
        if Label.equal ifnot block.start && not (Label.equal ifso ifnot)
        then
          replace_unless_destroyed
            predecessor_block.terminator.arg.(0)
            (Const_int 0n)
      | Int_test { lt; eq; gt; is_signed = Signed; imm = Some const } ->
        if
          Label.equal eq block.start
          && (not (Label.equal eq gt))
          && not (Label.equal eq lt)
        then
          replace_unless_destroyed
            predecessor_terminator.arg.(0)
            (Const_int (Nativeint.of_int const))
      | Switch labels ->
        let idx =
          find_unique_index labels ~f:(fun label ->
              Label.equal block.start label)
        in
        begin match idx with
        | None -> ()
        | Some idx ->
          replace_unless_destroyed
            predecessor_terminator.arg.(0)
            (Const_int (Nativeint.of_int idx))
        end
      | _ -> ()
      end
    | _ -> ()
    end
  in
  if !Oxcaml_flags.cfg_value_propagation_flow
  then infer_known_values_from_predecessor ();
  Dll.iter_cell block.body
    ~f:(fun (cell : Cfg.basic Cfg.instruction Dll.cell) ->
      let instr = Dll.value cell in
      let deleted =
        begin[@ocaml.warning "-4"] match instr.desc with
        | Op (Const_int c) ->
          if delete_if_redundant cell instr.res.(0) (Const_int c)
          then true
          else begin
            begin match find_move_source c ~dst:instr.res.(0) with
            | Some src ->
              (* The constant is expensive to materialize but is already
                 available in `src`: rewrite the materialization into a
                 register-to-register move. As for the deletion performed by
                 `delete_if_redundant`, the (unchanged) `live` fields become an
                 under-approximation of actual liveness (`src` now carries its
                 value to this new use across instructions whose `live` sets may
                 not mention it), which is safe for the same reasons, `src`
                 being an integer register holding a compile-time integer
                 constant. *)
              Dll.set_value cell
                { instr with desc = Cfg.Op Move; arg = [| src |] }
            | None -> ()
            end;
            false
          end
        | Op Move ->
          begin match Loc_map.find_opt instr.arg.(0) !known_values with
          | Some value
            when Cmm.equal_machtype_component instr.res.(0).typ
                   instr.arg.(0).typ ->
            delete_if_redundant cell instr.res.(0) value
          | Some (Const_int _ | Const_float32 _ | Const_float _) | None -> false
          end
        | Op (Intop _ | Intop_imm _) ->
          begin match known_int_op_result !known_values instr with
          | Some result ->
            (* Only pure operations have their result computed statically, so
               the operation is subject to the same treatment as a
               materialization of its result: it is deleted when the destination
               register already holds the result (safe for the reasons given on
               `delete_if_redundant`); otherwise it is rewritten into a
               materialization of the result when the constant is not expensive,
               or into a register-to-register move when it is expensive but
               statically known to be available in another register (safe for
               the reasons given on the rewrite of constant materializations
               above) -- materializing an expensive constant in place of the
               operation could otherwise be a regression. Dropping the uses of
               the argument registers makes the (unchanged) `live` fields an
               over-approximation of actual liveness for these registers, which
               is always safe. *)
            if delete_if_redundant cell instr.res.(0) (Const_int result)
            then true
            else begin
              if not (is_expensive_constant result)
              then
                Dll.set_value cell
                  { instr with desc = Cfg.Op (Const_int result); arg = [||] }
              else
                begin match find_move_source result ~dst:instr.res.(0) with
                | Some src ->
                  Dll.set_value cell
                    { instr with desc = Cfg.Op Move; arg = [| src |] }
                | None -> ()
                end;
              false
            end
          | None -> false
          end
        | _ -> false
        end
      in
      if not deleted
      then known_values := interpret_basic !known_values (Dll.value cell));
  !known_values

(* Compute the destination of a terminator, using [known_values] to determine
   the values of some registers, returning [None] if the destination is not
   statically known. *)
let evaluate_terminator (known_values : known_value Loc_map.t)
    (term : Cfg.terminator Cfg.instruction) : Label.t option =
  let[@inline] get_known_value ~(arg_idx : int) : known_value option =
    if arg_idx >= 0 && arg_idx < Array.length term.arg
    then Loc_map.find_opt (Array.unsafe_get term.arg arg_idx) known_values
    else
      Misc.fatal_errorf "invalid argument index (%d) for instruction %a" arg_idx
        InstructionId.format term.id
  in
  let[@inline] apply_constructor : type a b.
      known_value option ->
      extract:(known_value -> a option) ->
      f:(a -> b option) ->
      b option =
   fun value ~extract ~f ->
    let res = Option.map f (Option.bind value extract) in
    Option.join res
  in
  let[@inline] apply_constructors : type a b.
      known_value option ->
      known_value option ->
      extract:(known_value -> a option) ->
      f:(a -> a -> b option) ->
      b option =
   fun left right ~extract ~f ->
    let left = Option.bind left extract in
    let right = Option.bind right extract in
    match left, right with
    | None, None | None, Some _ | Some _, None -> None
    | Some left, Some right -> f left right
  in
  let[@inline] const_int = function
    | Const_int const -> Some const
    | Const_float32 _ -> None
    | Const_float _ -> None
  in
  let[@inline] const_float32 = function
    | Const_int _ -> None
    | Const_float32 const -> Some const
    | Const_float _ -> None
  in
  let[@inline] const_float = function
    | Const_int _ -> None
    | Const_float32 _ -> None
    | Const_float const -> Some const
  in
  match term.desc with
  | Parity_test { ifso; ifnot } ->
    apply_constructor (get_known_value ~arg_idx:0) ~extract:const_int
      ~f:(fun const ->
        if Nativeint.equal (Nativeint.logand const 1n) 0n
        then Some ifso
        else Some ifnot)
  | Truth_test { ifso; ifnot } ->
    apply_constructor (get_known_value ~arg_idx:0) ~extract:const_int
      ~f:(fun const ->
        if not (Nativeint.equal const 0n) then Some ifso else Some ifnot)
  | Int_test { lt; eq; gt; is_signed; imm } ->
    let left_arg = get_known_value ~arg_idx:0 in
    let right_arg =
      match imm with
      | Some const -> Some (Const_int (Nativeint.of_int const))
      | None -> get_known_value ~arg_idx:1
    in
    apply_constructors left_arg right_arg ~extract:const_int
      ~f:(fun left_const right_const ->
        let result =
          match is_signed with
          | Signed -> Nativeint.compare left_const right_const
          | Unsigned -> Nativeint.unsigned_compare left_const right_const
        in
        if result < 0 then Some lt else if result > 0 then Some gt else Some eq)
  | Float_test { width; lt : Label.t; eq : Label.t; gt : Label.t; uo } -> (
    let apply_float_constructors : type a.
        known_value option ->
        known_value option ->
        extract:(known_value -> a option) ->
        convert:(a -> float) ->
        Label.t option =
     fun left right ~extract ~convert ->
      apply_constructors left right ~extract
        ~f:(fun (left_const : a) (right_const : a) ->
          let left_const = convert left_const in
          let right_const = convert right_const in
          if Float.is_nan left_const || Float.is_nan right_const
          then Some uo
          else
            let result = Float.compare left_const right_const in
            if result < 0
            then Some lt
            else if result > 0
            then Some gt
            else Some eq)
    in
    match width with
    | Float32 ->
      apply_float_constructors
        (get_known_value ~arg_idx:0)
        (get_known_value ~arg_idx:1)
        ~extract:const_float32 ~convert:Int32.float_of_bits
    | Float64 ->
      apply_float_constructors
        (get_known_value ~arg_idx:0)
        (get_known_value ~arg_idx:1)
        ~extract:const_float ~convert:Int64.float_of_bits)
  | Switch labels ->
    apply_constructor (get_known_value ~arg_idx:0) ~extract:const_int
      ~f:(fun const ->
        if Nativeint.compare const (Nativeint.of_int Int.max_int) <= 0
        then
          let idx = Nativeint.to_int const in
          if idx >= 0 && idx < Array.length labels
          then Some (Array.unsafe_get labels idx)
          else None
        else None)
  | Never ->
    Misc.fatal_error
      "Simplify_terminator.evaluate_terminator: unexpected Never terminator"
  | Always _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
  | Call_no_return _ | Call _ | Prim _ | Invalid _ ->
    None

let block_known_values (block : C.basic_block)
    ~(known_values : known_value Loc_map.t option)
    ~(allowed_to_be_irreducible : bool) : bool =
  match known_values with
  | Some known_values when allowed_to_be_irreducible -> (
    match evaluate_terminator known_values block.terminator with
    | None -> false
    | Some succ ->
      block.terminator
        <- { block.terminator with desc = Always succ; arg = [||]; res = [||] };
      true)
  | Some _ | None -> false

(* CR-someday gyorsh: merge (Lbranch | Lcondbranch | Lcondbranch3)+ into a
   single terminator when the argments are the same. Enables reordering of
   branch instructions and save cmp instructions. The main problem is that it
   involves boolean combination of conditionals of type Mach.test that can arise
   from a sequence of branches. When all conditions in the combination are
   integer comparisons, we can simplify them into a single condition, but it
   doesn't work for Ieventest and Ioddtest (which come from the primitive "is
   integer"). The advantage is that it will enable us to reorder branch
   instructions to avoid generating jmp to fallthrough location in the new
   order. Also, for linear to cfg and back will be harder to generate exactly
   the same layout. Also, how do we map execution counts about branches onto
   this terminator? *)
let block_with_initial_values (cfg : C.t) (block : C.basic_block)
    ~(init : known_value Loc_map.t) : bool =
  let is_after_regalloc = cfg.register_locations_are_set in
  (* Note: in addition to collecting the known values, the call to
     [collect_known_values] deletes the constant moves made redundant by the
     collected values; it is hence performed whatever the shape of the
     terminator is. *)
  let known_values =
    if !Oxcaml_flags.cfg_value_propagation && is_after_regalloc
    then Some (collect_known_values cfg block ~init)
    else None
  in
  match block.terminator.desc with
  | Always successor_label ->
    (* If we have a jump to an empty block whose terminator is a condition, we
       can try and evaluate the condition at compile-time and short-circuit the
       empty block if we know the value(s) involved in the condition. *)
    let successor_block = C.get_block_exn cfg successor_label in
    if Dll.is_empty successor_block.body
    then
      (* CR-soon xclerc for xclerc: this logic is similar to the one of
         `block_known_values`, except for whether one or two blocks are
         involved. *)
      let new_successor =
        (* The graph may become irreducible if the successor block is the header
           block of a loop. Indeed, if we shortcircuit that block, it means we
           are jumping "inside" the loop directly, which in turn means the loop
           is no longer natural. This is acceptable if we are past the last use
           of the loop information. *)
        match known_values with
        | Some known_values when cfg.allowed_to_be_irreducible ->
          evaluate_terminator known_values successor_block.terminator
        | Some _ | None -> None
      in
      match new_successor with
      | Some succ ->
        block.terminator
          <- { block.terminator with
               desc = Always succ;
               arg = [||];
               res = [||]
             };
        true
      | None -> (
        if
          Label.equal block.start cfg.entry_label
          || not cfg.allowed_to_be_irreducible
        then false
        else
          (* If we jump to a block that is empty, we can copy the terminator
             from the successor to the current block. There might be size
             considerations, so we currently do so only for "tests" and return.
             The optimization is disabled because of a CFG invariant expecting
             "the tailrec block to be the entry block or the only successor of
             the entry block". *)
          match successor_block.terminator.desc with
          | Parity_test _ | Truth_test _ | Int_test _ | Float_test _ | Return ->
            block.terminator
              <- { block.terminator with
                   desc = successor_block.terminator.desc;
                   arg = Array.copy successor_block.terminator.arg;
                   res = Array.copy successor_block.terminator.res;
                   dbg = successor_block.terminator.dbg
                 };
            true
          | Never | Always _ | Switch _ | Raise _ | Tailcall_self _
          | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ | Invalid _ ->
            false)
    else false
  | Never ->
    Misc.fatal_errorf "Cannot simplify terminator: Never (in block %a)"
      Label.format block.start
  | Parity_test _ | Truth_test _ | Int_test _ | Float_test _ ->
    let labels = C.successor_labels ~normal:true ~exn:false block in
    if Label.Set.cardinal labels = 1
    then (
      let l = Label.Set.min_elt labels in
      block.terminator
        <- { block.terminator with desc = Always l; arg = [||]; res = [||] };
      false)
    else
      block_known_values block ~known_values
        ~allowed_to_be_irreducible:cfg.allowed_to_be_irreducible
  | Switch labels ->
    let shortcircuit =
      block_known_values block ~known_values
        ~allowed_to_be_irreducible:cfg.allowed_to_be_irreducible
    in
    if shortcircuit
    then true
    else (
      simplify_switch block labels;
      false)
  | Raise _ | Return | Tailcall_self _ | Tailcall_func _ | Call_no_return _
  | Call _ | Prim _ | Invalid _ ->
    false

let block (cfg : C.t) (block : C.basic_block) : bool =
  block_with_initial_values cfg block ~init:Loc_map.empty

let run (cfg : C.t) =
  (* When enabled, the dataflow analysis computes the known values at the start
     of every block, instead of every block starting from an empty state. The
     transformations applied while iterating over the blocks below cannot
     invalidate the computed states: deleting a redundant constant move does not
     change the value held by any register at any point, and rewriting a
     terminator only removes control-flow edges, making the states conservative.
     Copying the terminator of an empty successor into a block adds edges, but
     is also covered: the copying block's end state is a superset of the empty
     successor's start state (the latter being an intersection over a set of
     paths that includes the former), and the transfer functions are monotone,
     so the states along the new edges are supersets of the states along the
     paths through the empty successor and the facts recorded at the new targets
     remain true. The remaining rewrites (of materializations or operations into
     constants or moves, and the deletions of redundant operations) keep the
     value written to the destination and only drop uses and clobbers, so the
     states likewise remain true. *)
  let dataflow_values =
    if
      !Oxcaml_flags.cfg_value_propagation
      && !Oxcaml_flags.cfg_value_propagation_dataflow
      && cfg.register_locations_are_set
    then
      match
        Dataflow.run cfg ~init:(Dataflow.Domain.Reachable Loc_map.empty)
          ~handlers_are_entry_points:true ()
      with
      | Result.Ok values -> Some values
      | Result.Error () ->
        Misc.fatal_error
          "Simplify_terminator.run: forward analysis did not reach a fix-point"
    else None
  in
  let registration_needed =
    C.fold_blocks cfg ~init:false ~f:(fun _ b registration_needed ->
        let init =
          match dataflow_values with
          | None -> Loc_map.empty
          | Some values -> (
            match Label.Tbl.find_opt values b.start with
            | Some (Dataflow.Domain.Reachable known_values) -> known_values
            | Some Dataflow.Domain.Unreachable | None -> Loc_map.empty)
        in
        let shortcircuit = block_with_initial_values cfg b ~init in
        registration_needed || shortcircuit)
  in
  if registration_needed
  then (
    (* We may need to remove predecessors, and
       `register_predecessors_for_all_blocks` is only adding predecessors, so we
       first set all to empty. *)
    C.iter_blocks cfg ~f:(fun _label block ->
        block.predecessors <- Label.Set.empty);
    Cfg.register_predecessors_for_all_blocks cfg)
