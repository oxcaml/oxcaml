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
module Dll = Oxcaml_utils.Doubly_linked_list

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
    block.terminator <- { block.terminator with desc = Always l }
  | [(l0, n); (ln, k)] ->
    assert (Label.equal labels.(0) l0);
    assert (Label.equal labels.(n) ln);
    assert (len = n + k);
    let desc =
      C.Int_test
        { is_signed = Unsigned; imm = Some n; lt = l0; eq = ln; gt = ln }
    in
    block.terminator <- { block.terminator with desc }
  | [(l0, m); (l1, 1); (l2, _)] when Label.equal l0 l2 ->
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

(* Iterates over the passed instructions, and updates `known_values` so that it
   contains a map from registers to known values after the instructions have
   been executed. Currently only tracks constant values and moves between
   registers. *)
let collect_known_values (instrs : Cfg.basic_instruction_list) :
    known_value Reg.UsingLocEquality.Tbl.t =
  let known_values = Reg.UsingLocEquality.Tbl.create 17 in
  let replace reg value =
    if not (Reg.is_unknown reg)
    then Reg.UsingLocEquality.Tbl.replace known_values reg value
    else Misc.fatal_errorf "unexpected unknown location (%a)" Printreg.reg reg
  in
  let find_opt reg = Reg.UsingLocEquality.Tbl.find_opt known_values reg in
  let remove reg = Reg.UsingLocEquality.Tbl.remove known_values reg in
  Dll.iter instrs ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
      match instr.desc with
      | Op (Const_int c) -> replace instr.res.(0) (Const_int c)
      | Op (Const_float32 c) ->
        if !Oxcaml_flags.cfg_value_propagation_float
        then replace instr.res.(0) (Const_float32 c)
      | Op (Const_float c) ->
        if !Oxcaml_flags.cfg_value_propagation_float
        then replace instr.res.(0) (Const_float c)
      | Op Move -> (
        (* CR xclerc for xclerc: double check the "magic" / conversions behind
           moves in `Emit` will not result in invalid tracking here. *)
        match find_opt instr.arg.(0) with
        | Some value
          when Cmm.equal_machtype_component instr.res.(0).typ instr.arg.(0).typ
          ->
          replace instr.res.(0) value
        | Some _ | None -> remove instr.res.(0))
      | Op
          ( Spill | Reload | Const_symbol _ | Const_vec128 _ | Const_vec256 _
          | Const_vec512 _ | Stackoffset _ | Load _ | Store _ | Intop _
          | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _
          | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _ | Opaque
          | Begin_region | End_region | Specific _ | Name_for_debugger _
          | Dls_get | Poll | Pause | Alloc _ | Tls_get )
      | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Epilogue
      | Stack_check _ ->
        Array.iter
          (fun reg -> Reg.UsingLocEquality.Tbl.remove known_values reg)
          instr.res;
        let destroyed_regs = Proc.destroyed_at_basic instr.desc in
        Reg.UsingLocEquality.Tbl.filter_map_inplace
          (fun reg known_value ->
            let is_destroyed =
              Array.exists (fun r -> Reg.same_loc r reg) destroyed_regs
            in
            if is_destroyed then None else Some known_value)
          known_values);
  known_values

(* Compute the destination of a terminator, using [known_values] to determine
   the values of some registers, returning [None] if the destination is not
   statically known. *)
let evaluate_terminator (known_values : known_value Reg.UsingLocEquality.Tbl.t)
    (term : Cfg.terminator Cfg.instruction) : Label.t option =
  let[@inline] get_known_value ~(arg_idx : int) : known_value option =
    if arg_idx >= 0 && arg_idx < Array.length term.arg
    then
      Reg.UsingLocEquality.Tbl.find_opt known_values
        (Array.unsafe_get term.arg arg_idx)
    else
      Misc.fatal_errorf "invalid argument index (%d) for instruction %a" arg_idx
        InstructionId.format term.id
  in
  let[@inline] apply_constructor :
      type a b.
      known_value option ->
      extract:(known_value -> a option) ->
      f:(a -> b option) ->
      b option =
   fun value ~extract ~f ->
    let res = Option.map f (Option.bind value extract) in
    Option.join res
  in
  let[@inline] apply_constructors :
      type a b.
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
    let apply_float_constructors :
        type a.
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
  | Never -> assert false
  | Always _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
  | Call_no_return _ | Call _ | Prim _ ->
    None

let block_known_values (block : C.basic_block) ~(is_after_regalloc : bool) :
    bool =
  if !Oxcaml_flags.cfg_value_propagation && is_after_regalloc
  then (
    let known_values = collect_known_values block.body in
    match evaluate_terminator known_values block.terminator with
    | None -> false
    | Some succ ->
      block.terminator <- { block.terminator with desc = Always succ };
      true)
  else false

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
let block (cfg : C.t) (block : C.basic_block) : bool =
  let is_after_regalloc = cfg.register_locations_are_set in
  match block.terminator.desc with
  | Always successor_label ->
    (* If we have a jump to an empty block whose terminator is a condition, we
       can try and evaluate the condition at compile-time and short-circuit the
       empty block if we know the value(s) involved in the condition. *)
    let successor_block = C.get_block_exn cfg successor_label in
    if Dll.is_empty successor_block.body
    then
      (* CR-soon xclerc for xclerc: this logic is similar to the one of
         `block_known_values`, except for the guard and whether one or two
         blocks are involved. *)
      let new_successor =
        if is_after_regalloc
        then
          let known_values = collect_known_values block.body in
          evaluate_terminator known_values successor_block.terminator
        else None
      in
      match new_successor with
      | Some succ ->
        block.terminator <- { block.terminator with desc = Always succ };
        true
      | None -> (
        if Label.equal block.start cfg.entry_label
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
          | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ ->
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
      block.terminator <- { block.terminator with desc = Always l };
      false)
    else block_known_values block ~is_after_regalloc
  | Switch labels ->
    let shortcircuit = block_known_values block ~is_after_regalloc in
    if shortcircuit
    then true
    else (
      simplify_switch block labels;
      false)
  | Raise _ | Return | Tailcall_self _ | Tailcall_func _ | Call_no_return _
  | Call _ | Prim _ ->
    false

let run cfg =
  let registration_needed =
    C.fold_blocks cfg ~init:false ~f:(fun _ b registration_needed ->
        let shortcircuit = block cfg b in
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
