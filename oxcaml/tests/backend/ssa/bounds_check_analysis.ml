open Ssa.Export

let function_info : Ssa.Function_info.t =
  { sym_name = "bounds_check_analysis";
    parameters = [];
    codegen_options = [];
    dbg = Debuginfo.none;
    poll = Lambda.Default_poll;
    ret_type = Cmm.typ_int
  }

let block graph params =
  Block.create graph ~params ~cold:false ~handler_dbg:Debuginfo.none

let emit graph cursor op args =
  (Cursor.emit_op graph cursor op Debuginfo.none Cmm.typ_int args).(0)

let finish graph cursor term =
  Cursor.finish_block graph cursor ~dbg:Debuginfo.none term

let goto target args : _ Terminator.t =
  Continue
    { continuation = Goto target;
      args = Array.map (fun v -> Terminator.Arg v) args
    }

let return_const graph block n =
  let cursor = Cursor.start block in
  let v = emit graph cursor (Const_int n) [||] in
  finish graph cursor (Continue { continuation = Return; args = [| Arg v |] })

(* An unconstrained register value. These graphs are analyzed, not executed; the
   load address is immaterial. *)
let load : Operation.t =
  Load
    { memory_chunk = Cmm.Word_int;
      addressing_mode = Arch.identity_addressing;
      mutability = Mutable;
      is_atomic = false
    }

let check_eliminations graph expected =
  let _, eliminated =
    Bounds_check_elim.run ~ppf_dump:Format.err_formatter graph
  in
  assert (eliminated = expected)

(* A comparison of the index with the limit, or with a literal. *)
type comparison =
  | Index_vs_limit of Cmm.integer_comparison
  | Limit_vs_index of Cmm.integer_comparison
  | Index_vs of Cmm.integer_comparison * int

(* A branch dominating the check, and the edge by which the check is reached. *)
type guard =
  | Taken of comparison
  | Not_taken of comparison

(* The check [index check limit] (or [limit check index] when [exchanged]) in a
   block reached only through the [guards], in order. The index and the limit
   are loads, so the only facts about them are the guards'. A guard with
   [duplicate_targets] branches to the same block either way and conveys
   nothing. *)
let guarded_check ?(check = Cmm.Cult) ?(exchanged = false)
    ?(duplicate_targets = false) guards =
  let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
  let fail = block graph [||] in
  let ok = block graph [||] in
  let entry = Cursor.start (Ssa.entry graph) in
  let zero = emit graph entry (Const_int 0n) [||] in
  let index = emit graph entry load [| zero |] in
  let limit = emit graph entry load [| zero |] in
  let compare cursor = function
    | Index_vs_limit cmp ->
      emit graph cursor (Intop (Icomp cmp)) [| index; limit |]
    | Limit_vs_index cmp ->
      emit graph cursor (Intop (Icomp cmp)) [| limit; index |]
    | Index_vs (cmp, k) ->
      emit graph cursor (Intop_imm (Icomp cmp, k)) [| index |]
  in
  let cursor =
    List.fold_left
      (fun cursor guard ->
        let next = block graph [||] in
        let cond, targets =
          match guard with
          | Taken comparison -> compare cursor comparison, [| fail; next |]
          | Not_taken comparison -> compare cursor comparison, [| next; fail |]
        in
        let targets = if duplicate_targets then [| next; next |] else targets in
        finish graph cursor (Switch { index = cond; targets });
        Cursor.start next)
      entry guards
  in
  let args = if exchanged then [| limit; index |] else [| index; limit |] in
  let cond = emit graph cursor (Intop (Icomp check)) args in
  finish graph cursor (Switch { index = cond; targets = [| fail; ok |] });
  return_const graph ok 42n;
  return_const graph fail 0n;
  Ssa.finish_graph graph

(* The limit a counted loop runs to: a literal, an unconstrained register value,
   or an array length as read from a header word, whose range the linearizer
   knows. *)
type limit =
  | Literal of int
  | Register
  | Length

(* The loop [header -> body -> latch -> header], counting from the literal
   [start] by [step] while [i <s limit], with the check [i <u limit] in its
   body. The counter's lower bound is certified from the entry literal and the
   step; the header's test supplies the upper bound. *)
let counted_check ~start ~step limit =
  let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
  let header = block graph Cmm.typ_int in
  let body = block graph [||] in
  let latch = block graph [||] in
  let exit = block graph [||] in
  let fail = block graph [||] in
  let entry = Cursor.start (Ssa.entry graph) in
  let zero = emit graph entry (Const_int 0n) [||] in
  let start = emit graph entry (Const_int (Nativeint.of_int start)) [||] in
  let compare =
    let against limit cursor cmp i =
      emit graph cursor (Intop (Icomp cmp)) [| i; limit |]
    in
    match limit with
    | Literal k ->
      fun cursor cmp i -> emit graph cursor (Intop_imm (Icomp cmp, k)) [| i |]
    | Register -> against (emit graph entry load [| zero |])
    | Length ->
      let word = emit graph entry load [| zero |] in
      let shifted = emit graph entry (Intop_imm (Ilsl, 8)) [| word |] in
      against (emit graph entry (Intop_imm (Ilsr, 17)) [| shifted |])
  in
  finish graph entry (goto header [| start |]);
  let cursor = Cursor.start header in
  let i = Block.param header 0 in
  let cond = compare cursor Cmm.Clt i in
  finish graph cursor (Switch { index = cond; targets = [| exit; body |] });
  let cursor = Cursor.start body in
  let cond = compare cursor Cmm.Cult i in
  finish graph cursor (Switch { index = cond; targets = [| fail; latch |] });
  let cursor = Cursor.start latch in
  let next = emit graph cursor (Intop_imm (Iadd, step)) [| i |] in
  finish graph cursor (goto header [| next |]);
  return_const graph exit 42n;
  return_const graph fail 0n;
  Ssa.finish_graph graph

(* The loop a [for] compiles to: the check is in the header, and the exit test,
   in the body, is on the next value of the counter, [i + 1], against the stop
   value [limit - 1] computed before the loop, which the entry tests to be at
   least zero. The counter starts at the literal [start]. The stop value, with
   which the exit test compares an update of the counter, is certified as the
   counter's upper bound: every predecessor of the header passes at most it.
   That bound is what shows the update not to wrap. With [opaque_stop], the exit
   test compares against a register value loaded in the header instead, which
   bounds nothing. With [tagged], the check is on the counter's tagging, [2 i +
   1], against the tagged limit, as the compiler emits it (the tagging is then a
   target-specific scaled addition, which the unit test cannot build). *)
let for_check ?(opaque_stop = false) ?(tagged = false) ~start limit =
  let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
  let preheader = block graph [||] in
  let header = block graph Cmm.typ_int in
  let body = block graph [||] in
  let latch = block graph [||] in
  let exit = block graph [||] in
  let fail = block graph [||] in
  let entry = Cursor.start (Ssa.entry graph) in
  let zero = emit graph entry (Const_int 0n) [||] in
  let len =
    match limit with
    | Literal k -> emit graph entry (Const_int (Nativeint.of_int k)) [||]
    | Register -> emit graph entry load [| zero |]
    | Length ->
      let word = emit graph entry load [| zero |] in
      let shifted = emit graph entry (Intop_imm (Ilsl, 8)) [| word |] in
      emit graph entry (Intop_imm (Ilsr, 17)) [| shifted |]
  in
  let stop = emit graph entry (Intop_imm (Iadd, -1)) [| len |] in
  let enter = emit graph entry (Intop_imm (Icomp Cge, 0)) [| stop |] in
  finish graph entry (Switch { index = enter; targets = [| exit; preheader |] });
  let cursor = Cursor.start preheader in
  let start = emit graph cursor (Const_int (Nativeint.of_int start)) [||] in
  finish graph cursor (goto header [| start |]);
  let cursor = Cursor.start header in
  let i = Block.param header 0 in
  let stop = if opaque_stop then emit graph cursor load [| zero |] else stop in
  let tag v =
    let doubled = emit graph cursor (Intop_imm (Ilsl, 1)) [| v |] in
    emit graph cursor (Intop_imm (Iadd, 1)) [| doubled |]
  in
  let index, limit = if tagged then tag i, tag len else i, len in
  let cond = emit graph cursor (Intop (Icomp Cult)) [| index; limit |] in
  finish graph cursor (Switch { index = cond; targets = [| fail; body |] });
  let cursor = Cursor.start body in
  let next = emit graph cursor (Intop_imm (Iadd, 1)) [| i |] in
  let continue = emit graph cursor (Intop (Icomp Cle)) [| next; stop |] in
  finish graph cursor (Switch { index = continue; targets = [| exit; latch |] });
  let cursor = Cursor.start latch in
  finish graph cursor (goto header [| next |]);
  return_const graph exit 42n;
  return_const graph fail 0n;
  Ssa.finish_graph graph

let () =
  check_eliminations (guarded_check []) 0;
  (* The signed tests the compiler emits for [0 <= i && i < len]: both are
     needed. *)
  check_eliminations
    (guarded_check
       [Taken (Index_vs (Cmm.Cge, 0)); Taken (Index_vs_limit Cmm.Clt)])
    1;
  check_eliminations (guarded_check [Taken (Index_vs_limit Cmm.Clt)]) 0;
  check_eliminations (guarded_check [Taken (Index_vs (Cmm.Cge, 0))]) 0;
  check_eliminations
    (guarded_check
       [Taken (Index_vs (Cmm.Cgt, -1)); Taken (Limit_vs_index Cmm.Cgt)])
    1;
  (* The same facts from the false edges of the opposite tests. *)
  check_eliminations
    (guarded_check
       [Not_taken (Index_vs (Cmm.Clt, 0)); Not_taken (Limit_vs_index Cmm.Cle)])
    1;
  check_eliminations
    (guarded_check
       [Taken (Index_vs (Cmm.Cge, 0)); Taken (Limit_vs_index Cmm.Cle)])
    0;
  (* An unsigned comparison yields no affine fact, so an earlier identical check
     does not discharge a later one. Known limitation. *)
  check_eliminations (guarded_check [Taken (Index_vs_limit Cmm.Cult)]) 0;
  (* The other comparisons a check may use. *)
  check_eliminations
    (guarded_check ~check:Cmm.Cule
       [Taken (Index_vs (Cmm.Cge, 0)); Taken (Index_vs_limit Cmm.Cle)])
    1;
  check_eliminations
    (guarded_check ~check:Cmm.Cugt ~exchanged:true
       [Taken (Index_vs (Cmm.Cge, 0)); Taken (Index_vs_limit Cmm.Clt)])
    1;
  check_eliminations
    (guarded_check ~check:Cmm.Cugt
       [Taken (Index_vs (Cmm.Cge, 0)); Taken (Index_vs_limit Cmm.Clt)])
    0;
  check_eliminations
    (guarded_check ~duplicate_targets:true
       [Taken (Index_vs (Cmm.Cge, 0)); Taken (Index_vs_limit Cmm.Clt)])
    0;
  (* Loop counters: the certified lower bound replaces the [0 <= i] guard. It
     needs a literal entry value at least zero and a positive literal step. *)
  check_eliminations (counted_check ~start:0 ~step:1 (Literal 10)) 1;
  check_eliminations (counted_check ~start:3 ~step:2 (Literal 10)) 1;
  check_eliminations (counted_check ~start:(-1) ~step:1 (Literal 10)) 0;
  check_eliminations (counted_check ~start:0 ~step:(-1) (Literal 10)) 0;
  check_eliminations (counted_check ~start:0 ~step:0 (Literal 10)) 0;
  (* The update must be known not to wrap: the header's test bounds the counter
     by the limit, whose range must then be known. *)
  check_eliminations (counted_check ~start:0 ~step:1 Length) 1;
  check_eliminations (counted_check ~start:0 ~step:1 Register) 0;
  (* A [for] loop: the exit test is on the counter's update, so the bound it
     gives is certified as the counter's upper bound. The literal entry value
     must be within it, and its range must be known. *)
  check_eliminations (for_check ~start:0 Length) 1;
  check_eliminations (for_check ~tagged:true ~start:0 Length) 1;
  check_eliminations (for_check ~start:0 (Literal 10)) 1;
  check_eliminations (for_check ~start:11 (Literal 10)) 0;
  check_eliminations (for_check ~start:0 Register) 0;
  check_eliminations (for_check ~opaque_stop:true ~start:0 Length) 0;
  print_endline "OK"
