open Ssa.Export

let function_info : Ssa.Function_info.t =
  { sym_name = "loop_analysis";
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

type instruction_location =
  | Before_loop
  | Header
  | Latch

type body_shape =
  | Straight
  | Diamond
  | Inner_cycle
  | Irreducible_cycle

type prefix_shape =
  | Plain_prefix
  | Opaque_call
  | Trap_handler

(* The outer counter always advances towards ten and is dead. Only the control
   flow between header visits distinguishes these cases. The irreducible cycle
   has two entries, so rejecting nested natural loops alone would not
   suffice. *)
let loop ?extra ?(extra_type = Cmm.typ_int) ?(prefix = Plain_prefix) shape =
  let graph =
    Ssa.create_graph function_info ~keep_unused_ops:(Option.is_some extra)
  in
  let header = block graph Cmm.typ_int in
  let body = block graph [||] in
  let latch = block graph [||] in
  let exit = block graph [||] in
  let entry = Cursor.start (Ssa.entry graph) in
  let zero = emit graph entry (Const_int 0n) [||] in
  let flag = emit graph entry (Const_int 1n) [||] in
  let emit_extra location cursor =
    match extra with
    | Some (where, op) when where = location ->
      let (_ : _ Value.t array) =
        Cursor.emit_op graph cursor op Debuginfo.none extra_type [| flag |]
      in
      ()
    | Some _ | None -> ()
  in
  emit_extra Before_loop entry;
  let entry =
    match prefix with
    | Plain_prefix -> entry
    | Opaque_call | Trap_handler ->
      let returned = block graph [||] in
      let handler =
        match prefix with
        | Trap_handler -> Some (block graph Cmm.typ_int)
        | Plain_prefix | Opaque_call -> None
      in
      Option.iter (fun handler -> Cursor.emit_push_trap entry ~handler) handler;
      finish graph entry
        (Call
           { op = Direct (Cmm.global_symbol "prefix_call");
             args = [||];
             continuation = Goto returned;
             may_raise = true;
             nontail = true
           });
      let cursor = Cursor.start returned in
      Option.iter
        (fun handler ->
          Cursor.emit_pop_trap cursor ~handler;
          finish graph (Cursor.start handler)
            (Continue
               { continuation = Return;
                 args = [| Arg (Block.param handler 0) |]
               }))
        handler;
      cursor
  in
  finish graph entry (goto header [| zero |]);
  let cursor = Cursor.start header in
  emit_extra Header cursor;
  let i = Block.param header 0 in
  let cond = emit graph cursor (Intop_imm (Icomp Cmm.Clt, 10)) [| i |] in
  finish graph cursor (Switch { index = cond; targets = [| exit; body |] });
  let cursor = Cursor.start body in
  let branches ~cycle =
    let left = block graph [||] in
    let right = block graph [||] in
    finish graph cursor (Switch { index = flag; targets = [| left; right |] });
    let cursor = Cursor.start left in
    if cycle
    then
      finish graph cursor
        (Switch { index = flag; targets = [| latch; right |] })
    else finish graph cursor (goto latch [||]);
    finish graph (Cursor.start right)
      (goto (if cycle then left else latch) [||])
  in
  (match shape with
  | Straight -> finish graph cursor (goto latch [||])
  | Inner_cycle ->
    finish graph cursor (Switch { index = flag; targets = [| latch; body |] })
  | Diamond -> branches ~cycle:false
  | Irreducible_cycle -> branches ~cycle:true);
  let cursor = Cursor.start latch in
  emit_extra Latch cursor;
  let next = emit graph cursor (Intop_imm (Iadd, 1)) [| i |] in
  finish graph cursor (goto header [| next |]);
  let cursor = Cursor.start exit in
  let result = emit graph cursor (Const_int 42n) [||] in
  finish graph cursor
    (Continue { continuation = Return; args = [| Arg result |] });
  Ssa.finish_graph graph

let check_deletions graph expected =
  let _, deleted =
    Delete_empty_loops.run ~ppf_dump:Format.err_formatter graph
  in
  assert (deleted = expected)

let check_loop shape expected = check_deletions (loop shape) expected

(* Keep unused instructions so these checks reach the loop whitelist, rather
   than relying on an earlier dead-code pass to remove the operation. These
   graphs are analyzed, not executed; the load address is immaterial. *)
let check_extra ?extra_type location operation expected =
  check_deletions
    (loop ~extra:(location, operation) ?extra_type Straight)
    expected

let load ~is_atomic : Operation.t =
  Load
    { memory_chunk = Cmm.Word_int;
      addressing_mode = Arch.identity_addressing;
      mutability = Mutable;
      is_atomic
    }

(* A load supplies an unconstrained register bound. Steps of magnitude two
   require the affine range check, unlike the immediate-bound/unit-step tests. A
   duplicate-target guard conveys neither boolean outcome. *)
let register_bound_loop ~step ~guard ~duplicate_targets =
  let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
  let preheader = block graph [||] in
  let header = block graph Cmm.typ_int in
  let latch = block graph [||] in
  let exit = block graph [||] in
  let entry = Cursor.start (Ssa.entry graph) in
  let zero = emit graph entry (Const_int 0n) [||] in
  let bound = emit graph entry (load ~is_atomic:false) [| zero |] in
  (match guard with
  | None -> finish graph entry (goto preheader [||])
  | Some (comparison, threshold) ->
    let cond =
      emit graph entry (Intop_imm (Icomp comparison, threshold)) [| bound |]
    in
    let ifnot = if duplicate_targets then preheader else exit in
    finish graph entry
      (Switch { index = cond; targets = [| ifnot; preheader |] }));
  finish graph (Cursor.start preheader) (goto header [| zero |]);
  let cursor = Cursor.start header in
  let counter = Block.param header 0 in
  let comparison = if step > 0 then Cmm.Clt else Cmm.Cgt in
  let cond =
    emit graph cursor (Intop (Icomp comparison)) [| counter; bound |]
  in
  finish graph cursor (Switch { index = cond; targets = [| exit; latch |] });
  let cursor = Cursor.start latch in
  let next = emit graph cursor (Intop_imm (Iadd, step)) [| counter |] in
  finish graph cursor (goto header [| next |]);
  let cursor = Cursor.start exit in
  let result = emit graph cursor (Const_int 42n) [||] in
  finish graph cursor
    (Continue { continuation = Return; args = [| Arg result |] });
  Ssa.finish_graph graph

(* Further loop shapes: how the header is entered, what its exit test is, what
   its instructions compute, and how the loop relates to others. Each builds a
   graph whose loop counts from zero towards ten by one unless said otherwise,
   and returns the graph with the number of loops the pass must delete from it.
   Only the analysis is exercised: deleting the loop must preserve the program,
   and these graphs are not run. *)
module Shape = struct
  let ret v : _ Terminator.t =
    Continue { continuation = Return; args = [| Arg v |] }

  let const graph cursor n = emit graph cursor (Const_int n) [||]

  (* The loop [header -> latch -> header], exiting from the header to [exit]
     when the counter reaches [limit] with the test [cmp]; the header's
     parameter is the counter. [step] is the latch's increment. *)
  let counted_loop ?(cmp = Cmm.Clt) ?(limit = 10) ?(step = 1) graph ~header
      ~latch ~exit =
    let cursor = Cursor.start header in
    let i = Block.param header 0 in
    let cond = emit graph cursor (Intop_imm (Icomp cmp, limit)) [| i |] in
    finish graph cursor (Switch { index = cond; targets = [| exit; latch |] });
    let cursor = Cursor.start latch in
    let next = emit graph cursor (Intop_imm (Iadd, step)) [| i |] in
    finish graph cursor (goto header [| next |])

  let return_const graph block n =
    let cursor = Cursor.start block in
    finish graph cursor (ret (const graph cursor n))

  (* The header is entered from a [Call]'s continuation: the counter's initial
     value is the call's result. Deleted. *)
  let call_continuation_header () =
    let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
    let header = block graph Cmm.typ_int in
    let latch = block graph [||] and exit = block graph [||] in
    finish graph
      (Cursor.start (Ssa.entry graph))
      (Call
         { op = Direct (Cmm.global_symbol "f");
           args = [||];
           continuation = Goto header;
           may_raise = false;
           nontail = true
         });
    counted_loop graph ~header ~latch ~exit;
    return_const graph exit 42n;
    Ssa.finish_graph graph, 1

  (* The exit test and its step: continuing while the counter equals the bound
     terminates with any non-zero step ([Ceq]: deleted); an unsigned comparison,
     a zero step and a [Sub min_int] step (whose negation is not an [int]) are
     not recognised (kept). *)
  let exit_test ~cmp ~limit ~step ~sub_min_int =
    let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
    let header = block graph Cmm.typ_int in
    let latch = block graph [||] and exit = block graph [||] in
    let entry = Cursor.start (Ssa.entry graph) in
    finish graph entry (goto header [| const graph entry 0n |]);
    if sub_min_int
    then begin
      let cursor = Cursor.start header in
      let i = Block.param header 0 in
      let cond = emit graph cursor (Intop_imm (Icomp cmp, limit)) [| i |] in
      finish graph cursor (Switch { index = cond; targets = [| latch; exit |] });
      let cursor = Cursor.start latch in
      let next = emit graph cursor (Intop_imm (Isub, Int.min_int)) [| i |] in
      finish graph cursor (goto header [| next |])
    end
    else counted_loop graph ~cmp ~limit ~step ~header ~latch ~exit;
    return_const graph exit 42n;
    Ssa.finish_graph graph

  (* A header instruction whose value is returned after the loop: computed from
     a loop-invariant value, it is the same on every visit and the loop is
     deleted; computed from the counter, it keeps the loop. *)
  let header_value_live_out ~from_counter =
    let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
    let header = block graph Cmm.typ_int in
    let latch = block graph [||] and exit = block graph [||] in
    let entry = Cursor.start (Ssa.entry graph) in
    let zero = const graph entry 0n in
    finish graph entry (goto header [| zero |]);
    let cursor = Cursor.start header in
    let i = Block.param header 0 in
    let value =
      emit graph cursor
        (Intop_imm (Iadd, 5))
        [| (if from_counter then i else zero) |]
    in
    let cond = emit graph cursor (Intop_imm (Icomp Cmm.Clt, 10)) [| i |] in
    finish graph cursor (Switch { index = cond; targets = [| exit; latch |] });
    let cursor = Cursor.start latch in
    let next = emit graph cursor (Intop_imm (Iadd, 1)) [| i |] in
    finish graph cursor (goto header [| next |]);
    finish graph (Cursor.start exit) (ret value);
    Ssa.finish_graph graph, if from_counter then 0 else 1

  (* Two loops one after the other, the second counting down, sharing their exit
     block. Both deleted. *)
  let two_sequential_loops () =
    let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
    let header1 = block graph Cmm.typ_int and latch1 = block graph [||] in
    let header2 = block graph Cmm.typ_int and latch2 = block graph [||] in
    let between = block graph [||] and exit = block graph [||] in
    let entry = Cursor.start (Ssa.entry graph) in
    let zero = const graph entry 0n in
    finish graph entry (goto header1 [| zero |]);
    counted_loop graph ~header:header1 ~latch:latch1 ~exit:between;
    finish graph (Cursor.start between) (goto header2 [| zero |]);
    counted_loop graph ~cmp:Cmm.Cgt ~limit:(-10) ~step:(-1) ~header:header2
      ~latch:latch2 ~exit;
    return_const graph exit 42n;
    Ssa.finish_graph graph, 2

  (* The first loop exits into the header of a second, parameterless loop whose
     test is loop-invariant. The first is deleted; the second has no induction
     variable to show it terminates and is kept. *)
  let exit_into_loop_header () =
    let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
    let header1 = block graph Cmm.typ_int and latch1 = block graph [||] in
    let header2 = block graph [||] and latch2 = block graph [||] in
    let exit = block graph [||] in
    let entry = Cursor.start (Ssa.entry graph) in
    let zero = const graph entry 0n in
    let flag = const graph entry 1n in
    finish graph entry (goto header1 [| zero |]);
    counted_loop graph ~header:header1 ~latch:latch1 ~exit:header2;
    finish graph (Cursor.start header2)
      (Switch { index = flag; targets = [| exit; latch2 |] });
    finish graph (Cursor.start latch2) (goto header2 [||]);
    return_const graph exit 42n;
    Ssa.finish_graph graph, 1

  (* The header's exit edge goes to a parameterless block that passes a
     header-computed constant on to a block with a parameter. The parameterless
     block is the exit target, so the loop is deleted. *)
  let exit_through_trampoline () =
    let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
    let header = block graph Cmm.typ_int in
    let latch = block graph [||] in
    let trampoline = block graph [||] in
    let final = block graph Cmm.typ_int in
    let entry = Cursor.start (Ssa.entry graph) in
    finish graph entry (goto header [| const graph entry 0n |]);
    let cursor = Cursor.start header in
    let i = Block.param header 0 in
    let k = const graph cursor 7n in
    let cond = emit graph cursor (Intop_imm (Icomp Cmm.Clt, 10)) [| i |] in
    finish graph cursor
      (Switch { index = cond; targets = [| trampoline; latch |] });
    let cursor = Cursor.start latch in
    let next = emit graph cursor (Intop_imm (Iadd, 1)) [| i |] in
    finish graph cursor (goto header [| next |]);
    finish graph (Cursor.start trampoline) (goto final [| k |]);
    finish graph (Cursor.start final) (ret (Block.param final 0));
    Ssa.finish_graph graph, 1
end

let check_shape (graph, expected) = check_deletions graph expected

let graph_with_result build =
  let graph = Ssa.create_graph function_info ~keep_unused_ops:false in
  let cursor = Cursor.start (Ssa.entry graph) in
  let result = build (emit graph cursor) in
  finish graph cursor
    (Continue { continuation = Return; args = [| Arg result |] });
  let graph = Ssa.finish_graph graph in
  match Block.terminator (Ssa.entry graph) with
  | Continue { args = [| Arg result |]; _ } -> result
  | _ -> failwith "unexpected terminator"

let check_atom result =
  let (form, _), ctx = AffineSsa.linearize AffineSsa.new_ctx [] result in
  (* The walk of the value graph is bounded by the linearizer's node budget, so
     both long paths and shared DAGs allocate few atoms. *)
  assert (List.length ctx.AffineSsa.atoms <= 258);
  match form with
  | { FourierMotzkin.Affine.const = 0; terms = [(id, 1)] } ->
    assert (Value.equal (List.assoc id ctx.atoms) result)
  | _ -> failwith "expected an exact atom for the result"

let () =
  check_loop Straight 1;
  check_loop Diamond 1;
  check_loop Inner_cycle 0;
  check_loop Irreducible_cycle 0;
  List.iter
    (fun location ->
      check_extra location (Intop_imm (Iadd, 0)) 1;
      check_extra location (Intop_imm (Isub, 1)) 1;
      check_extra location (Intop_imm (Icomp Cmm.Ceq, 0)) 1;
      check_extra location (load ~is_atomic:false) 0;
      check_extra location (load ~is_atomic:true) 0;
      check_extra location (Intop_imm (Idiv { signed = true }, 1)) 0;
      check_extra location (Intop_imm (Ilsr, 1)) 0)
    [Header; Latch];
  check_extra Before_loop (load ~is_atomic:false) 1;
  check_deletions (loop ~prefix:Opaque_call Straight) 1;
  (* A trap handler set up around the loop is stepped over by the observation
     semantics (SsaContext.v), so it does not prevent deletion. *)
  let trapped = loop ~prefix:Trap_handler Straight in
  assert (DeleteEmptyLoops.graph_supported (Ssa.blocks trapped));
  check_deletions trapped 1;
  check_extra ~extra_type:[||] Before_loop (Const_int 0n) 0;
  check_extra ~extra_type:[| Cmm.Int; Cmm.Int |] Before_loop (Const_int 0n) 0;
  (* Exercise the bounded walk at a small boundary rather than allocate billions
     of graph blocks. The production limit is fixed in the model. *)
  assert (not (OxCamlList.length_at_most Int.min_int [()]));
  assert (OxCamlList.length_at_most Int.max_int [()]);
  assert (not (OxCamlList.length_at_most (-1) []));
  assert (OxCamlList.length_at_most 0 []);
  assert (not (OxCamlList.length_at_most 0 [()]));
  assert (OxCamlList.length_at_most 2 [(); ()]);
  assert (not (OxCamlList.length_at_most 2 [(); (); ()]));
  List.iter
    (fun (step, guard, duplicate_targets, expected) ->
      check_deletions
        (register_bound_loop ~step ~guard ~duplicate_targets)
        expected)
    [ 2, Some (Cmm.Cle, 10), false, 1;
      2, None, false, 0;
      2, Some (Cmm.Cgt, 10), false, 0;
      2, Some (Cmm.Cle, 10), true, 0;
      -2, Some (Cmm.Cge, -10), false, 1;
      -2, None, false, 0;
      -2, Some (Cmm.Clt, -10), false, 0;
      -2, Some (Cmm.Cge, -10), true, 0 ];
  check_shape (Shape.call_continuation_header ());
  check_deletions
    (Shape.exit_test ~cmp:Cmm.Ceq ~limit:0 ~step:1 ~sub_min_int:false)
    1;
  check_deletions
    (Shape.exit_test ~cmp:Cmm.Cult ~limit:10 ~step:1 ~sub_min_int:false)
    0;
  check_deletions
    (Shape.exit_test ~cmp:Cmm.Clt ~limit:10 ~step:0 ~sub_min_int:false)
    0;
  check_deletions
    (Shape.exit_test ~cmp:Cmm.Cge ~limit:10 ~step:0 ~sub_min_int:true)
    0;
  check_shape (Shape.header_value_live_out ~from_counter:false);
  check_shape (Shape.header_value_live_out ~from_counter:true);
  check_shape (Shape.two_sequential_loops ());
  check_shape (Shape.exit_into_loop_header ());
  check_shape (Shape.exit_through_trampoline ());
  (* Ordinary small expressions must still be expanded, not all atomized. *)
  let small =
    graph_with_result (fun emit ->
        let x = emit (Const_int 5n) [||] in
        emit (Intop_imm (Iadd, 7)) [| x |])
  in
  let (form, side), _ = AffineSsa.linearize AffineSsa.new_ctx [] small in
  assert (form = { FourierMotzkin.Affine.const = 12; terms = [] });
  assert (side = []);
  let zero_shift =
    graph_with_result (fun emit ->
        let x = emit (Const_int (-1n)) [||] in
        emit (Intop_imm (Ilsr, 0)) [| x |])
  in
  check_atom zero_shift;
  (* The shared graph has only 34 instructions, but unbounded unfolding produces
     196607 nodes. The outer shift is not recognized by the linearizer, so the
     result is an atom without any walk. *)
  let shared =
    graph_with_result (fun emit ->
        let x = ref (emit (Const_int 1n) [||]) in
        for _ = 1 to 16 do
          let shifted = emit (Intop_imm (Ilsl, 16)) [| !x |] in
          x := emit (Intop Isub) [| !x; shifted |]
        done;
        emit (Intop_imm (Ilsl, 16)) [| !x |])
  in
  check_atom shared;
  (* A shared expression made entirely of recognized operations: the walk must
     stop at the node budget. *)
  let shared_add =
    graph_with_result (fun emit ->
        let x = ref (emit (Const_int 1n) [||]) in
        for _ = 1 to 16 do
          x := emit (Intop Iadd) [| !x; !x |]
        done;
        !x)
  in
  check_atom shared_add;
  let deep =
    graph_with_result (fun emit ->
        let x = ref (emit (Const_int 0n) [||]) in
        for _ = 1 to 2000 do
          x := emit (Intop_imm (Iadd, 1)) [| !x |]
        done;
        !x)
  in
  check_atom deep;
  print_endline "OK"
