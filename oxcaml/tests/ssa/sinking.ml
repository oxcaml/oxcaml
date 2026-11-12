open Ssa.Export

let create ?(parameters = []) name =
  let info : Ssa.Function_info.t =
    { sym_name = name;
      parameters;
      codegen_options = [];
      dbg = Debuginfo.none;
      poll = Lambda.Default_poll;
      ret_type = Cmm.typ_val
    }
  in
  let graph = Ssa.create_graph info ~keep_unused_ops:false in
  graph, Cursor.start (Ssa.entry graph)

let block graph params =
  Block.create graph ~params ~cold:false ~handler_dbg:Debuginfo.none

let emit graph cursor op typ args =
  Cursor.emit_op graph cursor op Debuginfo.none typ args

let constant graph cursor typ n = (emit graph cursor (Const_int n) typ [||]).(0)

let opaque_use graph cursor args =
  let (_ : _ array) = emit graph cursor Opaque [||] args in
  ()

let store graph cursor pointer offset value =
  let addressing = Arch.offset_addressing Arch.identity_addressing offset in
  let chunk = Select_utils.chunk_of_machtype_component (Value.typ value) in
  let (_ : _ array) =
    emit graph cursor
      (Store (chunk, addressing, false))
      [||] [| value; pointer |]
  in
  ()

let allocate graph cursor mode field =
  let pointer =
    (emit graph cursor
       (Alloc { bytes = 16; mode; dbginfo = [] })
       Cmm.typ_val [||]).(0)
  in
  let addressing = Arch.offset_addressing Arch.identity_addressing (-8) in
  (match
     Cfg_selection.select_store ~is_assign:false addressing
       (Cmm.Cconst_int (1024, Debuginfo.none))
   with
  | Rewritten (op, Ctuple []) ->
    let (_ : _ array) = emit graph cursor op [||] [| pointer |] in
    ()
  | Rewritten _ | Use_default | Maybe_out_of_range ->
    store graph cursor pointer (-8) (constant graph cursor Cmm.typ_int 1024n));
  store graph cursor pointer 0 (field ());
  pointer

let finish graph cursor terminator =
  Cursor.finish_block graph cursor ~dbg:Debuginfo.none terminator

let return graph cursor value =
  finish graph cursor
    (Continue { continuation = Return; args = [| Arg value |] })

let goto graph cursor target args =
  finish graph cursor
    (Continue
       { continuation = Goto target;
         args = Array.map (fun value -> Terminator.Arg value) args
       })

let branch graph cursor yes no =
  let condition = constant graph cursor Cmm.typ_int 1n in
  finish graph cursor (Switch { index = condition; targets = [| no; yes |] })

let return_zero graph cursor =
  return graph cursor (constant graph cursor Cmm.typ_val 1n)

let finish_diamond graph cursor pointer =
  let yes = block graph [||] in
  let no = block graph [||] in
  branch graph cursor yes no;
  return graph (Cursor.start yes) pointer;
  return_zero graph (Cursor.start no)

let check_finished name graph ~allocations ~constructions =
  let result = Ssa_sink.run ~ppf_dump:Format.err_formatter graph in
  let actual_allocations =
    List.map
      (fun block ->
        Array.fold_left
          (fun count (instr : Ssa.finished Instruction.t) ->
            match instr with Op { op = Alloc _; _ } -> count + 1 | _ -> count)
          0 (Block.body block))
      (Ssa.blocks result)
  in
  let actual_constructions =
    List.map
      (fun block -> List.length (Ssa_allocation.find_in_block block))
      (Ssa.blocks result)
  in
  if
    (not (List.equal Int.equal actual_allocations allocations))
    || not (List.equal Int.equal actual_constructions constructions)
  then begin
    Format.eprintf "%s: unexpected placements@.Before:@.%aAfter:@.%a@." name
      Ssa_print.print graph Ssa_print.print result;
    failwith name
  end;
  Printf.printf "%s: passed\n" name;
  result

let check name graph ~allocations ~constructions =
  check_finished name (Ssa.finish_graph graph) ~allocations ~constructions

let () =
  let graph, entry = create "group and interleaved input" in
  let x = constant graph entry Cmm.typ_int 7n in
  let pointer =
    allocate graph entry Heap (fun () ->
        (emit graph entry (Intop_imm (Imul, 3)) Cmm.typ_int [| x |]).(0))
  in
  finish_diamond graph entry pointer;
  ignore
    (check "group and interleaved input" graph ~allocations:[0; 1; 0]
       ~constructions:[0; 1; 0]);
  let graph, entry = create "dependent allocation groups" in
  let first =
    allocate graph entry Heap (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  let second = allocate graph entry Heap (fun () -> first) in
  finish_diamond graph entry second;
  ignore
    (check "dependent allocation groups" graph ~allocations:[0; 2; 0]
       ~constructions:[0; 2; 0]);
  let graph, entry = create "mutable initializer stays before call" in
  let base = constant graph entry Cmm.typ_val 1n in
  let value =
    (emit graph entry
       (Load
          { memory_chunk = Word_val;
            addressing_mode = Arch.identity_addressing;
            mutability = Mutable;
            is_atomic = false
          })
       Cmm.typ_val [| base |]).(0)
  in
  let pointer = allocate graph entry Heap (fun () -> value) in
  let continuation = block graph [||] in
  finish graph entry
    (Call
       { op = Direct (Cmm.global_symbol "mutate");
         args = [||];
         continuation = Goto continuation;
         may_raise = true;
         nontail = false
       });
  finish_diamond graph (Cursor.start continuation) pointer;
  let result =
    check "mutable initializer stays before call" graph
      ~allocations:[0; 0; 1; 0] ~constructions:[0; 0; 1; 0]
  in
  assert (
    Array.exists
      (function Instruction.Op { op = Load _; _ } -> true | _ -> false)
      (Block.body (Ssa.entry result)));
  let graph, entry = create "earlier external use" in
  let pointer =
    allocate graph entry Heap (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  opaque_use graph entry [| pointer |];
  finish_diamond graph entry pointer;
  ignore
    (check "earlier external use" graph ~allocations:[1; 0; 0]
       ~constructions:[1; 0; 0]);
  let graph, entry = create "unfinished later allocation" in
  let pointer =
    allocate graph entry Heap (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  let unfinished =
    (emit graph entry
       (Alloc { bytes = 16; mode = Heap; dbginfo = [] })
       Cmm.typ_val [||]).(0)
  in
  let yes = block graph [||] in
  let no = block graph [||] in
  branch graph entry yes no;
  List.iter
    (fun target ->
      let cursor = Cursor.start target in
      store graph cursor unfinished (-8)
        (constant graph cursor Cmm.typ_int 1024n);
      store graph cursor unfinished 0 (constant graph cursor Cmm.typ_int 7n);
      if Block.equal target yes
      then return graph cursor pointer
      else return_zero graph cursor)
    [yes; no];
  ignore
    (check "unfinished later allocation" graph ~allocations:[2; 0; 0]
       ~constructions:[1; 0; 0]);
  let graph, entry = create "external Addr initializer" in
  let base = constant graph entry Cmm.typ_val 1n in
  let pointer =
    allocate graph entry Heap (fun () ->
        (emit graph entry (Intop_imm (Iadd, 8)) Cmm.typ_addr [| base |]).(0))
  in
  finish_diamond graph entry pointer;
  ignore
    (check "external Addr initializer" graph ~allocations:[1; 0; 0]
       ~constructions:[1; 0; 0]);
  let graph, entry = create "live Addr at destination" in
  let pointer =
    allocate graph entry Heap (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  let address =
    (emit graph entry
       (Intop_imm (Iadd, 8))
       Cmm.typ_addr
       [| constant graph entry Cmm.typ_val 1n |]).(0)
  in
  opaque_use graph entry [| address |];
  let yes = block graph [||] in
  let no = block graph [||] in
  branch graph entry yes no;
  let yes_cursor = Cursor.start yes in
  opaque_use graph yes_cursor [| address |];
  return graph yes_cursor pointer;
  return_zero graph (Cursor.start no);
  ignore
    (check "live Addr at destination" graph ~allocations:[1; 0; 0]
       ~constructions:[1; 0; 0]);
  let graph, entry = create "Addr block parameter" in
  let pointer =
    allocate graph entry Heap (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  let address =
    (emit graph entry
       (Intop_imm (Iadd, 8))
       Cmm.typ_addr
       [| constant graph entry Cmm.typ_val 1n |]).(0)
  in
  let pass = block graph [||] in
  let yes = block graph Cmm.typ_addr in
  let no = block graph [||] in
  branch graph entry pass no;
  goto graph (Cursor.start pass) yes [| address |];
  let yes_cursor = Cursor.start yes in
  opaque_use graph yes_cursor (Block.params yes);
  return graph yes_cursor pointer;
  return_zero graph (Cursor.start no);
  ignore
    (check "Addr block parameter" graph ~allocations:[1; 0; 0; 0]
       ~constructions:[1; 0; 0; 0]);
  List.iter
    (fun across_block ->
      let name =
        if across_block then "region in later block" else "region in source"
      in
      let graph, entry = create name in
      let pointer =
        allocate graph entry Heap (fun () ->
            constant graph entry Cmm.typ_int 7n)
      in
      let cursor =
        if across_block
        then begin
          let next = block graph [||] in
          goto graph entry next [||];
          Cursor.start next
        end
        else entry
      in
      let region = (emit graph cursor Begin_region Cmm.typ_int [||]).(0) in
      let (_ : _ array) = emit graph cursor End_region [||] [| region |] in
      finish_diamond graph cursor pointer;
      let counts = if across_block then [1; 0; 0; 0] else [1; 0; 0] in
      ignore (check name graph ~allocations:counts ~constructions:counts))
    [false; true];
  let graph, entry = create "pointer reinterpretation" in
  let pointer =
    allocate graph entry Heap (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  let raw =
    (emit graph entry (Reinterpret_cast Int_of_value) Cmm.typ_int
       [| constant graph entry Cmm.typ_val 1n |]).(0)
  in
  let yes = block graph [||] in
  let no = block graph [||] in
  branch graph entry yes no;
  let cursor = Cursor.start yes in
  opaque_use graph cursor [| raw |];
  return graph cursor pointer;
  return_zero graph (Cursor.start no);
  ignore
    (check "pointer reinterpretation" graph ~allocations:[1; 0; 0]
       ~constructions:[1; 0; 0]);
  let graph, entry = create "not into a loop" in
  let pointer =
    allocate graph entry Heap (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  let loop = block graph [||] in
  let no = block graph [||] in
  branch graph entry loop no;
  let cursor = Cursor.start loop in
  opaque_use graph cursor [| pointer |];
  branch graph cursor loop no;
  return_zero graph (Cursor.start no);
  ignore
    (check "not into a loop" graph ~allocations:[1; 0; 0]
       ~constructions:[1; 0; 0]);
  (* The original allocation is after the use of the incoming Addr and before
     the definition of the outgoing Addr. Removing it would make polling add a
     GC point on the back edge, where the outgoing Addr is live. *)
  List.iter
    (fun (name, typ, counts) ->
      let graph, entry = create name in
      let base = constant graph entry Cmm.typ_val 1n in
      let value = (emit graph entry (Intop_imm (Iadd, 8)) typ [| base |]).(0) in
      let header = block graph typ in
      let body = block graph [||] in
      let back_edge = block graph [||] in
      let yes = block graph [||] in
      let no = block graph [||] in
      goto graph entry header [| value |];
      let cursor = Cursor.start header in
      opaque_use graph cursor (Block.params header);
      branch graph cursor body no;
      let cursor = Cursor.start body in
      let pointer =
        allocate graph cursor Heap (fun () ->
            constant graph cursor Cmm.typ_int 7n)
      in
      branch graph cursor yes back_edge;
      let cursor = Cursor.start back_edge in
      let value =
        (emit graph cursor (Intop_imm (Iadd, 8)) typ [| base |]).(0)
      in
      goto graph cursor header [| value |];
      return graph (Cursor.start yes) pointer;
      return_zero graph (Cursor.start no);
      ignore (check name graph ~allocations:counts ~constructions:counts))
    [ ( "preserve loop safepoint with Addr back edge",
        Cmm.typ_addr,
        [0; 0; 1; 0; 0; 0] );
      "sink out of a loop when polling is safe", Cmm.typ_int, [0; 0; 0; 0; 1; 0]
    ];
  (* The loop's only safepoint cannot move to the exit: polling cannot split the
     exceptional back edge that would then need a replacement poll. *)
  let name = "preserve loop safepoint with exceptional back edge" in
  let parameter name typ =
    Backend_var.With_provenance.create (Backend_var.create_local name), typ
  in
  let graph, entry =
    create name
      ~parameters:
        [parameter "condition" Cmm.typ_int; parameter "exn" Cmm.typ_val]
  in
  let handler = block graph Cmm.typ_val in
  let use = block graph [||] in
  let retry = block graph [||] in
  let raise_to_handler cursor exn =
    finish graph cursor
      (Continue
         { continuation = Raise Lambda.Raise_notrace; args = [| Arg exn |] })
  in
  Cursor.emit_push_trap entry ~handler;
  raise_to_handler entry (Block.param (Ssa.entry graph) 1);
  let cursor = Cursor.start handler in
  Cursor.emit_push_trap cursor ~handler;
  let pointer =
    allocate graph cursor Heap (fun () -> constant graph cursor Cmm.typ_int 7n)
  in
  finish graph cursor
    (Switch
       { index = Block.param (Ssa.entry graph) 0; targets = [| retry; use |] });
  let cursor = Cursor.start use in
  Cursor.emit_pop_trap cursor ~handler;
  return graph cursor pointer;
  raise_to_handler (Cursor.start retry) (Block.param handler 0);
  let graph = Ssa.finish_graph graph in
  let lower_and_poll graph =
    Reg.clear_relocatable_regs ();
    let cfg =
      Cfg_of_ssa.convert ~future_funcnames:Misc.Stdlib.String.Set.empty graph
    in
    ignore
      (Cfg_polling.instrument_fundecl
         ~future_funcnames:Misc.Stdlib.String.Set.empty cfg
        : Cfg_with_layout.t)
  in
  Misc.protect_refs
    [Misc.R (Oxcaml_flags.disable_poll_insertion, false)]
    (fun () ->
      lower_and_poll graph;
      let result = Ssa_sink.run ~ppf_dump:Format.err_formatter graph in
      lower_and_poll result);
  Printf.printf "%s: passed\n" name;
  (* This local allocation is unfinished on the back edge. A new poll there
     would expose it before the header block has initialized it. *)
  let graph, entry = create "preserve loop safepoint with unfinished block" in
  let initial =
    allocate graph entry Local (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  let header = block graph Cmm.typ_val in
  let body = block graph [||] in
  let back_edge = block graph [||] in
  let yes = block graph [||] in
  let no = block graph [||] in
  goto graph entry header [| initial |];
  let cursor = Cursor.start header in
  let incoming = Block.param header 0 in
  store graph cursor incoming (-8) (constant graph cursor Cmm.typ_int 1024n);
  store graph cursor incoming 0 (constant graph cursor Cmm.typ_int 7n);
  branch graph cursor body no;
  let cursor = Cursor.start body in
  let pointer =
    allocate graph cursor Heap (fun () -> constant graph cursor Cmm.typ_int 7n)
  in
  branch graph cursor yes back_edge;
  let cursor = Cursor.start back_edge in
  let unfinished =
    (emit graph cursor
       (Alloc { bytes = 16; mode = Local; dbginfo = [] })
       Cmm.typ_val [||]).(0)
  in
  goto graph cursor header [| unfinished |];
  return graph (Cursor.start yes) pointer;
  return_zero graph (Cursor.start no);
  ignore
    (check "preserve loop safepoint with unfinished block" graph
       ~allocations:[1; 0; 1; 1; 0; 0] ~constructions:[0; 0; 1; 0; 0; 0]);
  (* If the allocation moves off the tail-call path, prologue polling may be
     reinstated before the Addr parameter's use. *)
  let parameter = Backend_var.create_local "address" in
  let graph, entry =
    create "unsafe prologue poll"
      ~parameters:[Backend_var.With_provenance.create parameter, Cmm.typ_addr]
  in
  opaque_use graph entry (Block.params (Ssa.entry graph));
  let pointer =
    allocate graph entry Heap (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  let yes = block graph [||] in
  let no = block graph [||] in
  branch graph entry yes no;
  return graph (Cursor.start yes) pointer;
  finish graph (Cursor.start no)
    (Call
       { op = Direct (Cmm.global_symbol "tail_call");
         args = [||];
         continuation = Return;
         may_raise = true;
         nontail = false
       });
  ignore
    (check "unsafe prologue poll" graph ~allocations:[1; 0; 0]
       ~constructions:[1; 0; 0]);
  let graph, entry = create "local allocation stays" in
  let pointer =
    allocate graph entry Local (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  finish_diamond graph entry pointer;
  ignore
    (check "local allocation stays" graph ~allocations:[1; 0; 0]
       ~constructions:[0; 0; 0]);
  let graph, entry = create "debugger marker follows group" in
  let pointer =
    allocate graph entry Heap (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  let (_ : _ array) =
    emit graph entry
      (Name_for_debugger
         { ident = Ident.create_local "p";
           which_parameter = None;
           provenance = None;
           regs = [||]
         })
      [||] [| pointer |]
  in
  finish_diamond graph entry pointer;
  let result =
    check "debugger marker follows group" graph ~allocations:[0; 1; 0]
      ~constructions:[0; 1; 0]
  in
  let marker_counts =
    List.map
      (fun block ->
        Array.fold_left
          (fun n -> function
            | Instruction.Op { op = Name_for_debugger _; _ } -> n + 1 | _ -> n)
          0 (Block.body block))
      (Ssa.blocks result)
  in
  assert (List.equal Int.equal marker_counts [0; 1; 0]);
  (* Exercise the actual producer, including ARM64's internal Addr resets. *)
  let pointer = Backend_var.create_local "p" in
  let condition = Backend_var.create_local "condition" in
  let fallback = Backend_var.create_local "fallback" in
  let dbg = Debuginfo.none in
  let fd : Cmm.fundecl =
    { fun_name = Cmm.global_symbol "large_construction";
      fun_args =
        [ Backend_var.With_provenance.create condition, Cmm.typ_int;
          Backend_var.With_provenance.create fallback, Cmm.typ_val ];
      fun_body =
        Clet
          ( Backend_var.With_provenance.create pointer,
            Cop
              ( Calloc (Heap, Alloc_block_kind_other),
                Cconst_int (600 * 1024, dbg)
                :: List.init 600 (fun i -> Cmm.Cconst_int (i, dbg)),
                dbg ),
            Cifthenelse
              (Cvar condition, dbg, Cvar pointer, dbg, Cvar fallback, dbg) );
      fun_codegen_options = [];
      fun_poll = Lambda.Default_poll;
      fun_dbg = dbg;
      fun_ret_type = Cmm.typ_val
    }
  in
  let graph =
    Ssa_of_cmm.convert ~ppf_dump:Format.err_formatter ~keep_unused_ops:false fd
  in
  ignore
    (check_finished "large construction with address resets" graph
       ~allocations:[0; 1; 0] ~constructions:[0; 1; 0]);
  (* Branch fusion makes the Addr operand live at the potential destination, so
     the allocation must stay put. Check the lowered CFG, not just SSA. *)
  let parameter name typ =
    Backend_var.With_provenance.create (Backend_var.create_local name), typ
  in
  let graph, entry =
    create "derived pointer comparison"
      ~parameters:[parameter "base" Cmm.typ_val; parameter "flag" Cmm.typ_int]
  in
  let pointer =
    allocate graph entry Heap (fun () -> constant graph entry Cmm.typ_int 7n)
  in
  let address =
    (emit graph entry
       (Intop_imm (Iadd, 8))
       Cmm.typ_addr
       [| Block.param (Ssa.entry graph) 0 |]).(0)
  in
  let comparison =
    (emit graph entry (Intop_imm (Icomp Cne, 0)) Cmm.typ_int [| address |]).(0)
  in
  let use = block graph [||] in
  let bypass = block graph [||] in
  let yes = block graph [||] in
  let no = block graph [||] in
  let flag = Block.param (Ssa.entry graph) 1 in
  finish graph entry (Switch { index = flag; targets = [| bypass; use |] });
  let cursor = Cursor.start use in
  let (_ : _ array) = emit graph cursor Opaque Cmm.typ_val [| pointer |] in
  finish graph cursor (Switch { index = comparison; targets = [| no; yes |] });
  return_zero graph (Cursor.start bypass);
  let cursor = Cursor.start yes in
  return graph cursor (constant graph cursor Cmm.typ_val 3n);
  let cursor = Cursor.start no in
  return graph cursor (constant graph cursor Cmm.typ_val 5n);
  let check_gc_liveness graph =
    Reg.clear_relocatable_regs ();
    let cfg =
      Cfg_of_ssa.convert ~future_funcnames:Misc.Stdlib.String.Set.empty graph
      |> Cfg_polling.instrument_fundecl
           ~future_funcnames:Misc.Stdlib.String.Set.empty
    in
    let infos = Cfg_with_infos.make cfg in
    Cfg_with_infos.fold_body_instructions infos ~init:()
      ~f:(fun () (instr : Cfg.basic Cfg.instruction) ->
        match instr.desc with
        | Op (Alloc { mode = Heap; _ } | Poll) ->
          let live = Cfg_with_infos.liveness_find infos instr.id in
          if
            Reg.Set.exists
              (fun (reg : Reg.t) -> Cmm.equal_machtype_component reg.typ Addr)
              live.across
          then begin
            Format.eprintf "Addr live across a GC point:@.%a@."
              Printcfg.cfg_with_layout cfg;
            failwith "derived pointer comparison"
          end
        | _ -> ())
  in
  let graph = Ssa.finish_graph graph in
  check_gc_liveness graph;
  let result =
    check_finished "derived pointer comparison" graph
      ~allocations:[1; 0; 0; 0; 0] ~constructions:[1; 0; 0; 0; 0]
  in
  check_gc_liveness result
