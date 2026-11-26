(* The counters through the SSA pipeline: [Ssa_of_cmm] puts those of the Cmm on
   the targets of switches and on calls, [Ssa_tail_call] and [Ssa_simplify] keep
   them with their edges and calls (except that of a self tail call, which
   becomes a jump and is dropped, see [Fdo_dropped_counters]), and [Cfg_of_ssa]
   puts them on the Cfg. Each case prints the terminators with counters after
   each pass, and checks that no counter is lost or added. *)

let () =
  Env.set_current_unit
    (Unit_info.make ~source_file:"ssa_counters.ml"
       ~for_pack_prefix:Compilation_unit.Prefix.empty Impl "ssa_counters")

open Ssa.Export

let function_ =
  Fdo_counter.Function.create_original
    (Fdo_counter.Name.create ~name_str:"Test.f" ~discriminator:0)

let body_hash = Fdo_counter.Body_hash.of_int32 1l

let branch_counter arm =
  Fdo_counter.of_position
    (Fdo_counter.Position.create_branch ~function_ ~body_hash ~index:0 ~arm)

let call_counter index =
  Fdo_counter.of_position
    (Fdo_counter.Position.create_call ~function_ ~body_hash ~index)

let entry_counter =
  Fdo_counter.of_position (Fdo_counter.Position.create_function_entry function_)

let dbg = Debuginfo.none

let annots arm =
  Branch_annotations.create ~likelihood:Likelihood.default
    ~fdo_annotation:(Fdo_annotation.singleton (branch_counter arm))

let int n = Cmm.Cconst_int (n, dbg)

let symbol sym_name = Cmm.global_symbol ~ty:Cmm.typ_code_pointer sym_name

let apply index args : Cmm.expression =
  Cop
    ( Capply
        { result_type = Cmm.typ_int;
          region = Rc_normal;
          callees = None;
          returns = true;
          callsite_counter = Some (call_counter index)
        },
      args,
      dbg )

let direct_call index name args =
  apply index (Cconst_symbol (symbol name, dbg) :: args)

let if_ cond ~ifso ~ifnot : Cmm.expression =
  Cifthenelse (cond, dbg, annots 1, ifso, dbg, annots 2, ifnot, dbg)

let print_counters title counters =
  Format.printf "  @[<hov 2>%s:%a@]@." title
    (Format.pp_print_list
       ~pp_sep:(fun _ () -> ())
       (fun ppf counter ->
         Format.fprintf ppf "@ %s" (Fdo_counter.to_string counter)))
    counters

let mem counter counters = List.exists (Fdo_counter.equal counter) counters

let only_in a b = List.filter (fun counter -> not (mem counter b)) a

let entry_counters annotation =
  Fdo_annotation.to_list (Fdo_annotation.With_body_hash.annotation annotation)

(* The counters of [expr]. *)
let rec cmm_counters (expr : Cmm.expression) =
  let of_annots annots =
    Fdo_annotation.to_list (Branch_annotations.fdo_annotation annots)
  in
  let shallow () =
    let counters = ref [] in
    Cmm.iter_shallow (fun e -> counters := !counters @ cmm_counters e) expr;
    !counters
  in
  match[@warning "-fragile-match"] expr with
  | Cifthenelse (_, _, ifso_annots, _, _, ifnot_annots, _, _) ->
    of_annots ifso_annots @ of_annots ifnot_annots @ shallow ()
  | Cswitch (_, _, cases, _) ->
    List.concat_map
      (fun (_, _, annots) -> of_annots annots)
      (Array.to_list cases)
    @ shallow ()
  | Cop (Capply { callsite_counter; _ }, _, _) ->
    Option.to_list callsite_counter @ shallow ()
  | _ -> shallow ()

let terminator_counters (terminator : finished Terminator.t) =
  match terminator with
  | Switch { targets; index = _ } ->
    List.concat_map
      (fun ({ target = _; fdo_annotation } : _ Terminator.successor) ->
        Fdo_annotation.to_list fdo_annotation)
      (Array.to_list targets)
  | Call
      { op = Direct { callsite_counter; _ } | Indirect { callsite_counter; _ };
        _
      } ->
    Option.to_list callsite_counter
  | Call { op = External _ | Probe _; _ } | Continue _ | Invalid _ -> []

let ssa_counters graph =
  entry_counters (Ssa.function_info graph).fdo_entry_annotation
  @ List.concat_map
      (fun block -> terminator_counters (Block.terminator block))
      (Ssa.blocks graph)

let print_ssa title graph =
  Format.printf "  %s:@." title;
  List.iter
    (fun block ->
      let terminator = Block.terminator block in
      if not (List.is_empty (terminator_counters terminator))
      then
        Format.printf "    %a: %a@." Block.print_id block Terminator.print
          terminator)
    (Ssa.blocks graph)

let cfg_counters (cfg : Cfg.t) =
  entry_counters cfg.fun_fdo_entry_annotation
  @ Cfg.fold_blocks cfg ~init:[] ~f:(fun _ block counters ->
      counters
      @ Fdo_annotation.to_list
          (Cfg.fdo_counters_of_terminator block.terminator.desc))

let print_cfg (cfg : Cfg.t) =
  Format.printf "  cfg:@.";
  Cfg.iter_blocks cfg ~f:(fun label block ->
      if
        not
          (Fdo_annotation.is_empty
             (Cfg.fdo_counters_of_terminator block.terminator.desc))
      then
        Format.printf "    %a: %a@." Label.format label
          (Printcfg.terminator_desc ~sep:"; ")
          block.terminator.desc)

let var name =
  Backend_var.With_provenance.create (Backend_var.create_local name)

let test name ?(arg_type = Cmm.typ_int) ~args body =
  let args = List.map (fun arg -> var arg, arg_type) args in
  let body =
    body
      (List.map
         (fun (v, _) -> Cmm.Cvar (Backend_var.With_provenance.var v))
         args)
  in
  let fundecl : Cmm.fundecl =
    { fun_name = symbol "f";
      fun_args = args;
      fun_body = body;
      fun_codegen_options = [];
      fun_poll = Default_poll;
      fun_dbg = dbg;
      fun_fdo_entry_annotation =
        Fdo_annotation.With_body_hash.create
          ~annotation:(Fdo_annotation.singleton entry_counter)
          ~body_hash:(Some body_hash);
      fun_ret_type = Cmm.typ_int
    }
  in
  let in_cmm = entry_counter :: cmm_counters body in
  Fdo_dropped_counters.reset ();
  Reg.clear_relocatable_regs ();
  let ppf_dump = Format.err_formatter in
  let of_cmm = Ssa_of_cmm.convert ~ppf_dump fundecl ~keep_unused_ops:true in
  let tail_calls = Ssa_tail_call.run ~ppf_dump ~keep_unused_ops:true of_cmm in
  let simplified = Ssa_simplify.run ~ppf_dump tail_calls in
  Reg.clear_relocatable_regs ();
  let cfg =
    Cfg_with_layout.cfg
      (Cfg_of_ssa.convert ~future_funcnames:Misc.Stdlib.String.Set.empty
         simplified)
  in
  let dropped =
    Fdo_dropped_counters.registered ()
    |> Fdo_dropped_counters.to_list |> List.map fst
  in
  Format.printf "%s:@.%a@." name Printcmm.expression body;
  print_ssa "Ssa_of_cmm" of_cmm;
  print_ssa "Ssa_tail_call" tail_calls;
  print_ssa "Ssa_simplify" simplified;
  print_cfg cfg;
  print_counters "dropped" dropped;
  let check title counters =
    print_counters (title ^ ", lost") (only_in in_cmm (counters @ dropped));
    print_counters (title ^ ", added") (only_in counters in_cmm)
  in
  check "ssa" (ssa_counters simplified);
  check "cfg" (cfg_counters cfg);
  Format.printf "@."

let () =
  test "integer comparison" ~args:["x"] (function
    | [x] -> if_ (Cop (Ccmpi Clt, [x; int 0], dbg)) ~ifso:(int 3) ~ifnot:(int 5)
    | _ -> assert false);
  test "float comparison" ~arg_type:Cmm.typ_float ~args:["x"; "y"] (function
    | [x; y] ->
      if_
        (Cop (Ccmpf (Float64, CFnlt), [x; y], dbg))
        ~ifso:(int 3) ~ifnot:(int 5)
    | _ -> assert false);
  test "truth test" ~args:["x"] (function
    | [x] -> if_ x ~ifso:(int 3) ~ifnot:(int 5)
    | _ -> assert false);
  (* The low bit of the argument, as a condition: an odd test. *)
  test "odd test" ~args:["x"] (function
    | [x] -> if_ (Cop (Cand, [x; int 1], dbg)) ~ifso:(int 3) ~ifnot:(int 5)
    | _ -> assert false);
  (* A parity test is a comparison that cannot be negated: [Ssa_simplify] swaps
     the targets instead, with their counters. *)
  test "parity test" ~args:["x"] (function
    | [x] ->
      if_
        (Cop (Ccmpi Ceq, [Cop (Cand, [x; int 1], dbg); int 0], dbg))
        ~ifso:(int 3) ~ifnot:(int 5)
    | _ -> assert false);
  (* Values 0 and 2 share case 0: both targets have its counter. *)
  test "switch with shared cases" ~args:["x"] (function
    | [x] ->
      Cswitch
        ( x,
          [| 0; 1; 0; 2 |],
          [| int 3, dbg, annots 0; int 5, dbg, annots 1; int 7, dbg, annots 2 |],
          dbg )
    | _ -> assert false);
  test "calls" ~args:["x"; "g"] (function
    | [x; g] ->
      let v = var "v" in
      Clet
        ( v,
          direct_call 0 "h" [x],
          Cop
            ( Caddi,
              [apply 1 [g; Cvar (Backend_var.With_provenance.var v)]; int 2],
              dbg ) )
    | _ -> assert false);
  (* [Ssa_tail_call] turns the calls whose result is returned into tail calls,
     which keep their counters. *)
  test "tail calls" ~args:["x"; "g"] (function
    | [x; g] ->
      if_
        (Cop (Ccmpi Clt, [x; int 0], dbg))
        ~ifso:(direct_call 0 "h" [x])
        ~ifnot:(apply 1 [g; x])
    | _ -> assert false);
  (* A self tail call becomes a jump, which drops its counter. *)
  test "self tail call" ~args:["x"] (function
    | [x] ->
      if_
        (Cop (Ccmpi Clt, [x; int 0], dbg))
        ~ifso:(direct_call 0 "f" [Cop (Caddi, [x; int 2], dbg)])
        ~ifnot:x
    | _ -> assert false)
