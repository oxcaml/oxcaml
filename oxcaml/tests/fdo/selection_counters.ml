(* The counters that instruction selection puts in the Cfg. First, the tests
   that [Select_utils.terminator_of_test] builds for each condition: each
   outcome has the counters of the branch it goes to ([ifso] has those of arm 1,
   [ifnot] those of arm 2). Then, the Cfg selected from Cmm functions: the
   counters of the Cmm must be on the edges and calls of the Cfg, or dropped
   (see [Fdo_dropped_counters]). *)

let () =
  Env.set_current_unit
    (Unit_info.make ~source_file:"selection_counters.ml"
       ~for_pack_prefix:Compilation_unit.Prefix.empty Impl "selection_counters")

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

let print_counters title counters =
  Format.printf "  @[<hov 2>%s:%a@]@." title
    (Format.pp_print_list
       ~pp_sep:(fun _ () -> ())
       (fun ppf counter ->
         Format.fprintf ppf "@ %s" (Fdo_counter.to_string counter)))
    counters

let mem counter counters = List.exists (Fdo_counter.equal counter) counters

let ifso = Label.new_label ()

let ifnot = Label.new_label ()

let print_test name (test : Operation.test) =
  let successor label arm =
    Cfg.successor label
      ~fdo_annotation:(Fdo_annotation.singleton (branch_counter arm))
  in
  Format.printf "%s (ifso: %a, ifnot: %a):@.  %a@." name Label.format ifso
    Label.format ifnot
    (Printcfg.terminator_desc ~sep:"; ")
    (Select_utils.terminator_of_test test ~ifso:(successor ifso 1)
       ~ifnot:(successor ifnot 2))

let integer_comparisons : (string * Cmm.integer_comparison) list =
  [ "eq", Ceq;
    "ne", Cne;
    "lt", Clt;
    "gt", Cgt;
    "le", Cle;
    "ge", Cge;
    "ult", Cult;
    "ugt", Cugt;
    "ule", Cule;
    "uge", Cuge ]

let float_comparisons : (string * Cmm.float_comparison) list =
  [ "eq", CFeq;
    "neq", CFneq;
    "lt", CFlt;
    "nlt", CFnlt;
    "gt", CFgt;
    "ngt", CFngt;
    "le", CFle;
    "nle", CFnle;
    "ge", CFge;
    "nge", CFnge ]

let () =
  print_test "truth" Itruetest;
  print_test "false" Ifalsetest;
  print_test "odd" Ioddtest;
  print_test "even" Ieventest;
  List.iter
    (fun (name, comparison) ->
      print_test ("int " ^ name) (Iinttest comparison);
      print_test ("int imm " ^ name) (Iinttest_imm (comparison, 0)))
    integer_comparisons;
  List.iter
    (fun (name, comparison) ->
      print_test ("float " ^ name) (Ifloattest (Float64, comparison)))
    float_comparisons;
  Format.printf "@."

module Selection = Cfg_selectgen.Make (Cfg_selection)

let dbg = Debuginfo.none

let annots arm =
  Branch_annotations.create ~likelihood:Likelihood.default
    ~fdo_annotation:(Fdo_annotation.singleton (branch_counter arm))

let int n = Cmm.Cconst_int (n, dbg)

let symbol sym_name : Cmm.symbol = { sym_name; sym_global = Global }

let apply ?(region = Lambda.Rc_normal) index args : Cmm.expression =
  Cop
    ( Capply
        { result_type = Cmm.typ_int;
          region;
          callees = None;
          callsite_counter = Some (call_counter index)
        },
      args,
      dbg )

let direct_call ?region index name args =
  apply ?region index (Cconst_symbol (symbol name, dbg) :: args)

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

let cfg_counters (cfg : Cfg.t) =
  Fdo_annotation.to_list
    (Fdo_annotation.With_body_hash.annotation cfg.fun_fdo_entry_annotation)
  @ Cfg.fold_blocks cfg ~init:[] ~f:(fun _ block counters ->
      counters
      @ Fdo_annotation.to_list
          (Cfg.fdo_counters_of_terminator block.terminator.desc))

let print_cfg_terminators (cfg : Cfg.t) =
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
  let cfg =
    Cfg_with_layout.cfg
      (Selection.emit_fundecl ~future_funcnames:Misc.Stdlib.String.Set.empty
         fundecl)
  in
  let in_cfg = cfg_counters cfg in
  let dropped =
    Fdo_dropped_counters.registered ()
    |> Fdo_dropped_counters.to_list |> List.map fst
  in
  Format.printf "%s:@.%a@.  cfg:@." name Printcmm.expression body;
  Format.printf "    entry: %a@." Fdo_annotation.With_body_hash.print
    cfg.fun_fdo_entry_annotation;
  print_cfg_terminators cfg;
  print_counters "dropped" dropped;
  print_counters "lost"
    (List.filter
       (fun counter -> not (mem counter in_cfg || mem counter dropped))
       in_cmm);
  print_counters "added"
    (List.filter (fun counter -> not (mem counter in_cmm)) in_cfg);
  Format.printf "@."

let () =
  test "if then else" ~args:["x"] (function
    | [x] ->
      Cifthenelse
        ( Cop (Ccmpi Clt, [x; int 0], dbg),
          dbg,
          annots 1,
          int 3,
          dbg,
          annots 2,
          int 5,
          dbg )
    | _ -> assert false);
  test "float comparison" ~arg_type:Cmm.typ_float ~args:["x"; "y"] (function
    | [x; y] ->
      Cifthenelse
        ( Cop (Ccmpf (Float64, CFnlt), [x; y], dbg),
          dbg,
          annots 1,
          int 3,
          dbg,
          annots 2,
          int 5,
          dbg )
    | _ -> assert false);
  (* Values 0 and 2 share case 0: both entries of the switch have its
     counter. *)
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
          apply 1 [g; Cvar (Backend_var.With_provenance.var v)] )
    | _ -> assert false);
  test "tail calls" ~args:["x"; "g"] (function
    | [x; g] ->
      Cifthenelse
        ( Cop (Ccmpi Clt, [x; int 0], dbg),
          dbg,
          annots 1,
          direct_call 0 "h" [x],
          dbg,
          annots 2,
          apply 1 [g; x],
          dbg )
    | _ -> assert false);
  (* A self tail call becomes a jump, which drops its counter. *)
  test "self tail call" ~args:["x"] (function
    | [x] ->
      Cifthenelse
        ( Cop (Ccmpi Clt, [x; int 0], dbg),
          dbg,
          annots 1,
          direct_call 0 "f" [Cop (Caddi, [x; int 2], dbg)],
          dbg,
          annots 2,
          x,
          dbg )
    | _ -> assert false)
