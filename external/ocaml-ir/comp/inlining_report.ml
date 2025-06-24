open! Ocaml_ir_fiber
open! Sexplib.Std
open! Ocaml_ir_common.Std
open! Ocaml_ir_common
module Ir = Flambda2_simplify_shared.Inlining_report
module OCaml_tree = Ir.Inlining_tree
module OCaml_path = Flambda2_terms.Inlining_history.Absolute
type location_adjusters =
  {
  calls: Location_adjuster.t ;
  fundecls: Location_adjuster.t ;
  scopes: Location_adjuster.t }
let create_location_adjusters ~file_content
  (parse_tree : Parsetree.structure option) =
  let t =
    {
      calls = (Location_adjuster.create ());
      fundecls = (Location_adjuster.create ());
      scopes = (Location_adjuster.create ())
    } in
  Location_adjuster.default_for_call ~file_content parse_tree t.calls;
  Location_adjuster.default_for_call ~file_content parse_tree t.fundecls;
  t
let decision_description decision =
  Ir.Decision_with_context.print Format.str_formatter decision;
  Format.flush_str_formatter ()
let convert_dbg ~location_adjuster dbg =
  let loc = Debuginfo.to_location dbg in
  (Location_adjuster.map ~from:loc location_adjuster) |>
    Location.Simple.of_warnings_loc
let rec convert_path ~location_adjusters (ocaml_path : OCaml_path.t)  :
  Inlining_report.Path.t=
  let cu_name =
    (OCaml_path.compilation_unit ocaml_path) |>
      Compilation_unit.full_path_as_string in
  let rec aux ocaml_path =
    match ocaml_path with
    | OCaml_path.Empty -> Inlining_report.Path.empty cu_name
    | Unknown { prev } ->
        Inlining_report.Path.add (aux prev) Inlining_report.Path.Unknown
    | Function { name; dbg; prev } ->
        Inlining_report.Path.add (aux prev)
          (Fundecl
             {
               name;
               loc =
                 (convert_dbg
                    ~location_adjuster:(location_adjusters.fundecls) dbg)
             })
    | Call { callee; dbg; prev } ->
        Inlining_report.Path.add (aux prev)
          (Call
             {
               fn = (convert_path ~location_adjusters callee);
               loc =
                 (convert_dbg ~location_adjuster:(location_adjusters.calls)
                    dbg)
             })
    | Inline { prev } -> aux prev
    | Module { name; prev } ->
        Inlining_report.Path.add (aux prev) (Module name)
    | Class { name; prev } ->
        Inlining_report.Path.add (aux prev) (Class name) in
  aux (OCaml_path.path ocaml_path)
let fundecl_decision decision =
  match (decision : Ir.Decision_with_context.t) with
  | { context = _; decision = Fundecl ocaml_fundecl_decision } as decision ->
      let decision_description = decision_description decision in
      let decision : Inlining_report.Fundecl_decision.t =
        if
          Flambda2_terms.Function_decl_inlining_decision_type.must_be_inlined
            ocaml_fundecl_decision
        then Must_be_inlined
        else
          if
            Flambda2_terms.Function_decl_inlining_decision_type.cannot_be_inlined
              ocaml_fundecl_decision
          then Cannot_be_inlined
          else Inlinable in
      (decision_description, decision)
  | _ ->
      failwith
        "Conversion error: decision attached to a fundecl wasn't a fundecl decision"
let call_decision ~location_adjusters decision  :
  (string * Inlining_report.Call_decision.t)=
  match (decision : Ir.Inlining_tree.decision_or_reference) with
  | Reference path ->
      ("", (Reference (convert_path ~location_adjusters path)))
  | Unavailable -> ("", Unavailable)
  | Decision
      ({ context = _; decision = Call ocaml_call_decision } as decision) ->
      let decision_description = decision_description decision in
      let decision : Inlining_report.Call_decision.t =
        match Flambda2_simplify_shared.Call_site_inlining_decision_type.can_inline
                ocaml_call_decision
        with
        | Inline _ -> Inlined
        | Do_not_inline _ -> Not_inlined in
      (decision_description, decision)
  | _ ->
      failwith
        "Conversion error: decision attached to a call wasn't a call decision"
let rec convert_inlining_tree ~location_adjusters (ocaml_tree : OCaml_tree.t)
  =
  OCaml_tree.Map.fold
    (fun key item tree ->
       let (dbg, key) = key in
       let loc =
         match key with
         | Call _ ->
             convert_dbg ~location_adjuster:(location_adjusters.calls) dbg
         | Fundecl _ ->
             convert_dbg ~location_adjuster:(location_adjusters.fundecls) dbg
         | Scope _ ->
             convert_dbg ~location_adjuster:(location_adjusters.scopes) dbg in
       match ((key : OCaml_tree.Key.element), (item : OCaml_tree.item)) with
       | (Scope (Module, name), Scope ocaml_tree) ->
           let next_tree =
             convert_inlining_tree ~location_adjusters ocaml_tree in
           Inlining_report.Tree.insert ~key:(Module name)
             ~element:(Scope next_tree) tree
       | (Scope (Class, name), Scope ocaml_tree) ->
           let next_tree =
             convert_inlining_tree ~location_adjusters ocaml_tree in
           Inlining_report.Tree.insert ~key:(Class name)
             ~element:(Scope next_tree) tree
       | (Scope (Unknown, _), Scope ocaml_tree) ->
           let next_tree =
             convert_inlining_tree ~location_adjusters ocaml_tree in
           Inlining_report.Tree.insert ~key:Unknown
             ~element:(Scope next_tree) tree
       | (Fundecl name, Fundecl { decisions = decision::_; body }) ->
           let next_tree = convert_inlining_tree ~location_adjusters body in
           let (decision_description, decision) = fundecl_decision decision in
           Inlining_report.Tree.insert ~key:(Fundecl { name; loc })
             ~element:(Fundecl
                         { decision; decision_description; body = next_tree })
             tree
       | (Call fct, Call { decision; tree = ocaml_tree }) ->
           let next_tree =
             convert_inlining_tree ~location_adjusters ocaml_tree in
           let (decision_description, decision) =
             call_decision ~location_adjusters decision in
           let fn = convert_path ~location_adjusters fct in
           Inlining_report.Tree.insert ~key:(Call { fn; loc })
             ~element:(Call
                         {
                           decision;
                           decision_description;
                           inlined = next_tree
                         }) tree
       | _ ->
           failwith
             "Conversion error: saw mismatching key and item in inlining report")
    ocaml_tree Inlining_report.Tree.empty
let find_inlining_report_flambda2 ~filename ~output_prefix ~compiler_flags
  ~as_org_mode =
  let ir = ref None in
  let parse_tree : Parsetree.structure option ref = ref None in
  let languages = Language.Set.singleton Inlining_report in
  let compiler_flags = "-inlining-report" :: compiler_flags in
  Result.Fiber.Let_syntax.Let_syntax.bind
    (Compile.invoke_compiler ~filename ~output_prefix ~compiler_flags
       ~languages
       ~setup_hooks:(fun () ->
                       Compiler_hooks.register Compiler_hooks.Parse_tree_impl
                         (fun p -> parse_tree := (Some p));
                       Compiler_hooks.register Compiler_hooks.Inlining_tree
                         (fun ir_file -> ir := (Some ir_file))))
    ~f:(fun () ->
          match !ir with
          | None ->
              Fiber.return
                (Result.ocaml_ir_comp_error
                   (Error.of_string
                      "[find_inlining_report]: ocaml_ir_comp wasn't able to get the inlining report. Are you sure you're using Flambda2?"))
          | Some ir ->
              if as_org_mode
              then
                Fiber.Let_syntax.Let_syntax.bind
                  (Fiber.Io.read_file
                     ~path:(output_prefix ^ ".0.inlining.org"))
                  ~f:(fun content ->
                        Result.Fiber.return
                          (Inlining_report.Org_mode { content }))
              else
                Fiber.Let_syntax.Let_syntax.bind
                  (Fiber.Io.read_file ~path:filename)
                  ~f:(fun file_content ->
                        let location_adjusters =
                          create_location_adjusters ~file_content
                            (!parse_tree) in
                        Result.Fiber.return
                          (Inlining_report.Tree
                             (convert_inlining_tree ~location_adjusters ir))))
let get_all_calls_in_cmm_expression calls expr =
  let calls = ref calls in
  let rec aux expr =
    (match expr with
     | Cmm.Cop ((Capply _ | Cextcall _), _, dbg) ->
         calls :=
           (Location.Simple.Map.add (Location.Simple.of_debuginfo dbg) ()
              (!calls))
     | _ -> ());
    Cmm.iter_shallow aux expr in
  aux expr; !calls
let get_all_calls_in_cmm_phrase calls phrase =
  match phrase with
  | Cmm.Cdata _ -> calls
  | Cmm.Cfunction { fun_body;_} ->
      get_all_calls_in_cmm_expression calls fun_body
let get_all_calls_in_cmm cmm =
  List.fold_left ~f:get_all_calls_in_cmm_phrase
    ~init:Location.Simple.Map.empty cmm
let get_all_calls_in_parse_tree parse_tree =
  let calls = ref Location.Simple.Map.empty in
  let iterator =
    {
      Ast_iterator.default_iterator with
      expr =
        (fun this e ->
           (match e.pexp_desc with
            | Pexp_apply _ ->
                calls :=
                  (Location.Simple.Map.add
                     (Location.Simple.of_warnings_loc e.pexp_loc) () (
                     !calls))
            | _ -> ());
           Ast_iterator.default_iterator.expr this e)
    } in
  iterator.structure iterator parse_tree; !calls
let infer_inlining_tree ~cmm ~parse_tree =
  let all_non_inlined = get_all_calls_in_cmm cmm in
  let all_inlined = get_all_calls_in_parse_tree parse_tree in
  Location.Simple.Map.fold
    (fun loc () tree ->
       let element =
         if Location.Simple.Map.mem loc all_non_inlined
         then
           Inlining_report.Tree.Call
             {
               decision_description = "";
               decision = Not_inlined;
               inlined = Inlining_report.Tree.empty
             }
         else
           Inlining_report.Tree.Call
             {
               decision_description = "";
               decision = Inlined;
               inlined = Inlining_report.Tree.empty
             } in
       let key =
         Inlining_report.Path.Call
           { fn = (Inlining_report.Path.empty ""); loc } in
       Inlining_report.Tree.insert ~key ~element tree) all_inlined
    Inlining_report.Tree.empty
let find_inlining_report_infer ~filename ~output_prefix ~compiler_flags
  ~as_org_mode =
  let parse_tree = ref None in
  let cmm = ref None in
  let languages = Language.Set.singleton Inlining_report in
  Result.Fiber.Let_syntax.Let_syntax.bind
    (Compile.invoke_compiler ~filename ~output_prefix ~compiler_flags
       ~languages
       ~setup_hooks:(fun () ->
                       Compiler_hooks.register Compiler_hooks.Parse_tree_impl
                         (fun ir -> parse_tree := (Some ir));
                       Compiler_hooks.register Compiler_hooks.Cmm
                         (fun ir -> cmm := (Some ir))))
    ~f:(fun () ->
          match ((!parse_tree), (!cmm)) with
          | (None, _) | (_, None) ->
              Fiber.return
                (Result.ocaml_ir_comp_error
                   (Error.of_string
                      "[find_inlining_report]: ocaml_ir_comp encountered an unexpected error\n while inferring inlining reports. Are you sure you passed an *.ml file?"))
          | (Some parse_tree, Some cmm) ->
              if as_org_mode
              then
                Fiber.return
                  (Result.ocaml_ir_comp_error
                     (Error.of_string
                        "[find_inlining_report]: org mode inlining reports are only available with\nFlambda2."))
              else
                Result.Fiber.return
                  (Inlining_report.Tree
                     (infer_inlining_tree ~cmm ~parse_tree)))
module V1 =
  struct
    let run ~filename ~output_prefix ~compiler_flags ~as_org_mode =
      let extension = Filename.extension filename in
      match File_type.from_extension extension with
      | Ok (Ml) ->
          if Config.flambda2
          then
            (try
               find_inlining_report_flambda2 ~filename ~compiler_flags
                 ~output_prefix ~as_org_mode
             with | Failure error -> Fiber.return (Result.compiler_exn error))
          else
            (try
               find_inlining_report_infer ~filename ~compiler_flags
                 ~output_prefix ~as_org_mode
             with | Failure error -> Fiber.return (Result.compiler_exn error))
      | Ok (Mli) | Error _ ->
          Fiber.return Result.unsupported_source_file_extension
  end
