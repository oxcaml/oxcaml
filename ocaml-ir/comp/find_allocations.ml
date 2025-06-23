open! Ocaml_ir_fiber
open! Sexplib.Std
open! Ocaml_ir_common.Std
open! Ocaml_ir_common
module Allocations = Ocaml_ir_common.Allocations
module Allocation = Allocations.Allocation
module Location = Ocaml_ir_common.Location
let create_location_adjuster ~file_content adjuster parse_tree =
  (let open Location_adjuster in
     default_for_fundecl ~file_content parse_tree adjuster);
  (let open Location_adjuster in
     default_for_call ~file_content parse_tree adjuster);
  (let open Location_adjuster in
     shrink_for_datatypes ~file_content parse_tree adjuster)
module Tracker =
  struct
    type t =
      {
      mutable basename: string ;
      mutable location_adjuster: Location_adjuster.t ;
      mutable allocations: Allocations.t }
    let t : t =
      {
        basename = "";
        location_adjuster = (Location_adjuster.create ());
        allocations = Allocations.empty
      }
    let reset ~basename =
      t.basename <- basename;
      t.location_adjuster <- (Location_adjuster.create ());
      t.allocations <- Allocations.empty
    let generate_location_adjuster ~file_content parse_tree =
      create_location_adjuster ~file_content t.location_adjuster parse_tree
    let to_location dbg =
      (Location.With_inlined_frames.of_debuginfo' dbg) |>
        (Location.With_inlined_frames.update_in_file
           ~f:(fun loc ->
                 Location_adjuster.map' ~from:loc t.location_adjuster))
    let open_function function_name =
      t.allocations <-
        (Allocations.open_function ~function_name t.allocations)
    let add_deadcode_eliminated ~name ~dbg =
      t.allocations <-
        (Allocations.notify_deadcode_eliminated ~name ~dbg t.allocations)
    let record ~context ~kind ~locality ~dbgs ~dbg =
      let rec aux dbg =
        match dbg with
        | { Debuginfo.dinfo_file = dinfo_file;_}::_ as dbg when
            String.equal dinfo_file t.basename ->
            Some
              (let open Location.Inferred in
                 { precision = Exact; loc = (to_location dbg) })
        | _::dbg -> aux dbg
        | [] ->
            List.fold_left
              ~f:(fun acc dbg ->
                    let dbg' = Debuginfo.Dbg.to_list (Debuginfo.get_dbg dbg) in
                    match acc with
                    | None when Debuginfo.is_none dbg -> None
                    | None when
                        String.equal (List.hd dbg').dinfo_file t.basename ->
                        Some
                          (let open Location.Inferred in
                             {
                               precision = Approximate;
                               loc = (to_location dbg')
                             })
                    | None -> None
                    | _ -> acc) ~init:None dbgs in
      t.allocations <-
        (Allocations.add
           (Allocations.Allocation.create ~context ~kind ~locality
              (aux (Debuginfo.Dbg.to_list (Debuginfo.get_dbg dbg))))
           t.allocations)
    let allocations () = t.allocations
  end
let collect_existing_values_in_parsetree (parsetree : Parsetree.structure) =
  let all_values = Hashtbl.create 32 in
  let iterator =
    {
      Ast_iterator.default_iterator with
      structure_item =
        (fun this e ->
           (match e.pstr_desc with
            | Pstr_value (_, bindings) ->
                List.iter bindings
                  ~f:(fun binding ->
                        match ((binding.Parsetree.pvb_pat).ppat_desc,
                                (binding.Parsetree.pvb_expr).pexp_desc)
                        with
                        | (Ppat_var sloc, Pexp_function _) ->
                            let loc_cmm =
                              {
                                ((binding.pvb_expr).pexp_loc) with
                                loc_ghost = false
                              } in
                            (match Hashtbl.find_opt all_values loc_cmm with
                             | None ->
                                 Hashtbl.add all_values loc_cmm
                                   [(sloc.txt, binding.pvb_loc)]
                             | Some f ->
                                 Hashtbl.replace all_values loc_cmm
                                   ((sloc.txt, binding.pvb_loc) :: f))
                        | _ -> ())
            | _ -> ());
           Ast_iterator.default_iterator.structure_item this e)
    } in
  iterator.structure iterator parsetree; all_values
let find_deadcode ~known_to_exist_in_parsetree ~known_to_exist_in_allocs cmm
  =
  List.iter cmm
    ~f:(function
        | Cmm.Cfunction { fun_name; fun_dbg;_} ->
            let demangled_name =
              match List.rev
                      (String.split_on_char ~sep:'_' fun_name.sym_name)
              with
              | "code"::_::_::tl -> String.concat ~sep:"_" (List.rev tl)
              | _ -> fun_name.sym_name in
            if Hashtbl.mem known_to_exist_in_allocs fun_name.sym_name
            then
              let loc_dbg =
                { (Debuginfo.to_location fun_dbg) with loc_ghost = false } in
              (match Hashtbl.find_opt known_to_exist_in_parsetree loc_dbg
               with
               | Some candidates ->
                   let candidates =
                     List.filter
                       ~f:(fun (name, _) ->
                             not
                               (String.ends_with ~suffix:name demangled_name))
                       candidates in
                   if List.is_empty candidates
                   then Hashtbl.remove known_to_exist_in_parsetree loc_dbg
                   else
                     Hashtbl.replace known_to_exist_in_parsetree loc_dbg
                       candidates
               | None -> ())
        | _ -> ());
  (((Hashtbl.to_seq_values known_to_exist_in_parsetree) |>
      (Seq.map List.to_seq))
     |> Seq.concat)
    |> List.of_seq
let find_local_allocations_in_op op (dbg : Debuginfo.t) =
  match op with
  | Cmm.Calloc (Cmm.Alloc_mode.Local, _block_kind) ->
      if (Debuginfo.Dbg.length (Debuginfo.get_dbg dbg)) = 1
      then
        Tracker.record ~context:Direct ~locality:Local ~kind:Ocaml ~dbgs:[]
          ~dbg
      else ()
  | Cmm.Calloc (Cmm.Alloc_mode.Heap, _block_kind) -> ()
  | _ -> ()
let rec find_local_allocations_in_expression expression =
  let f = find_local_allocations_in_expression in
  match expression with
  | Cmm.Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_float32 _
  | Cconst_symbol _ | Cconst_vec128 _ | Cvar _ | Cconst_vec256 _ |Cconst_vec512 _ -> ()
  | Clet (_, let_expr, in_expr) -> (f let_expr; f in_expr)
  | Cphantom_let (_, _, expr) -> f expr
  | Ctuple exprs -> List.iter ~f exprs
  | Cop (Craise _, exprs, _) -> List.iter ~f exprs
  | Cop (op, exprs, op_dbg) ->
      (find_local_allocations_in_op op op_dbg; List.iter ~f exprs)
  | Csequence (expr, expr') -> (f expr; f expr')
  | Cifthenelse (cond, _dbg_cond, if_, _dbg_if, else_, _dbg_else) ->
      (f cond; f if_; f else_)
  | Cswitch (cond, _, branches, _dbg) ->
      (f cond;
       (Array.to_list branches) |> (List.iter ~f:(fun (expr, _) -> f expr)))
  | Ccatch (_, branches, default_) ->
      (List.iter ~f:(fun (_, _, expr, _dbg, _) -> f expr) branches;
       f default_)
  | Cexit (_, exprs, _) -> List.iter ~f exprs
let find_local_allocations_in_phrase phrase =
  match phrase with
  | Cmm.Cdata _ -> ()
  | Cfunction { fun_name; fun_body;_} ->
      (Tracker.open_function fun_name.sym_name;
       find_local_allocations_in_expression fun_body)
let find_allocations_using_hooks all_allocations =
  let known_to_exist_in_allocs = Hashtbl.create 32 in
  List.iter
    ~f:(fun (name, { Zero_alloc_checker.Witnesses.nor = nor; exn; div }) ->
          let convert =
            function
            | Zero_alloc_checker.Witness.Extcall { callee } ->
                Allocation.Ccall callee
            | _ -> Ocaml in
          Hashtbl.add known_to_exist_in_allocs name ();
          Tracker.open_function name;
          (let record context kind dbg =
             Tracker.record ~dbgs:[] ~kind:(convert kind) ~context
               ~locality:Global ~dbg in
           Zero_alloc_checker.Witnesses.iter nor
             ~f:(fun { dbg; kind } -> record Direct kind dbg);
           Zero_alloc_checker.Witnesses.iter exn
             ~f:(fun { dbg; kind } -> record Raise kind dbg);
           Zero_alloc_checker.Witnesses.iter div
             ~f:(fun { dbg; kind } -> record Direct kind dbg);
           ())) (!all_allocations);
  known_to_exist_in_allocs
let find_allocations ~filename ~output_prefix ~compiler_flags =
  let parse_tree = ref None in
  let cmm = ref None in
  let languages = Language.Set.singleton Cmm in
  Fiber.Let_syntax.Let_syntax.bind (Fiber.Io.read_file ~path:filename)
    ~f:(fun file_content ->
          let basename = Filename.basename filename in
          let all_allocations = ref [] in
          Tracker.reset ~basename;
          Result.Fiber.Let_syntax.Let_syntax.bind
            (Compile.invoke_compiler ~filename ~output_prefix
               ~compiler_flags:("-zero-alloc-checker-details-cutoff" :: "-1"
               :: compiler_flags) ~languages
               ~setup_hooks:(fun () ->
                               Compiler_hooks.register
                                 Compiler_hooks.Check_allocations
                                 (fun witnesses ->
                                    witnesses
                                      (fun name x ->
                                         if
                                           String.ends_with ~suffix:"__entry"
                                             name
                                         then ()
                                         else
                                           all_allocations := ((name, x) ::
                                             (!all_allocations))));
                               Compiler_hooks.register Compiler_hooks.Cmm
                                 (fun tree -> cmm := (Some tree));
                               Compiler_hooks.register
                                 Compiler_hooks.Parse_tree_impl
                                 (fun tree -> parse_tree := (Some tree))))
            ~f:(fun () ->
                  Tracker.generate_location_adjuster ~file_content
                    (!parse_tree);
                  (let known_to_exist_in_check_allocs =
                     find_allocations_using_hooks all_allocations in
                   (match ((!parse_tree), (!cmm)) with
                    | (Some parse_tree, Some cmm) ->
                        let known_to_exist_in_parsetree =
                          collect_existing_values_in_parsetree parse_tree in
                        let _deadcode =
                          find_deadcode ~known_to_exist_in_parsetree
                            ~known_to_exist_in_allocs:known_to_exist_in_check_allocs
                            cmm in
                        (List.iter ~f:find_local_allocations_in_phrase cmm;
                         List.iter
                           ~f:(fun (name, dbg) ->
                                 Tracker.add_deadcode_eliminated ~name
                                   ~dbg:(let open Location in
                                           {
                                             precision = Exact;
                                             loc =
                                               (With_inlined_frames.of_simple
                                                  (Location.Simple.of_warnings_loc
                                                     dbg))
                                           })) [])
                    | _ -> ());
                   Result.Fiber.return (Tracker.allocations ()))))
module V1 =
  struct
    let run ~filename ~output_prefix ~compiler_flags =
      let extension = Filename.extension filename in
      match File_type.from_extension extension with
      | Ok (Ml) -> find_allocations ~filename ~compiler_flags ~output_prefix
      | Ok (Mli) | Error _ ->
          Fiber.return Result.unsupported_source_file_extension
  end
