(* This program is a simple command-line tool to manipulate data stored in
   .cmir-cfg-regalloc files. *)

open! Int_replace_polymorphic_compare
module List = ListLabels

let fatal : ('a, Format.formatter, unit, unit, unit, _) format6 -> 'a =
 fun fmt ->
  Format.kfprintf
    (fun _ -> exit 1)
    Format.err_formatter
    ("*** error: " ^^ fmt ^^ "\n%!")

let iter_cfg_before_regalloc file ~f =
  let unit_info, _digest = Cfg_format.restore file in
  List.iter unit_info.items ~f:(fun (item : Cfg_format.cfg_item_info) ->
      begin
        match item with
        | Cfg _ -> ()
        | Data _ -> ()
        | Cfg_before_regalloc
            { cfg_with_layout_and_relocatable_regs; cmm_label; reg_stamp } ->
          let cfg_with_layout, relocatable_regs =
            cfg_with_layout_and_relocatable_regs
          in
          f ~cfg_with_layout ~relocatable_regs ~cmm_label ~reg_stamp
      end)

let apply_cfg_before_regalloc file function_ ~f =
  let exception Found in
  try
    iter_cfg_before_regalloc file
      ~f:(fun ~cfg_with_layout ~relocatable_regs ~cmm_label ~reg_stamp ->
        let cfg = Cfg_with_layout.cfg cfg_with_layout in
        if String.equal cfg.fun_name function_
        then begin
          f ~cfg_with_layout ~relocatable_regs ~cmm_label ~reg_stamp;
          raise Found
        end);
    fatal "did not find %S in %S" function_ file
  with Found -> ()

let list files =
  let process_file file =
    Format.printf "Functions in %S\n%!" file;
    iter_cfg_before_regalloc file
      ~f:(fun ~cfg_with_layout ~relocatable_regs ~cmm_label:_ ~reg_stamp:_ ->
        let cfg = Cfg_with_layout.cfg cfg_with_layout in
        Format.printf "- %s (%d blocks, %d registers)\n%!" cfg.fun_name
          (Label.Tbl.length cfg.blocks)
          (List.length relocatable_regs))
  in
  List.iter files ~f:process_file

(* note: this is the path on macOS; we use the full path to avoid using another
   tool in the PATH. *)
let open_tool_path = "/usr/bin/open"

(* note: for graphviz, we simply expect to have the tool in the PATH.*)
let dot_tool = "dot"

let run_command cmd =
  match Sys.command cmd with
  | 0 -> ()
  | exit_code -> fatal "command %S exited with code %d" cmd exit_code

let output_cfg cfg_with_layout ~function_ ~output =
  let prefix = "cfg_tool" in
  let dot_file, pdf_file =
    match Filename.extension output with
    | ".dot" -> output, None
    | ".pdf" -> Filename.temp_file prefix ".dot", Some (output, false)
    | "" ->
      ( Filename.temp_file prefix ".dot",
        Some (Filename.temp_file prefix ".pdf", true) )
    | extension -> fatal "unknown file extension (%S)" extension
  in
  Cfg_with_layout.save_as_dot ~filename:dot_file cfg_with_layout function_;
  match pdf_file with
  | None -> ()
  | Some (pdf_file, open_file) ->
    run_command (Printf.sprintf "%s -Tpdf -o %S %S" dot_tool pdf_file dot_file);
    if open_file
    then begin
      if Sys.file_exists open_tool_path
      then run_command (Printf.sprintf "%s %S" open_tool_path pdf_file)
      else fatal "%S does not exist" open_tool_path
    end

let run_and_print_dominators cfg_with_infos =
  (* CR-soon xclerc for xclerc: share printing scode with
     `Regalloc_split_utils`. *)
  let doms = Cfg_with_infos.dominators cfg_with_infos in
  let rec print_dom_tree depth (tree : Cfg_dominators.dominator_tree) =
    for _ = 1 to depth do
      Format.printf "  "
    done;
    Format.printf ". %a\n%!" Label.print tree.label;
    List.iter tree.children ~f:(fun child -> print_dom_tree (succ depth) child)
  in
  Format.printf "dominator forest:\n%!";
  List.iter (Cfg_dominators.dominator_forest doms) ~f:(fun dom_tree ->
      print_dom_tree 0 dom_tree)

let run_and_print_loop_infos cfg_with_infos =
  let loop_infos = Cfg_with_infos.loop_infos cfg_with_infos in
  Format.printf "back edges:\n%!";
  Cfg_edge.Set.iter
    (fun { Cfg_edge.src; dst } ->
      Format.printf "- %a -> %a\n%!" Label.print src Label.print dst)
    loop_infos.back_edges;
  Format.printf "loops:\n%!";
  Label.Map.iter
    (fun header loops ->
      Format.printf "- header %a:\n%!" Label.print header;
      List.iteri loops ~f:(fun idx loop ->
          Format.printf "  - loop %d\n%!" idx;
          Label.Set.iter
            (fun label -> Format.printf "    - %a\n%!" Label.print label)
            loop))
    loop_infos.header_map

let extract ~file ~function_ ~output ~run_invariants ~run_dominators
    ~run_loop_infos =
  apply_cfg_before_regalloc file function_
    ~f:(fun ~cfg_with_layout ~relocatable_regs:_ ~cmm_label:_ ~reg_stamp:_ ->
      if run_invariants
      then begin
        match Cfg_invariants.run Format.std_formatter cfg_with_layout with
        | false -> ()
        | true -> fatal "invariants failed"
      end;
      let cfg_with_infos = Cfg_with_infos.make cfg_with_layout in
      if run_dominators then run_and_print_dominators cfg_with_infos;
      if run_loop_infos then run_and_print_loop_infos cfg_with_infos;
      output_cfg cfg_with_layout ~function_ ~output)

let commands =
  [ ( "list",
      begin
        fun () ->
          let files = ref [] in
          let anonymous file = files := file :: !files in
          Arg.parse_argv ~current:(ref 1) Sys.argv [] anonymous
            "List all the CFGs store in the passed files";
          list (List.rev !files)
      end );
    ( "extract",
      begin
        fun () ->
          let file = ref "" in
          let function_ = ref "" in
          let output = ref "" in
          let run_invariants = ref false in
          let run_dominators = ref false in
          let run_loop_infos = ref false in
          let anonymous _ =
            fatal
              "the \"extract\" command does not support anomymous command-line \
               arguments"
          in
          Arg.parse_argv ~current:(ref 1) Sys.argv
            [ ( "-file",
                Arg.Set_string file,
                " Path of the file to extract the CFG from" );
              ( "-function",
                Arg.Set_string function_,
                " Name of the function to extract" );
              ( "-output",
                Arg.Set_string output,
                " Path of the file to save the CFG to" );
              "-run-invariants", Arg.Set run_invariants, " Run CFG invariants";
              ( "-run-dominators",
                Arg.Set run_dominators,
                " Run and output CFG dominators" );
              ( "-run-loop-infos",
                Arg.Set run_loop_infos,
                " Run and ouput CFG loop infos" ) ]
            anonymous "Extract one CFG from a file";
          extract ~file:!file ~function_:!function_ ~output:!output
            ~run_invariants:!run_invariants ~run_dominators:!run_dominators
            ~run_loop_infos:!run_loop_infos
      end ) ]

let () =
  if Array.length Sys.argv < 2
  then
    let commands =
      commands |> List.map ~f:fst
      |> List.sort ~cmp:String.compare
      |> String.concat ", "
    in
    fatal "program expects a command\n    (available commands: %s)" commands
  else
    let command_name = Sys.argv.(1) in
    match List.assoc_opt command_name commands with
    | None -> fatal "unknown command %S" command_name
    | Some command_func -> command_func ()
