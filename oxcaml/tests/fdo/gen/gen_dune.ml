(* Generates dune.inc, the rules of the FDO tests. After changing this file, run
   the tests and promote dune.inc. *)

(* {2 S-expressions} *)

type sexp =
  | Atom of string
  | List of sexp list

(* A list is on one line if it fits. Else, a list of atoms fills its lines, and
   any other list has its first element on the line of the parenthesis and each
   other element on a line of its own. *)
let rec print ppf = function
  | Atom s -> Format.pp_print_string ppf s
  | List l ->
    let all_atoms =
      List.for_all (function Atom _ -> true | List _ -> false) l
    in
    Format.fprintf ppf
      (if all_atoms then "@[<hov 1>(%a)@]" else "@[<hv 1>(%a)@]")
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print)
      l

let atom s = Atom s

let atoms = List.map atom

let field name args = List (Atom name :: args)

(* {2 Tests} *)

type compile =
  { flags : string list;
    output : string option;
    source : string
  }

let compile ?output flags source = { flags; output; source }

type step =
  | Annotations of string list  (** A command of [tools/fdo_annotations.exe]. *)
  | Cat of string

type test =
  | Unit of
      { name : string;
        libraries : string
      }  (** An executable whose output is compared with [<name>.output]. *)
  | Dumps of
      { name : string;
        compiles : compile list;
        steps : step list;
        linux_only : bool
      }
      (** Compiles sources with dumps, reads their FDO annotations with
          [tools/fdo_annotations.exe], and compares its output with
          [<name>.output]. *)
  | Codegen of
      { sources : string list;
        configs : (string * string list) list
      }
      (** Compiles each source with each named set of flags, with and without
          counters, and checks that the code is the same. *)

let enabled_if ~linux_only =
  let main = List (atoms ["="; "%{context_name}"; "\"main\""]) in
  field "enabled_if"
    [ (if linux_only
       then List [Atom "and"; main; List (atoms ["="; "%{system}"; "\"linux\""])]
       else main) ]

let diff ~linux_only name =
  let output = name ^ ".output" and corrected = name ^ ".output.corrected" in
  field "rule"
    [ field "alias" [Atom "runtest"];
      enabled_if ~linux_only;
      field "deps" (atoms [output; corrected]);
      field "action" [field "diff" (atoms [output; corrected])] ]

let compile_action { flags; output; source } =
  let run =
    field "run"
      (atoms
         (["%{bin:ocamlopt.opt}"; "-c"]
         @ flags @ ["-color"; "never"]
         @ (match output with None -> [] | Some output -> ["-o"; output])
         @ [source]))
  in
  (* The dumps are read from next to the .cmx files, so OCAMLPARAM is cleared:
     CI's metrics job sets dump-dir in it, which moves them. *)
  if List.mem "-dump-into-file" flags
  then field "setenv" [Atom "OCAMLPARAM"; Atom "\"\""; run]
  else run

let mkdir = function
  | [] -> []
  | dirs -> [field "run" (atoms ("mkdir" :: "-p" :: dirs))]

let stanzas = function
  | Unit { name; libraries } ->
    [ field "executable"
        [ field "name" [Atom name];
          field "modules" [Atom name];
          field "libraries" [Atom libraries] ];
      field "rule"
        [ enabled_if ~linux_only:false;
          field "action"
            [ field "with-stdout-to"
                [ Atom (name ^ ".output.corrected");
                  field "run" [Atom ("./" ^ name ^ ".exe")] ] ] ];
      diff ~linux_only:false name ]
  | Codegen { sources; configs } ->
    List.concat_map
      (fun (config, flags) ->
        List.concat_map
          (fun source ->
            let module_ = Filename.chop_extension source in
            let dir counters = "codegen_" ^ config ^ "_" ^ counters in
            let code counters =
              String.concat "." [module_; config; counters; "code"]
            in
            let build counters counter_flags =
              let output ext = dir counters ^ "/" ^ module_ ^ ext in
              [ compile_action
                  (compile ~output:(output ".cmx") (flags @ counter_flags)
                     source);
                field "with-stdout-to"
                  [ Atom (code counters);
                    field "pipe-stdout"
                      [ field "run" (atoms ["objdump"; "-dr"; output ".o"]);
                        field "run" [Atom "%{dep:tools/filter_objdump.sh}"] ] ]
              ]
            in
            [ field "rule"
                [ field "targets" (atoms [code "without"; code "with"]);
                  field "deps" [Atom source];
                  enabled_if ~linux_only:true;
                  field "action"
                    [ field "progn"
                        (mkdir [dir "without"; dir "with"]
                        @ build "without" []
                        @ build "with" ["-fdo-counters"]) ] ];
              field "rule"
                [ field "alias" [Atom "runtest"];
                  enabled_if ~linux_only:true;
                  field "action"
                    [field "diff" (atoms [code "without"; code "with"])] ] ])
          sources)
      configs
  | Dumps { name; compiles; steps; linux_only } ->
    let dedup l =
      List.rev
        (List.fold_left
           (fun acc x -> if List.mem x acc then acc else x :: acc)
           [] l)
    in
    let sources = dedup (List.map (fun { source; _ } -> source) compiles) in
    let dirs =
      dedup
        (List.filter_map
           (fun { output; _ } ->
             match Option.map Filename.dirname output with
             | None | Some "." -> None
             | Some dir -> Some dir)
           compiles)
    in
    let step = function
      | Annotations args ->
        field "run" (atoms ("%{exe:tools/fdo_annotations.exe}" :: args))
      | Cat file -> field "run" (atoms ["cat"; file])
    in
    let steps =
      match List.map step steps with
      | [step] -> step
      | steps -> field "progn" steps
    in
    [ field "rule"
        [ field "targets" [Atom (name ^ ".output.corrected")];
          field "deps" (atoms sources);
          enabled_if ~linux_only;
          field "action"
            [ field "progn"
                (mkdir dirs
                @ List.map compile_action compiles
                @ [ field "with-stdout-to"
                      [Atom (name ^ ".output.corrected"); steps] ]) ] ];
      diff ~linux_only name ]

let unit name = Unit { name; libraries = "ocamloptcomp" }

let dumps ?(linux_only = false) name compiles steps =
  Dumps { name; compiles; steps; linux_only }

let list file = Annotations ["list"; file]

let dropped file = Annotations ["dropped"; file]

let compare_sets a b = Annotations ["compare-sets"; a; b]

let check a b = Annotations ["check"; a; b]

(* Compiles [source] with the fexpr dump after [pass], whose annotations
   [fdo_annotations.exe] reads. [flags] come first, [dumps] last. *)
let fexpr ?output ?(flags = []) ?(dumps = []) ~pass source =
  compile ?output
    (flags
    @ [ "-fdo-counters";
        "-fexpr-print-fdo-counters";
        "-fexpr-print-branch-annotations";
        "-dflambda-heavy-invariants";
        "-dfexpr-annot-after";
        pass ]
    @ dumps)
    source

(* For each of [configs], a suffix and flags, compiles each of [sources] once
   for each of [stages], a name and dump flags, into [<name><suffix>_<stage>].
   Then, for each config and source, runs [checks ~dump source], where [dump
   stage] is the dump of the source at that stage. *)
let pipeline ?(configs = ["", []]) name ~sources ~stages checks =
  let dir suffix stage = name ^ suffix ^ "_" ^ stage in
  let module_ source = Filename.chop_extension source in
  let for_each f =
    List.concat_map
      (fun (suffix, flags) -> List.concat_map (f suffix flags) sources)
      configs
  in
  dumps name
    (for_each (fun suffix flags source ->
         List.map
           (fun (stage, dump_flags) ->
             compile
               ~output:(dir suffix stage ^ "/" ^ module_ source ^ ".cmx")
               (flags @ ["-fdo-counters"] @ dump_flags @ ["-dump-into-file"])
               source)
           stages))
    (for_each (fun suffix _ source ->
         checks
           ~dump:(fun stage ->
             dir suffix stage ^ "/" ^ module_ source ^ ".cmx.dump")
           source))

let tests =
  [ Unit { name = "fdo_counters"; libraries = "ocamlcommon" };
    unit "cmm_switch_counters";
    unit "selection_counters";
    unit "simplify_terminator_counters";
    unit "ssa_counters";
    unit "cfg_to_linear_counters";
    unit "branch_relaxation_counters";
    unit "merge_blocks_counters";
    dumps "function_ids"
      [ fexpr ~pass:"raw" "function_ids.ml";
        fexpr ~output:"function_ids.edited.cmx" ~pass:"raw"
          "function_ids_edited.ml" ]
      [ Annotations
          [ "compare-entries";
            "function_ids.raw.fl";
            "function_ids.edited.raw.fl" ] ];
    dumps "body_hash"
      [ fexpr ~pass:"raw" "body_hash.ml";
        fexpr ~output:"body_hash.edited.cmx" ~pass:"raw" "body_hash_edited.ml"
      ]
      [ Annotations
          ["compare-entries"; "body_hash.raw.fl"; "body_hash.edited.raw.fl"];
        Annotations
          ["compare-counters"; "body_hash.raw.fl"; "body_hash.edited.raw.fl"] ];
    dumps "instrumentation"
      [fexpr ~pass:"raw" "instrumentation.ml"]
      [list "instrumentation.raw.fl"];
    (* Compilation stops after the middle end, so that only its dropped counters
       are listed. *)
    dumps "simplify"
      [ fexpr ~pass:"simplify"
          ~dumps:
            [ "-dfdo-dropped-counters";
              "-dump-into-file";
              "-stop-after";
              "middle_end" ]
          "simplify.ml" ]
      [list "simplify.simplify.fl"; Cat "simplify.cmx.dump"];
    (* The same at -O3 with the reaper, in a directory of its own so that the
       module keeps its name. Without local value slots, which CI may enable by
       default and which remove the code of [R.step]. *)
    dumps "simplify_o3_reaper"
      [ fexpr ~output:"o3_reaper/simplify.cmx"
          ~flags:["-O3"; "-flambda2-reaper"; "-no-reaper-local-fields"]
          ~pass:"reaper"
          ~dumps:
            [ "-dfdo-dropped-counters";
              "-dump-into-file";
              "-stop-after";
              "middle_end" ]
          "simplify.ml" ]
      [list "o3_reaper/simplify.reaper.fl"; Cat "o3_reaper/simplify.cmx.dump"];
    (* [to_cmm.ml] is also compiled with the inline fast path of [caml_apply],
       whose two calls share the counter, and [simplify.ml] at -O3 with the
       reaper. *)
    (let cmm = ["-dcmm"; "-dump-into-file"] in
     dumps "to_cmm"
       [ fexpr ~output:"to_cmm.cmx" ~pass:"simplify" ~dumps:cmm "to_cmm.ml";
         fexpr ~output:"to_cmm_fast_path/to_cmm.cmx"
           ~flags:["-caml-apply-inline-fast-path"]
           ~pass:"simplify" ~dumps:cmm "to_cmm.ml";
         fexpr ~output:"to_cmm_o3_reaper/simplify.cmx"
           ~flags:["-O3"; "-flambda2-reaper"]
           ~pass:"reaper" ~dumps:cmm "simplify.ml" ]
       [ compare_sets "to_cmm.simplify.fl" "to_cmm.cmx.dump";
         list "to_cmm.cmx.dump";
         compare_sets "to_cmm_fast_path/to_cmm.simplify.fl"
           "to_cmm_fast_path/to_cmm.cmx.dump";
         list "to_cmm_fast_path/to_cmm.cmx.dump";
         compare_sets "to_cmm_o3_reaper/simplify.reaper.fl"
           "to_cmm_o3_reaper/simplify.cmx.dump" ]);
    dumps "to_cmm_reaper"
      [ fexpr
          ~flags:["-O3"; "-flambda2-reaper"]
          ~pass:"reaper"
          ~dumps:["-dcmm"; "-dfdo-dropped-counters"; "-dump-into-file"]
          "to_cmm_reaper.ml" ]
      [ compare_sets "to_cmm_reaper.reaper.fl" "to_cmm_reaper.cmx.dump";
        list "to_cmm_reaper.cmx.dump";
        dropped "to_cmm_reaper.cmx.dump" ];
    (* Probes are only supported on Linux. *)
    dumps "to_cmm_probe" ~linux_only:true
      [ compile
          ["-fdo-counters"; "-dcmm"; "-dfdo-dropped-counters"; "-dump-into-file"]
          "to_cmm_probe.ml" ]
      [list "to_cmm_probe.cmx.dump"; dropped "to_cmm_probe.cmx.dump"];
    (* The Cfg dumps hold every pass that prints one. *)
    pipeline "cfg" ~sources:["cfg.ml"; "to_cmm.ml"]
      ~stages:["cmm", ["-dcmm"]; "cfg", ["-dcfg"; "-dfdo-dropped-counters"]]
      (fun ~dump source ->
        compare_sets (dump "cmm") (dump "cfg")
        ::
        (if String.equal source "cfg.ml"
         then [list (dump "cmm"); list (dump "cfg"); dropped (dump "cfg")]
         else []));
    (* -use-ssa also checks, by -ssa-validate, that the Cfg converted from SSA
       has the counters of the one selected from Cmm, on the same edges and
       calls. The SSA dumps hold the SSA before and after [Ssa_simplify]. *)
    pipeline "ssa"
      ~configs:["", ["-use-ssa"]]
      ~sources:["cfg.ml"; "to_cmm.ml"]
      ~stages:
        [ "cmm", ["-dcmm"];
          "ssa", ["-dssa"; "-dfdo-dropped-counters"];
          "cfg", ["-dcfg"] ]
      (fun ~dump source ->
        [compare_sets (dump "cmm") (dump "ssa")]
        @ (if String.equal source "cfg.ml"
           then [list (dump "ssa"); dropped (dump "ssa")]
           else [])
        @ [compare_sets (dump "cmm") (dump "cfg")]);
    pipeline "linear"
      ~sources:["linear.ml"; "cfg.ml"; "to_cmm.ml"]
      ~stages:["cmm", ["-dcmm"]; "linear", ["-dlinear"]]
      (fun ~dump source ->
        compare_sets (dump "cmm") (dump "linear")
        ::
        (if String.equal source "to_cmm.ml" then [] else [list (dump "linear")]));
    (* End to end through the backend, at -O3, with the SSA pipeline and with
       the merging of identical blocks. *)
    pipeline "backend_o3"
      ~configs:
        [ "", ["-O3"];
          "_ssa", ["-O3"; "-use-ssa"];
          "_merge_blocks", ["-O3"; "-cfg-merge-blocks"] ]
      ~sources:
        ["linear.ml"; "cfg.ml"; "to_cmm.ml"; "to_cmm_reaper.ml"; "simplify.ml"]
      ~stages:
        ["cmm", ["-dcmm"]; "linear", ["-dlinear"; "-dfdo-dropped-counters"]]
      (fun ~dump _ -> [check (dump "cmm") (dump "linear")]);
    Codegen
      { sources =
          [ "body_hash.ml";
            "cfg.ml";
            "function_ids.ml";
            "instrumentation.ml";
            "linear.ml";
            "simplify.ml";
            "to_cmm.ml";
            "to_cmm_probe.ml";
            "to_cmm_reaper.ml" ];
        configs =
          [ "default", [];
            "o3", ["-O3"];
            "ssa", ["-use-ssa"];
            "o3_ssa", ["-O3"; "-use-ssa"];
            "o3_merge_blocks", ["-O3"; "-cfg-merge-blocks"] ]
      } ]

let () =
  (* Format breaks lines that reach the margin. *)
  Format.set_margin 81;
  Format.printf "@[<v>%a@]@."
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,@,")
       print)
    (List.concat_map stanzas tests)
