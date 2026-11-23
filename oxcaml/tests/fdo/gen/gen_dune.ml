(* Generates dune.inc, the rules of the FDO tests. A unit test is an executable
   whose output is compared with [<name>.output]. A dump test compiles sources
   with dumps, reads their FDO annotations with [fdo_annotations.exe], and
   compares its output with [<name>.output]. After changing this file, run the
   tests and promote dune.inc. *)

(* {2 S-expressions, printed in the style of dune files} *)

type sexp =
  | Atom of string
  | List of sexp list

let rec flat = function
  | Atom s -> s
  | List l -> "(" ^ String.concat " " (List.map flat l) ^ ")"

let margin = 80

(* A list is on one line if it fits, else its first element is on the line of
   the parenthesis and each other element on a line of its own. *)
let rec print buf ~indent sexp =
  let line = flat sexp in
  match sexp with
  | Atom _ -> Buffer.add_string buf line
  | List _ when indent + String.length line <= margin ->
    Buffer.add_string buf line
  | List [] -> Buffer.add_string buf "()"
  | List (first :: rest) ->
    Buffer.add_char buf '(';
    print buf ~indent:(indent + 1) first;
    List.iter
      (fun sexp ->
        Buffer.add_char buf '\n';
        Buffer.add_string buf (String.make (indent + 1) ' ');
        print buf ~indent:(indent + 1) sexp)
      rest;
    Buffer.add_char buf ')'

(* [text], filled into comment lines. *)
let comment buf text =
  let words =
    String.split_on_char ' '
      (String.concat " " (String.split_on_char '\n' text))
    |> List.filter (fun word -> not (String.equal word ""))
  in
  let line = Buffer.create margin in
  let flush () =
    Buffer.add_string buf (";" ^ Buffer.contents line ^ "\n");
    Buffer.clear line
  in
  List.iter
    (fun word ->
      if Buffer.length line + 1 + String.length word + 1 > margin then flush ();
      Buffer.add_char line ' ';
      Buffer.add_string line word)
    words;
  if Buffer.length line > 0 then flush ()

let atom s = Atom s

let atoms = List.map atom

let field name args = List (Atom name :: args)

(* {2 Tests} *)

type compile =
  { flags : string list;
    output : string option;
    source : string;
    clear_ocamlparam : bool
  }

(* The dumps are read from next to the .cmx files, so OCAMLPARAM is cleared by
   default: CI's metrics job sets dump-dir in it, which moves them. *)
let compile ?(clear_ocamlparam = true) ?output flags source =
  { flags; output; source; clear_ocamlparam }

type step =
  | Annotations of string list  (** A command of [fdo_annotations.exe]. *)
  | Cat of string

type test =
  | Unit of
      { name : string;
        libraries : string
      }
  | Dumps of
      { name : string;
        sources : string list;
        dirs : string list;
        compiles : compile list;
        steps : step list;
        linux_only : bool
      }
  | Annotations_exe

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

let stanzas = function
  | Annotations_exe ->
    [ field "executable"
        [ field "name" [Atom "fdo_annotations"];
          field "modules" [Atom "fdo_annotations"] ] ]
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
  | Dumps { name; sources; dirs; compiles; steps; linux_only } ->
    let compile { flags; output; source; clear_ocamlparam } =
      let run =
        field "run"
          (atoms
             (["%{bin:ocamlopt.opt}"; "-c"]
             @ flags @ ["-color"; "never"]
             @ (match output with None -> [] | Some output -> ["-o"; output])
             @ [source]))
      in
      if clear_ocamlparam
      then field "setenv" [Atom "OCAMLPARAM"; Atom "\"\""; run]
      else run
    in
    let step = function
      | Annotations args ->
        field "run" (atoms ("./fdo_annotations.exe" :: args))
      | Cat file -> field "run" (atoms ["cat"; file])
    in
    let steps =
      match List.map step steps with
      | [step] -> step
      | steps -> field "progn" steps
    in
    let mkdir =
      match dirs with
      | [] -> []
      | dirs -> [field "run" (atoms ("mkdir" :: "-p" :: dirs))]
    in
    [ field "rule"
        [ field "targets" [Atom (name ^ ".output.corrected")];
          field "deps" (atoms (sources @ ["fdo_annotations.exe"]));
          enabled_if ~linux_only;
          field "action"
            [ field "progn"
                (mkdir @ List.map compile compiles
                @ [ field "with-stdout-to"
                      [Atom (name ^ ".output.corrected"); steps] ]) ] ];
      diff ~linux_only name ]

let unit name = Unit { name; libraries = "ocamloptcomp" }

let dumps ?(linux_only = false) ?(dirs = []) name ~sources compiles steps =
  Dumps { name; sources; dirs; compiles; steps; linux_only }

let list file = Annotations ["list"; file]

let dropped file = Annotations ["dropped"; file]

let compare_sets a b = Annotations ["compare-sets"; a; b]

let fexpr_flags pass =
  [ "-fdo-counters";
    "-fexpr-print-fdo-counters";
    "-fexpr-print-branch-annotations";
    "-dflambda-heavy-invariants";
    "-dfexpr-annot-after";
    pass ]

let tests =
  [ ( "The operations on pseudo-instrumentation counters ([Fdo_counter]).",
      Unit { name = "fdo_counters"; libraries = "ocamlcommon" } );
    ( "The counters of the cases of the switches compiled to Cmm.",
      unit "cmm_switch_counters" );
    ( "The counters that instruction selection puts in the Cfg.",
      unit "selection_counters" );
    ( "The counters of the terminators that [Simplify_terminator] rewrites.",
      unit "simplify_terminator_counters" );
    "The counters through the passes of the SSA pipeline.", unit "ssa_counters";
    ( "The counters on the machine edges of the terminators that \
       [Cfg_to_linear] linearizes.",
      unit "cfg_to_linear_counters" );
    ( "The counters of the conditional branches that [Branch_relaxation] \
       relaxes.",
      unit "branch_relaxation_counters" );
    ( "The counters of the blocks that [Cfg_merge_blocks] merges.",
      unit "merge_blocks_counters" );
    "Extracting the FDO annotations from dumps.", Annotations_exe;
    ( "Function identities ([Fdo_function_naming]). function_ids_edited.ml is \
       function_ids.ml with anonymous functions inserted (other functions keep \
       their names), an optional argument added to [opt] (it keeps its name, \
       and gains a stub), and the second [shadowing] removed. In both, \
       shadowed functions and anonymous functions with the same body get \
       distinct discriminators, and functions without a location (as in \
       classes) are named by their binders.",
      let flags =
        [ "-fdo-counters";
          "-fexpr-print-fdo-counters";
          "-dfexpr-annot-after";
          "raw" ]
      in
      dumps "function_ids"
        ~sources:["function_ids.ml"; "function_ids_edited.ml"]
        [ compile ~clear_ocamlparam:false flags "function_ids.ml";
          compile ~clear_ocamlparam:false ~output:"function_ids.edited.cmx"
            flags "function_ids_edited.ml" ]
        [ Annotations
            [ "compare-entries";
              "function_ids.raw.fl";
              "function_ids.edited.raw.fl" ] ] );
    ( "Body hashes ([Fdo_body_hash]). body_hash_edited.ml changes the [keep_*] \
       functions in ways that must keep their hashes, and the [change_*] \
       functions in ways that must change them. Functions with the same hash \
       in both must have the same counters.",
      let flags =
        [ "-fdo-counters";
          "-fexpr-print-fdo-counters";
          "-fexpr-print-branch-annotations";
          "-dfexpr-annot-after";
          "raw" ]
      in
      dumps "body_hash"
        ~sources:["body_hash.ml"; "body_hash_edited.ml"]
        [ compile ~clear_ocamlparam:false flags "body_hash.ml";
          compile ~clear_ocamlparam:false ~output:"body_hash.edited.cmx" flags
            "body_hash_edited.ml" ]
        [ Annotations
            ["compare-entries"; "body_hash.raw.fl"; "body_hash.edited.raw.fl"];
          Annotations
            ["compare-counters"; "body_hash.raw.fl"; "body_hash.edited.raw.fl"]
        ] );
    ( "The counters attached by [Fdo_instrumentation] during the translation \
       from Lambda: edges of switches, interior and module-level calls, in \
       nested functions, functors and local modules.",
      dumps "instrumentation" ~sources:["instrumentation.ml"]
        [ compile ~clear_ocamlparam:false (fexpr_flags "raw")
            "instrumentation.ml" ]
        [list "instrumentation.raw.fl"] );
    ( "The counters after Simplify: inlined bodies get their call site as \
       inlining context, the functions of inlined functor applications are \
       copies with their own counters, and the counters of removed \
       instructions (inlined calls, folded switches, self tail calls) are \
       dropped. Compilation stops after the middle end, so that only its \
       dropped counters are listed.",
      dumps "simplify" ~sources:["simplify.ml"]
        [ compile
            (fexpr_flags "simplify"
            @ [ "-dfdo-dropped-counters";
                "-dump-into-file";
                "-stop-after";
                "middle_end" ])
            "simplify.ml" ]
        [list "simplify.simplify.fl"; Cat "simplify.cmx.dump"] );
    ( "The same at -O3 with the reaper, after the reaper (in a directory of \
       its own, so that the module keeps its name). Without local value slots, \
       which CI may enable by default and which remove the code of [R.step].",
      dumps "simplify_o3_reaper" ~sources:["simplify.ml"] ~dirs:["o3_reaper"]
        [ compile ~output:"o3_reaper/simplify.cmx"
            (["-O3"; "-flambda2-reaper"; "-no-reaper-local-fields"]
            @ fexpr_flags "reaper"
            @ [ "-dfdo-dropped-counters";
                "-dump-into-file";
                "-stop-after";
                "middle_end" ])
            "simplify.ml" ]
        [list "o3_reaper/simplify.reaper.fl"; Cat "o3_reaper/simplify.cmx.dump"]
    );
    ( "The counters in Cmm: none of the Flambda translated to Cmm is lost, and \
       none is added. [to_cmm.ml] is also compiled with the inline fast path \
       of [caml_apply], whose two calls share the counter, and [simplify.ml] \
       at -O3 with the reaper.",
      let flags pass =
        [ "-fdo-counters";
          "-fexpr-print-fdo-counters";
          "-fexpr-print-branch-annotations";
          "-dfexpr-annot-after";
          pass;
          "-dcmm";
          "-dump-into-file" ]
      in
      dumps "to_cmm"
        ~sources:["to_cmm.ml"; "simplify.ml"]
        ~dirs:["to_cmm_fast_path"; "to_cmm_o3_reaper"]
        [ compile ~output:"to_cmm.cmx" (flags "simplify") "to_cmm.ml";
          compile ~output:"to_cmm_fast_path/to_cmm.cmx"
            ("-caml-apply-inline-fast-path" :: flags "simplify")
            "to_cmm.ml";
          compile ~output:"to_cmm_o3_reaper/simplify.cmx"
            (["-O3"; "-flambda2-reaper"] @ flags "reaper")
            "simplify.ml" ]
        [ compare_sets "to_cmm.simplify.fl" "to_cmm.cmx.dump";
          list "to_cmm.cmx.dump";
          compare_sets "to_cmm_fast_path/to_cmm.simplify.fl"
            "to_cmm_fast_path/to_cmm.cmx.dump";
          list "to_cmm_fast_path/to_cmm.cmx.dump";
          compare_sets "to_cmm_o3_reaper/simplify.reaper.fl"
            "to_cmm_o3_reaper/simplify.cmx.dump" ] );
    ( "The counters of the switches To_cmm turns into a table lookup or a \
       single jump, which the reaper leaves for it: they are dropped.",
      dumps "to_cmm_reaper" ~sources:["to_cmm_reaper.ml"]
        [ compile
            (["-O3"; "-flambda2-reaper"]
            @ fexpr_flags "reaper"
            @ ["-dcmm"; "-dfdo-dropped-counters"; "-dump-into-file"])
            "to_cmm_reaper.ml" ]
        [ compare_sets "to_cmm_reaper.reaper.fl" "to_cmm_reaper.cmx.dump";
          list "to_cmm_reaper.cmx.dump";
          dropped "to_cmm_reaper.cmx.dump" ] );
    ( "The counter of the call of a probe handler, which To_cmm replaces by \
       the probe: it is dropped. Probes are only supported on Linux.",
      dumps "to_cmm_probe" ~linux_only:true ~sources:["to_cmm_probe.ml"]
        [ compile
            [ "-fdo-counters";
              "-dcmm";
              "-dfdo-dropped-counters";
              "-dump-into-file" ]
            "to_cmm_probe.ml" ]
        [list "to_cmm_probe.cmx.dump"; dropped "to_cmm_probe.cmx.dump"] );
    ( "The counters in Cfg: none of the Cmm selected is lost, and none is \
       added. A self tail call becomes a jump, whose counter is dropped. The \
       Cfg dumps hold every pass that prints one.",
      dumps "cfg" ~sources:["cfg.ml"; "to_cmm.ml"] ~dirs:["cfg_cmm"; "cfg_cfg"]
        [ compile ~output:"cfg_cmm/cfg.cmx"
            ["-fdo-counters"; "-dcmm"; "-dump-into-file"]
            "cfg.ml";
          compile ~output:"cfg_cfg/cfg.cmx"
            [ "-fdo-counters";
              "-dcfg";
              "-dfdo-dropped-counters";
              "-dump-into-file" ]
            "cfg.ml";
          compile ~output:"cfg_cmm/to_cmm.cmx"
            ["-fdo-counters"; "-dcmm"; "-dump-into-file"]
            "to_cmm.ml";
          compile ~output:"cfg_cfg/to_cmm.cmx"
            ["-fdo-counters"; "-dcfg"; "-dump-into-file"]
            "to_cmm.ml" ]
        [ compare_sets "cfg_cmm/cfg.cmx.dump" "cfg_cfg/cfg.cmx.dump";
          list "cfg_cmm/cfg.cmx.dump";
          list "cfg_cfg/cfg.cmx.dump";
          dropped "cfg_cfg/cfg.cmx.dump";
          compare_sets "cfg_cmm/to_cmm.cmx.dump" "cfg_cfg/to_cmm.cmx.dump" ] );
    ( "The counters through SSA: those of the Cmm are in the SSA and in the \
       Cfg converted from it, except that of a self tail call, which becomes a \
       jump. [-use-ssa] also checks, by [-ssa-validate], that the Cfg \
       converted from SSA has the counters of the one selected from Cmm, on \
       the same edges and calls. The SSA dumps hold the SSA before and after \
       [Ssa_simplify].",
      let compile dir dump ?(extra = []) source =
        compile
          ~output:(dir ^ "/" ^ Filename.chop_extension source ^ ".cmx")
          (["-use-ssa"; "-fdo-counters"; dump] @ extra @ ["-dump-into-file"])
          source
      in
      dumps "ssa" ~sources:["cfg.ml"; "to_cmm.ml"]
        ~dirs:["ssa_cmm"; "ssa_ssa"; "ssa_cfg"]
        [ compile "ssa_cmm" "-dcmm" "cfg.ml";
          compile "ssa_ssa" "-dssa" ~extra:["-dfdo-dropped-counters"] "cfg.ml";
          compile "ssa_cfg" "-dcfg" "cfg.ml";
          compile "ssa_cmm" "-dcmm" "to_cmm.ml";
          compile "ssa_ssa" "-dssa" "to_cmm.ml";
          compile "ssa_cfg" "-dcfg" "to_cmm.ml" ]
        [ compare_sets "ssa_cmm/cfg.cmx.dump" "ssa_ssa/cfg.cmx.dump";
          list "ssa_ssa/cfg.cmx.dump";
          dropped "ssa_ssa/cfg.cmx.dump";
          compare_sets "ssa_cmm/cfg.cmx.dump" "ssa_cfg/cfg.cmx.dump";
          compare_sets "ssa_cmm/to_cmm.cmx.dump" "ssa_ssa/to_cmm.cmx.dump";
          compare_sets "ssa_cmm/to_cmm.cmx.dump" "ssa_cfg/to_cmm.cmx.dump" ] );
    ( "The counters in Linear: none of the Cmm is lost through the Cfg passes \
       apart from the dropped ones, and none is added. A conditional branch \
       has the counters of its taken edge and of its fallthrough edge.",
      let compile dir dump source =
        compile
          ~output:(dir ^ "/" ^ Filename.chop_extension source ^ ".cmx")
          ["-fdo-counters"; dump; "-dump-into-file"]
          source
      in
      dumps "linear"
        ~sources:["linear.ml"; "cfg.ml"; "to_cmm.ml"]
        ~dirs:["linear_cmm"; "linear_linear"]
        [ compile "linear_cmm" "-dcmm" "linear.ml";
          compile "linear_linear" "-dlinear" "linear.ml";
          compile "linear_cmm" "-dcmm" "cfg.ml";
          compile "linear_linear" "-dlinear" "cfg.ml";
          compile "linear_cmm" "-dcmm" "to_cmm.ml";
          compile "linear_linear" "-dlinear" "to_cmm.ml" ]
        [ compare_sets "linear_cmm/linear.cmx.dump"
            "linear_linear/linear.cmx.dump";
          list "linear_linear/linear.cmx.dump";
          compare_sets "linear_cmm/cfg.cmx.dump" "linear_linear/cfg.cmx.dump";
          list "linear_linear/cfg.cmx.dump";
          compare_sets "linear_cmm/to_cmm.cmx.dump"
            "linear_linear/to_cmm.cmx.dump" ] ) ]

let () =
  let buf = Buffer.create 65536 in
  List.iter
    (fun (description, test) ->
      Buffer.add_char buf '\n';
      comment buf description;
      List.iter
        (fun stanza ->
          Buffer.add_char buf '\n';
          print buf ~indent:0 stanza;
          Buffer.add_char buf '\n')
        (stanzas test))
    tests;
  print_string (Buffer.contents buf)
