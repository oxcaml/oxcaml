open Std

(** {1 OCaml commandline parsing} *)

let { Logger.log } = Logger.for_section "Mconfig"

type ocaml =
  { include_dirs : string list;
    hidden_dirs : string list;
    no_std_include : bool;
    unsafe : bool;
    classic : bool;
    principal : bool;
    real_paths : bool;
    threads : [ `None | `Threads | `Vmthreads ];
    recursive_types : bool;
    strict_sequence : bool;
    applicative_functors : bool;
    nopervasives : bool;
    strict_formats : bool;
    open_modules : string list;
    ppx : string with_workdir list;
    pp : string with_workdir option;
    warnings : Warnings.state;
    cmi_file : string option;
    parameters : string list;
    as_parameter : bool;
    as_argument_for : string option;
    zero_alloc_check : Zero_alloc_annotations.Check.t;
    zero_alloc_assert : Zero_alloc_annotations.Assert.t;
    infer_with_bounds : bool
  }

let dump_warnings st =
  let st' = Warnings.backup () in
  Warnings.restore st;
  Misc.try_finally Warnings.dump ~always:(fun () -> Warnings.restore st')

let dump_ocaml x =
  `Assoc
    [ ("include_dirs", `List (List.map ~f:Json.string x.include_dirs));
      ("hidden_dirs", `List (List.map ~f:Json.string x.hidden_dirs));
      ("no_std_include", `Bool x.no_std_include);
      ("unsafe", `Bool x.unsafe);
      ("classic", `Bool x.classic);
      ("principal", `Bool x.principal);
      ("real_paths", `Bool x.real_paths);
      ("recursive_types", `Bool x.recursive_types);
      ("strict_sequence", `Bool x.strict_sequence);
      ("applicative_functors", `Bool x.applicative_functors);
      ("nopervasives", `Bool x.nopervasives);
      ("strict_formats", `Bool x.strict_formats);
      ("open_modules", Json.list Json.string x.open_modules);
      ("ppx", Json.list (dump_with_workdir Json.string) x.ppx);
      ("pp", Json.option (dump_with_workdir Json.string) x.pp);
      ("warnings", dump_warnings x.warnings);
      ("cmi_file", Json.option Json.string x.cmi_file);
      ("parameters", `List (List.map ~f:Json.string x.parameters));
      ("as_parameter", `Bool x.as_parameter);
      ( "zero_alloc_check",
        `String (Zero_alloc_annotations.Check.to_string x.zero_alloc_check) );
      ( "zero_alloc_assert",
        `String (Zero_alloc_annotations.Assert.to_string x.zero_alloc_assert) )
    ]

(** Some paths can be resolved relative to a current working directory *)

let cwd = ref None

let unsafe_get_cwd () =
  match !cwd with
  | None -> assert false
  | Some cwd -> cwd

let canonicalize_filename path = Misc.canonicalize_filename ?cwd:!cwd path

let marg_path f =
  Marg.param "path" (fun path acc -> f (canonicalize_filename path) acc)

let marg_commandline f =
  Marg.param "command" (fun workval acc ->
      f { workdir = unsafe_get_cwd (); workval } acc)

(** {1 Merlin high-level settings} *)

type merlin =
  { build_path : string list;
    source_path : string list;
    hidden_build_path : string list;
    hidden_source_path : string list;
    cmi_path : string list;
    cmt_path : string list;
    index_files : string list;
    extensions : string list;
    suffixes : (string * string) list;
    stdlib : string option;
    source_root : string option;
    unit_name : string option;
    unit_name_for : string String.Map.t;
    wrapping_prefix : string option;
    reader : string list;
    protocol : [ `Json | `Sexp ];
    log_file : string option;
    log_sections : string list;
    config_path : string option;
    use_ppx_cache : bool;
    exclude_query_dir : bool;
    flags_to_apply : string list with_workdir list;
    flags_applied : string list with_workdir list;
    failures : string list;
    extension_to_reader : (string * string) list;
    cache_lifespan : int
  }

let dump_merlin x =
  let dump_flag_list flags = dump_with_workdir (Json.list Json.string) flags in
  `Assoc
    [ ("build_path", `List (List.map ~f:Json.string x.build_path));
      ("source_path", `List (List.map ~f:Json.string x.source_path));
      ("hidden_build_path", `List (List.map ~f:Json.string x.hidden_build_path));
      ( "hidden_source_path",
        `List (List.map ~f:Json.string x.hidden_source_path) );
      ("cmi_path", `List (List.map ~f:Json.string x.cmi_path));
      ("cmt_path", `List (List.map ~f:Json.string x.cmt_path));
      ("index_files", `List (List.map ~f:Json.string x.index_files));
      ("flags_applied", `List (List.map ~f:dump_flag_list x.flags_applied));
      ("extensions", `List (List.map ~f:Json.string x.extensions));
      ( "suffixes",
        `List
          (List.map
             ~f:(fun (impl, intf) ->
               `Assoc [ ("impl", `String impl); ("intf", `String intf) ])
             x.suffixes) );
      ("stdlib", Json.option Json.string x.stdlib);
      ("source_root", Json.option Json.string x.source_root);
      ("unit_name", Json.option Json.string x.unit_name);
      ( "unit_name_for",
        let alist =
          x.unit_name_for |> String.Map.map ~f:Json.string |> String.Map.to_list
        in
        `Assoc alist );
      ("wrapping_prefix", Json.option Json.string x.wrapping_prefix);
      ("reader", `List (List.map ~f:Json.string x.reader));
      ( "protocol",
        match x.protocol with
        | `Json -> `String "json"
        | `Sexp -> `String "sexp" );
      ("log_file", Json.option Json.string x.log_file);
      ("log_sections", Json.list Json.string x.log_sections);
      ("flags_to_apply", `List (List.map ~f:dump_flag_list x.flags_to_apply));
      ("failures", `List (List.map ~f:Json.string x.failures));
      ( "assoc_suffixes",
        `List
          (List.map
             ~f:(fun (suffix, reader) ->
               `Assoc
                 [ ("extension", `String suffix); ("reader", `String reader) ])
             x.extension_to_reader) );
      ("cache_lifespan", Json.string (string_of_int x.cache_lifespan))
    ]

module Verbosity = struct
  type t = Smart | Lvl of int

  let default = Lvl 0

  let to_int t ~for_smart =
    match t with
    | Smart -> for_smart
    | Lvl v -> v

  let param_spec = "\"smart\" | <integer>"

  let of_string = function
    | "smart" -> Smart
    | maybe_int -> (
      try Lvl (int_of_string maybe_int)
      with _ -> invalid_arg ("argument should be: " ^ param_spec))

  let to_string = function
    | Smart -> "smart"
    | Lvl v -> "lvl " ^ string_of_int v

  let to_json t = `String (to_string t)
end

type query =
  { filename : string;
    directory : string;
    printer_width : int;
    verbosity : Verbosity.t
  }

let dump_query x =
  `Assoc
    [ ("filename", `String x.filename);
      ("directory", `String x.directory);
      ("printer_width", `Int x.printer_width);
      ("verbosity", Verbosity.to_json x.verbosity)
    ]

type t = { ocaml : ocaml; merlin : merlin; query : query }

let dump x =
  `Assoc
    [ ("ocaml", dump_ocaml x.ocaml);
      ("merlin", dump_merlin x.merlin);
      ("query", dump_query x.query)
    ]

let arguments_table = Hashtbl.create 67

let stdlib =
  let env =
    try Some (Sys.getenv "OCAMLLIB")
    with Not_found -> (
      try Some (Sys.getenv "CAMLLIB") with Not_found -> None)
  in
  fun config ->
    match config.merlin.stdlib with
    | Some stdlib -> stdlib
    | None -> (
      match env with
      | Some stdlib -> stdlib
      | None -> Standard_library.path)

let normalize_step t =
  let merlin = t.merlin in
  if merlin.flags_to_apply <> [] then
    let flagss = merlin.flags_to_apply in
    let t =
      { t with
        merlin =
          { merlin with
            flags_to_apply = [];
            flags_applied = flagss @ merlin.flags_applied
          }
      }
    in
    let failures = ref [] in
    let warning failure = failures := failure :: !failures in
    let t =
      List.fold_left
        ~f:(fun t { workdir; workval } ->
          fst
            (let_ref cwd (Some workdir)
               (Marg.parse_all ~warning arguments_table [] workval t)))
        ~init:t flagss
    in
    { t with
      merlin = { t.merlin with failures = !failures @ t.merlin.failures }
    }
  else t

let is_normalized t =
  let merlin = t.merlin in
  merlin.flags_to_apply = []

let rec normalize t =
  if is_normalized t then (
    log ~title:"normalize" "%a" Logger.json (fun () -> dump t);
    t)
  else normalize (normalize_step t)

let merge_merlin_config dot merlin ~failures ~config_path =
  { merlin with
    build_path = dot.Mconfig_dot.build_path @ merlin.build_path;
    source_path = dot.source_path @ merlin.source_path;
    hidden_build_path = dot.hidden_build_path @ merlin.hidden_build_path;
    hidden_source_path = dot.hidden_source_path @ merlin.hidden_source_path;
    cmi_path = dot.cmi_path @ merlin.cmi_path;
    cmt_path = dot.cmt_path @ merlin.cmt_path;
    index_files = dot.index_files @ merlin.index_files;
    exclude_query_dir = dot.exclude_query_dir || merlin.exclude_query_dir;
    use_ppx_cache = dot.use_ppx_cache || merlin.use_ppx_cache;
    extensions = dot.extensions @ merlin.extensions;
    suffixes = dot.suffixes @ merlin.suffixes;
    stdlib = (if dot.stdlib = None then merlin.stdlib else dot.stdlib);
    unit_name =
      (if dot.unit_name = None then merlin.unit_name else dot.unit_name);
    unit_name_for =
      String.Map.merge
        ~f:(fun _ dot merlin ->
          match (dot, merlin) with
          | Some dot, _ -> Some dot
          | None, Some merlin -> Some merlin
          | None, None -> None)
        dot.unit_name_for merlin.unit_name_for;
    wrapping_prefix =
      (if dot.wrapping_prefix = None then merlin.wrapping_prefix
       else dot.wrapping_prefix);
    source_root =
      (if dot.source_root = None then merlin.source_root else dot.source_root);
    reader = (if dot.reader = [] then merlin.reader else dot.reader);
    flags_to_apply = dot.flags @ merlin.flags_to_apply;
    failures = failures @ merlin.failures;
    config_path = Some config_path
  }

let get_external_config path t =
  let path = Misc.canonicalize_filename path in
  let directory = Filename.dirname path in
  match Mconfig_dot.find_project_context directory with
  | None -> t
  | Some (ctxt, config_path) ->
    let dot, failures = Mconfig_dot.get_config ctxt path in
    let merlin = merge_merlin_config dot t.merlin ~failures ~config_path in
    normalize { t with merlin }

let merlin_flags =
  [ ( "-build-path",
      marg_path (fun dir merlin ->
          { merlin with build_path = dir :: merlin.build_path }),
      "<dir> Add <dir> to merlin build path" );
    ( "-source-path",
      marg_path (fun dir merlin ->
          { merlin with source_path = dir :: merlin.source_path }),
      "<dir> Add <dir> to merlin source path" );
    ( "-hidden-build-path",
      marg_path (fun dir merlin ->
          { merlin with hidden_build_path = dir :: merlin.hidden_build_path }),
      "<dir> Add <dir> to merlin hidden build path" );
    ( "-hidden-source-path",
      marg_path (fun dir merlin ->
          { merlin with hidden_source_path = dir :: merlin.hidden_source_path }),
      "<dir> Add <dir> to merlin hidden source path" );
    ( "-cmi-path",
      marg_path (fun dir merlin ->
          { merlin with cmi_path = dir :: merlin.cmi_path }),
      "<dir> Add <dir> to merlin cmi path" );
    ( "-cmt-path",
      marg_path (fun dir merlin ->
          { merlin with cmt_path = dir :: merlin.cmt_path }),
      "<dir> Add <dir> to merlin cmt path" );
    ( "-index-file",
      marg_path (fun file merlin ->
          { merlin with index_files = file :: merlin.index_files }),
      "<file> Add <file> to the index files used by merlin" );
    ( "-reader",
      Marg.param "command" (fun reader merlin ->
          { merlin with reader = Shell.split_command reader }),
      "<command> Use <command> as a merlin reader" );
    ( "-assocsuffix",
      Marg.param "suffix:reader" (fun assoc_pair merlin ->
          match Misc.rev_string_split ~on:':' assoc_pair with
          | [ reader; suffix ] ->
            { merlin with
              extension_to_reader =
                (suffix, reader) :: merlin.extension_to_reader
            }
          | _ -> merlin),
      "Associate suffix with reader" );
    ( "-addsuffix",
      Marg.param "implementation Suffix, interface Suffix"
        (fun suffix_pair merlin ->
          match Misc.rev_string_split ~on:':' suffix_pair with
          | [ intf; impl ] ->
            { merlin with suffixes = (impl, intf) :: merlin.suffixes }
          | _ -> merlin),
      "Add a suffix implementation,interface pair" );
    ( "-merlin-extension",
      Marg.param "extension" (fun extension merlin ->
          match Extension.lookup extension with
          | None -> invalid_arg "Unknown extension"
          | Some _ ->
            { merlin with extensions = extension :: merlin.extensions }),
      "<extension> Load merlin syntax extension" );
    ( "-flags",
      Marg.param "string" (fun flags merlin ->
          let flags =
            { workdir = unsafe_get_cwd (); workval = Shell.split_command flags }
          in
          { merlin with flags_to_apply = flags :: merlin.flags_to_apply }),
      "<quoted flags> Unescape argument and interpret it as more flags" );
    ( "-protocol",
      Marg.param "protocol" (fun prot merlin ->
          match prot with
          | "json" -> { merlin with protocol = `Json }
          | "sexp" -> { merlin with protocol = `Sexp }
          | _ -> invalid_arg "Valid protocols are 'json' and 'sexp'"),
      "<protocol> Select frontend protocol ('json' or 'sexp')" );
    ( "-log-file",
      Marg.param "file" (fun file merlin ->
          { merlin with log_file = Some file }),
      "<file> Log messages to specified file ('' for disabling, '-' for stderr)"
    );
    ( "-log-section",
      Marg.param "file" (fun section merlin ->
          let sections = String.split_on_char_ ',' section in
          { merlin with log_sections = sections @ merlin.log_sections }),
      "<section,...> Only log specific sections (separated by comma)" );
    ( "-ocamllib-path",
      marg_path (fun path merlin -> { merlin with stdlib = Some path }),
      "<path> Change path of ocaml standard library" );
    ( "-cache-lifespan",
      Marg.param "int" (fun prot merlin ->
          try { merlin with cache_lifespan = int_of_string prot }
          with _ -> invalid_arg "Valid value is int"),
      "Change file cache retention period. It's measured in minutes. Default \
       value is 5." );
    ( (* Legacy support for janestreet. Ignored. To be removed soon. *)
      "-attributes-allowed",
      Marg.unit_ignore,
      " DEPRECATED" )
  ]

let query_flags =
  [ ( "-verbosity",
      Marg.param Verbosity.param_spec (fun verbosity query ->
          let verbosity = Verbosity.of_string verbosity in
          { query with verbosity }),
      "\"smart\" | <integer> Verbosity determines the number of expansions of \
       aliases in answers. \"smart\" is equivalent to verbosity=0 but expands \
       module types." );
    ( "-printer-width",
      Marg.param "integer" (fun width query ->
          let printer_width =
            try int_of_string width
            with _ -> invalid_arg "argument should be an integer"
          in
          { query with printer_width }),
      "<integer> Optimal width for formatting types, signatures, etc" )
  ]

let ocaml_ignored_flags =
  [ "-a";
    "-absname";
    "-alias-deps";
    "-annot";
    "-app-funct";
    "-bin-annot";
    "-bin-annot-cms";
    "-c";
    "-compact";
    "-compat-32";
    "-config";
    "-custom";
    "-dclambda";
    "-dcmm";
    "-dcse";
    "-dflambda";
    "-dflambda-no-invariants";
    "-dflambda-verbose";
    "-dinstr";
    "-dlambda";
    "-dblambda";
    "-dlinear";
    "-dparsetree";
    "-dshape";
    "-dslambda";
    "-drawclambda";
    "-drawflambda";
    "-drawlambda";
    "-dsource";
    "-dstartup";
    "-dtimings";
    "-dprofile";
    "-dtypedtree";
    "-dtypes";
    "-dump-pass";
    "-fno-PIC";
    "-fPIC";
    "-g";
    "-i";
    "-inlining-report";
    "-keep-docs";
    "-no-keep-docs";
    "-keep-locs";
    "-linkall";
    "-make_runtime";
    "-make-runtime";
    "-modern";
    "-no-alias-deps";
    "-noassert";
    "-noautolink";
    "-no-check-prims";
    "-nodynlink";
    "-no-float-const-prop";
    "-no-keep-locs";
    "-no-principal";
    "-no-rectypes";
    "-no-strict-formats";
    "-no-strict-sequence";
    "-no-unbox-free-vars-of-clos";
    "-no-unbox-specialised-args";
    "-no-unboxed-types";
    "-O2";
    "-O3";
    "-Oclassic";
    "-only-erasable-extensions";
    "-opaque";
    "-output-complete-obj";
    "-output-complete-exe";
    "-output-obj";
    "-p";
    "-pack";
    "-remove-unused-arguments";
    "-S";
    "-shared";
    "-unbox-closures";
    "-unboxed-types";
    "-v";
    "-verbose";
    "-where";
    "-no-absname";
    "-no-g";
    "-safe-matching";
    "-bin-annot-occurrences";
    (* flambda-backend specific *)
    "-basic-block-sections";
    "-caml-apply-inline-fast-path";
    "-debug-ocaml";
    "-dgc-timings";
    "-flambda2-backend-cse-at-toplevel";
    "-flambda2-debug";
    "-flambda2-debug-concrete-types-only-on-canonicals";
    "-flambda2-debug-keep-invalid-handlers";
    "-flambda2-debug-permute-every-name";
    "-flambda2-expert-can-inline-recursive-functions";
    "-flambda2-expert-code-id-and-symbol-scoping-checks";
    "-flambda2-expert-fallback-inlining-heuristic";
    "-flambda2-expert-inline-effects-in-cmm";
    "-flambda2-expert-phantom-lets";
    "-flambda2-inlining-report-bin";
    "-flambda2-join-points";
    "-no-flambda2-result-types";
    "-flambda2-result-types-all-functions";
    "-flambda2-result-types-functors-only";
    "-flambda2-speculative-inlining-only-if-arguments-useful";
    "-flambda2-unbox-along-intra-function-control-flow";
    "-flambda2-unicode";
    "-flambda2-kind-checks";
    "-no-flambda2-backend-cse-at-toplevel";
    "-no-flambda2-debug";
    "-no-flambda2-debug-concrete-types-only-on-canonicals";
    "-no-flambda2-debug-keep-invalid-handlers";
    "-no-flambda2-debug-permute-every-name";
    "-no-flambda2-expert-can-inline-recursive-functions";
    "-no-flambda2-expert-code-id-and-symbol-scoping-checks";
    "-no-flambda2-expert-fallback-inlining-heuristic";
    "-no-flambda2-expert-inline-effects-in-cmm";
    "-no-flambda2-expert-phantom-lets";
    "-no-flambda2-join-points";
    "-no-flambda2-speculative-inlining-only-if-arguments-useful";
    "-no-flambda2-unbox-along-intra-function-control-flow";
    "-ocamlcfg";
    "-no-ocamlcfg";
    "-regalloc-validate";
    "-no-regalloc-validate";
    "-use-cached-generic-functions";
    "-flambda2-expert-shorten-symbol-names";
    "-no-flambda2-expert-shorten-symbol-names";
    "-symbol-visibility-protected";
    "-no-symbol-visibility-protected";
    "-flambda2-basic-meet";
    "-flambda2-advanced-meet";
    "-directory";
    (* Jane Street specific *)
    "-disable-builtin-check";
    "-disable-poll-insertion";
    "-gdwarf-may-alter-codegen";
    "-gno-dwarf-may-alter-codegen";
    "-davail";
    "-dranges";
    "-ddebug-invariants";
    "-cfg-peephole-optimize";
    "-no-cfg-peephole-optimize";
    "-verbose-types";
    "-no-verbose-types";
    "-fsse3";
    "-fno-sse3";
    "-fssse3";
    "-fno-ssse3";
    "-fsse41";
    "-fno-sse41";
    "-fsse42";
    "-fno-sse42";
    "-fsimd-regalloc";
    "-fno-simd-regalloc";
    "-fclmul";
    "-fno-clmul";
    "-no-auto-include-otherlibs";
    "-fbmi2";
    "-fno-bmi2";
    "-fbmi";
    "-fno-bmi";
    "-fprefetchwt1";
    "-fno-prefetchwt1";
    "-fprefetchw";
    "-fno-prefetchw";
    "-fpopcnt";
    "-fno-popcnt";
    "-disable-zero-alloc-checker";
    "-disable-precise-zero-alloc-checker";
    "-cfg-stack-checks";
    "-no-cfg-stack-checks";
    "-gdwarf-inlined-frames";
    "-gno-dwarf-inlined-frames";
    "-cfg-stack-checks";
    "-no-cfg-stack-checks";
    "-gno-upstream-dwarf";
    "-dzero-alloc";
    "-dletreclambda";
    "-dcounters";
    "-vectorize";
    "-no-vectorize";
    "-dvectorize";
    "-dump-into-csv";
    "-no-mach-ir";
    "-flambda2-reaper";
    "-no-flambda2-reaper";
    "-dsimplify";
    "-dreaper";
    "-instantiate";
    "-dflambda-heavy-invariants";
    "-cfg-eliminate-dead-trap-handlers";
    "-no-cfg-eliminate-dead-trap-handlers";
    "-module-entry-functions-section";
    "-zero-alloc-checker-details-extra";
    "-no-zero-alloc-checker-details-extra";
    "-favx512f";
    "-fno-avx512f";
    "-favx2";
    "-fno-avx2";
    "-favx";
    "-fno-avx";
    "-ffma";
    "-fno-fma";
    "-ff16c";
    "-fno-f16c";
    "-dllvmir";
    "-keep-llvmir";
    "-llvm-backend";
    "-no-llvm-backend";
    "-ddwarf-types";
    "-ocamlcfg";
    "-cfg-prologue-validate";
    "-no-cfg-prologue-validate";
    "-cfg-prologue-shrink-wrap";
    "-no-cfg-prologue-shrink-wrap";
    "-cfg-value-propagation";
    "-no-cfg-value-propagation";
    "-cfg-value-propagation-float";
    "-no-cfg-value-propagation-float";
    "-gdwarf-pedantic";
    "-ddwarf-metrics";
    "-afl-instrument";
    "-probes";
    "-no-probes";
    "-probes-optimized";
    "-no-probes-optimized";
    "-disable-all-extensions";
    "-nocwd";
    "-with-runtime";
    "-without-runtime";
    "-ddebug-uids";
    "-ddebug-uid-tables";
    "-version";
    "--version";
    "-vnum";
    "-warn-help";
    "-help";
    "--help";
    "-match-context-rows";
    "-safer-matching";
    "-dno-unique-ids";
    "-dunique-ids";
    "-dno-locations";
    "-dlocations";
    "-dcamlprimc";
    "-dtimings-precision";
    "-dprofile-output";
    "-locs";
    "-no-locs";
    "-flzcnt";
    "-fno-lzcnt";
    "-reaper-local-fields";
    "-no-reaper-local-fields";
    "-reaper-unbox";
    "-no-reaper-unbox";
    "-reaper-change-calling-conventions";
    "-no-reaper-change-calling-conventions";
    "-flambda2-expert-cmm-safe-subst";
    "-no-flambda2-expert-cmm-safe-subst"
  ]

let ocaml_ignored_parametrized_flags =
  [ "-cc";
    "-cclib";
    "-ccopt";
    "-color";
    "-dflambda-let";
    "-dllib";
    "-dllpath";
    "-for-pack";
    "-impl";
    "-inline-alloc-cost";
    "-inline-branch-cost";
    "-inline-branch-factor";
    "-inline-call-cost";
    "-inline-indirect-cost";
    "-inline-lifting-benefit";
    "-inline-max-depth";
    "-inline-max-unroll";
    "-inline";
    "-inline-prim-cost";
    "-inline-toplevel";
    "-intf";
    "-intf_suffix";
    "-intf-suffix";
    "-o";
    "-rounds";
    "-runtime-variant";
    "-ocamlrunparam";
    "-unbox-closures-factor";
    "-use-prims";
    "-use_runtime";
    "-use-runtime";
    "-error-style";
    "-dump-dir";
    "-I-manifest";
    "-H-manifest";
    (* flambda-backend specific *)
    "-extension";
    "-extension-universe";
    "-no-extension";
    "-drawfexpr-to";
    "-dfexpr-to";
    "-dflexpect-to";
    "-reorder-blocks-random";
    "-heap-reduction-threshold";
    "-flambda2-cse-depth";
    "-flambda2-expert-max-block-size-for-projections";
    "-flambda2-expert-max-unboxing-depth";
    "-flambda2-join-depth";
    "-flambda2-inline-alloc-cost";
    "-flambda2-inline-branch-cost";
    "-flambda2-inline-call-cost";
    "-flambda2-inline-indirect-cost";
    "-flambda2-inline-large-function-size";
    "-flambda2-inline-max-depth";
    "-flambda2-inline-max-rec-depth";
    "-flambda2-inline-poly-compare-cost";
    "-flambda2-inline-prim-cost";
    "-flambda2-inline-small-function-size";
    "-flambda2-inline-threshold";
    "-flambda2-join-algorithm";
    "-flambda2-expert-cont-specialization-budget";
    "-regalloc";
    "-regalloc-linscan-threshold";
    "-regalloc-param";
    "-cached-generic-functions-path";
    "-gdwarf-max-function-complexity";
    "-function-layout";
    "-cfg-stack-checks-threshold";
    "-zero-alloc-checker-details-cutoff";
    "-zero-alloc-checker-join";
    "-dgranularity";
    "-flambda2-expert-cont-lifting-budget";
    "-vectorize-max-block-size";
    "-save-ir-before";
    "-shape-format";
    "-gdwarf-compression";
    "-gdwarf-fission";
    "-cfg-prologue-shrink-wrap-threshold";
    "-gdwarf-config-shape-reduce-depth";
    "-gdwarf-config-shape-eval-depth";
    "-gdwarf-config-max-cms-files-per-unit";
    "-gdwarf-config-max-cms-files-per-variable";
    "-gdwarf-config-max-type-to-shape-depth";
    "-gdwarf-config-max-shape-reduce-steps-per-variable";
    "-gdwarf-config-max-evaluation-steps-per-variable";
    "-gdwarf-config-shape-reduce-fuel";
    "-gdwarf-fidelity";
    "-llvm-path";
    "-afl-inst-ratio";
    "-config-var";
    "-stop-after";
    "-plugin";
    "-dump-into-file";
    "-ddwarf-metrics-output-file";
    "-reaper-debug-flags"
  ]

let ocaml_warnings_spec ~error =
  Marg.param "warning specification" (fun spec ocaml ->
      let b' = Warnings.backup () in
      Warnings.restore ocaml.warnings;
      Misc.try_finally
        (fun () ->
          ignore @@ Warnings.parse_options error spec;
          { ocaml with warnings = Warnings.backup () })
        ~always:(fun () -> Warnings.restore b'))

let ocaml_alert_spec =
  Marg.param "alert specification" (fun spec ocaml ->
      let b' = Warnings.backup () in
      Warnings.restore ocaml.warnings;
      Misc.try_finally
        (fun () ->
          Warnings.parse_alert_option spec;
          { ocaml with warnings = Warnings.backup () })
        ~always:(fun () -> Warnings.restore b'))

let ocaml_flags =
  [ ( "-I",
      marg_path (fun dir ocaml ->
          { ocaml with include_dirs = dir :: ocaml.include_dirs }),
      "<dir> Add <dir> to the list of include directories" );
    ( "-H",
      marg_path (fun dir ocaml ->
          { ocaml with hidden_dirs = dir :: ocaml.hidden_dirs }),
      "<dir>  Add <dir> to the list of \"hidden\" include directories\n\
      \ (Like -I, but the program can not directly reference these \
       dependencies)" );
    ( "-nostdlib",
      Marg.unit (fun ocaml -> { ocaml with no_std_include = true }),
      " Do not add default directory to the list of include directories" );
    ( "-unsafe",
      Marg.unit (fun ocaml -> { ocaml with unsafe = true }),
      " Do not compile bounds checking on array and string access" );
    ( "-labels",
      Marg.unit (fun ocaml -> { ocaml with classic = false }),
      " Use commuting label mode" );
    ( "-nolabels",
      Marg.unit (fun ocaml -> { ocaml with classic = true }),
      " Ignore non-optional labels in types" );
    ( "-principal",
      Marg.unit (fun ocaml -> { ocaml with principal = true }),
      " Check principality of type inference" );
    ( "-real-paths",
      Marg.unit (fun ocaml -> { ocaml with real_paths = true }),
      " Display real paths in types rather than short ones" );
    ( "-short-paths",
      Marg.unit (fun ocaml -> { ocaml with real_paths = false }),
      " Shorten paths in types" );
    ( "-rectypes",
      Marg.unit (fun ocaml -> { ocaml with recursive_types = true }),
      " Allow arbitrary recursive types" );
    ( "-strict-sequence",
      Marg.unit (fun ocaml -> { ocaml with strict_sequence = true }),
      " Left-hand part of a sequence must have type unit" );
    ( "-no-app-funct",
      Marg.unit (fun ocaml -> { ocaml with applicative_functors = false }),
      " Deactivate applicative functors" );
    ( "-thread",
      Marg.unit (fun ocaml -> { ocaml with threads = `Threads }),
      " Add support for system threads library" );
    ( "-vmthread",
      Marg.unit (fun ocaml -> { ocaml with threads = `None }),
      " Add support for VM-scheduled threads library" );
    ( "-safe-string",
      Marg.unit (fun ocaml -> ocaml),
      " Default to true unconditionally since 5.00" );
    ( "-nopervasives",
      Marg.unit (fun ocaml -> { ocaml with nopervasives = true }),
      " Don't open Pervasives module (advanced)" );
    ( "-strict-formats",
      Marg.unit (fun ocaml -> { ocaml with strict_formats = true }),
      " Reject invalid formats accepted by legacy implementations" );
    ( "-open",
      Marg.param "module" (fun md ocaml ->
          { ocaml with open_modules = md :: ocaml.open_modules }),
      "<module>  Opens the module <module> before typing" );
    ( "-ppx",
      marg_commandline (fun command ocaml ->
          { ocaml with ppx = command :: ocaml.ppx }),
      "<command> Pipe abstract syntax trees through preprocessor <command>" );
    ( "-pp",
      marg_commandline (fun pp ocaml -> { ocaml with pp = Some pp }),
      "<command> Pipe sources through preprocessor <command>" );
    ( "-w",
      ocaml_warnings_spec ~error:false,
      Printf.sprintf
        "<list>  Enable or disable warnings according to <list>:\n\
        \        +<spec>   enable warnings in <spec>\n\
        \        -<spec>   disable warnings in <spec>\n\
        \        @<spec>   enable warnings in <spec> and treat them as errors\n\
        \     <spec> can be:\n\
        \        <num>             a single warning number\n\
        \        <num1>..<num2>    a range of consecutive warning numbers\n\
        \        <letter>          a predefined set\n\
        \     default setting is %S" Warnings.defaults_w );
    ( "-warn-error",
      ocaml_warnings_spec ~error:true,
      Printf.sprintf
        "<list> Enable or disable error status for warnings according\n\
        \     to <list>.  See option -w for the syntax of <list>.\n\
        \     Default setting is %S" Warnings.defaults_warn_error );
    ( "-alert",
      ocaml_alert_spec,
      Printf.sprintf
        "<list>  Enable or disable alerts according to <list>:\n\
        \        +<alertname>  enable alert <alertname>\n\
        \        -<alertname>  disable alert <alertname>\n\
        \        ++<alertname> treat <alertname> as fatal error\n\
        \        --<alertname> treat <alertname> as non-fatal\n\
        \        @<alertname>  enable <alertname> and treat it as fatal error\n\
        \    <alertname> can be 'all' to refer to all alert names" );
    ( "-cmi-file",
      Marg.param "file" (fun cmi_file ocaml ->
          { ocaml with cmi_file = Some cmi_file }),
      "<file>  Use the <file> interface to type-check" );
    ( "-parameter",
      Marg.param "module name" (fun parameter ocaml ->
          { ocaml with parameters = parameter :: ocaml.parameters }),
      "<module name> Compile the module with <module name> as a parameter." );
    ( "-as-parameter",
      Marg.unit (fun ocaml -> { ocaml with as_parameter = true }),
      " Compile the interface as a parameter module." );
    ( "-as-argument-for",
      Marg.param "module name" (fun as_argument_for ocaml ->
          { ocaml with as_argument_for = Some as_argument_for }),
      "<module name> Compile the module as an argument for the named parameter."
    );
    ( "-zero-alloc-check",
      Marg.param "string" (fun zero_alloc_str ocaml ->
          match Zero_alloc_annotations.Check.of_string zero_alloc_str with
          | Some zero_alloc_check -> { ocaml with zero_alloc_check }
          | None ->
            failwith ("Invalid value for -zero-alloc-check: " ^ zero_alloc_str)),
      " Check that annotated functions do not allocate and do not have \
       indirect calls. " ^ Zero_alloc_annotations.Check.doc );
    ( "-zero-alloc-assert",
      Marg.param "string" (fun zero_alloc_str ocaml ->
          match Zero_alloc_annotations.Assert.of_string zero_alloc_str with
          | Some zero_alloc_assert -> { ocaml with zero_alloc_assert }
          | None ->
            failwith ("Invalid value for -zero-alloc-assert: " ^ zero_alloc_str)),
      " Add zero_alloc annotations to all functions. "
      ^ Zero_alloc_annotations.Assert.doc );
    ( "-infer-with-bounds",
      Marg.unit (fun ocaml -> { ocaml with infer_with_bounds = true }),
      "Infer with-bounds on kinds for type declarations. May impact \
       performance." )
  ]

(** {1 Main configuration} *)

let initial =
  { ocaml =
      { include_dirs = [];
        hidden_dirs = [];
        no_std_include = false;
        unsafe = false;
        classic = false;
        principal = false;
        real_paths = true;
        threads = `None;
        recursive_types = false;
        strict_sequence = false;
        applicative_functors = true;
        nopervasives = false;
        strict_formats = false;
        open_modules = [];
        ppx = [];
        pp = None;
        warnings = Warnings.backup ();
        cmi_file = None;
        parameters = [];
        as_parameter = false;
        as_argument_for = None;
        zero_alloc_check = Zero_alloc_annotations.Check.Check_default;
        zero_alloc_assert = Zero_alloc_annotations.Assert.Assert_default;
        infer_with_bounds = false
      };
    merlin =
      { build_path = [];
        source_path = [];
        hidden_build_path = [];
        hidden_source_path = [];
        cmi_path = [];
        cmt_path = [];
        index_files = [];
        extensions = [];
        suffixes = [ (".ml", ".mli"); (".re", ".rei") ];
        stdlib = None;
        source_root = None;
        unit_name = None;
        unit_name_for = String.Map.empty;
        wrapping_prefix = None;
        reader = [];
        protocol = `Json;
        log_file = None;
        log_sections = [];
        config_path = None;
        exclude_query_dir = false;
        use_ppx_cache = false;
        flags_to_apply = [];
        flags_applied = [];
        failures = [];
        extension_to_reader = [ (".re", "reason"); (".rei", "reason") ];
        cache_lifespan = 5
      };
    query =
      { filename = "*buffer*";
        directory = Sys.getcwd ();
        verbosity = Verbosity.default;
        printer_width = 0
      }
  }

let parse_arguments ~wd ~warning local_spec args t local =
  let_ref cwd (Some wd) @@ fun () ->
  Marg.parse_all ~warning arguments_table local_spec args t local

let global_flags =
  [ ( "-filename",
      marg_path (fun path t ->
          let query = t.query in
          let path = Misc.canonicalize_filename path in
          let filename = Filename.basename path in
          let directory = Filename.dirname path in
          let t = { t with query = { query with filename; directory } } in
          Logger.with_log_file t.merlin.log_file ~sections:t.merlin.log_sections
          @@ fun () -> get_external_config path t),
      "<path> Path of the buffer; extension determines the kind of file \
       (interface or implementation), basename is used as name of the module \
       being definer, directory is used to resolve other relative paths" );
    ( "-dot-merlin",
      marg_path (fun dotmerlin t -> get_external_config dotmerlin t),
      "<path> Load <path> as a .merlin; if it is a directory, look for .merlin \
       here or in a parent directory" )
  ]

let () =
  List.iter
    ~f:(fun name -> Hashtbl.add arguments_table name Marg.unit_ignore)
    ocaml_ignored_flags;
  List.iter
    ~f:(fun name -> Hashtbl.add arguments_table name Marg.param_ignore)
    ocaml_ignored_parametrized_flags;
  let lens prj upd flag : _ Marg.t =
   fun args a ->
    let cwd' =
      match !cwd with
      | None when a.query.directory <> "" -> Some a.query.directory
      | cwd -> cwd
    in
    let_ref cwd cwd' @@ fun () ->
    let args, b = flag args (prj a) in
    (args, upd a b)
  in
  let add prj upd (name, flag, _doc) =
    if Hashtbl.mem arguments_table name then
      failwith ("Duplicate flag spec: " ^ name);
    Hashtbl.add arguments_table name (lens prj upd flag)
  in
  List.iter
    ~f:(add (fun x -> x.ocaml) (fun x ocaml -> { x with ocaml }))
    ocaml_flags;
  List.iter
    ~f:(add (fun x -> x.merlin) (fun x merlin -> { x with merlin }))
    merlin_flags;
  List.iter
    ~f:(add (fun x -> x.query) (fun x query -> { x with query }))
    query_flags;
  List.iter ~f:(add (fun x -> x) (fun _ x -> x)) global_flags

let flags_for_completion () =
  List.sort ~cmp:compare
    ("-dot-merlin" :: "-reader" :: List.map ~f:(fun (x, _, _) -> x) ocaml_flags)

let document_arguments oc =
  let print_doc flags =
    List.iter
      ~f:(fun (name, _flag, doc) -> Printf.fprintf oc "  %s\t%s\n" name doc)
      flags
  in
  output_string oc "Flags affecting Merlin:\n";
  print_doc merlin_flags;
  print_doc query_flags;
  output_string oc "Flags affecting OCaml frontend:\n";
  print_doc ocaml_flags;
  output_string oc
    "Flags accepted by ocamlc and ocamlopt but not affecting merlin will be \
     ignored.\n"

let source_path config =
  let stdlib = if config.ocaml.no_std_include then [] else [ stdlib config ] in
  List.concat
    [ [ config.query.directory ];
      stdlib;
      config.merlin.source_path;
      config.merlin.hidden_source_path
    ]
  |> List.filter_dup

let hidden_source_path config =
  config.merlin.hidden_source_path @ config.ocaml.hidden_dirs

let collect_paths ~log_title ~config paths =
  let dirs =
    match config.ocaml.threads with
    | `None -> config.ocaml.include_dirs
    | `Threads -> "+threads" :: config.ocaml.include_dirs
    | `Vmthreads -> "+vmthreads" :: config.ocaml.include_dirs
  in
  let dirs = paths @ dirs in
  let stdlib = stdlib config in
  let exp_dirs = List.map ~f:(Misc.expand_directory stdlib) dirs in
  let stdlib = if config.ocaml.no_std_include then [] else [ stdlib ] in
  let dirs = List.rev_append exp_dirs stdlib in
  let result =
    if config.merlin.exclude_query_dir then dirs
    else config.query.directory :: dirs
  in
  let result = List.filter_dup result in
  log ~title:log_title "%d items in path, %d after deduplication"
    (List.length result) (List.length result);
  result

let build_path config =
  collect_paths ~log_title:"build_path" ~config
    (config.merlin.cmi_path @ config.merlin.build_path)

let hidden_build_path config =
  config.merlin.hidden_build_path @ config.ocaml.hidden_dirs

let cmt_path config =
  collect_paths ~log_title:"cmt_path" ~config
    (config.merlin.cmt_path @ config.merlin.build_path
   @ config.merlin.hidden_build_path)

let global_modules ?(include_current = false) config =
  let modules = Misc.modules_in_path ~ext:".cmi" (build_path config) in
  if include_current then modules
  else
    match config.query.filename with
    | "" -> modules
    | filename -> List.remove (Misc.unitname filename) modules

(** {1 Accessors for other information} *)

let filename t = t.query.filename

let unitname t =
  match t.merlin.unit_name with
  | Some name -> Misc.unitname name
  | None ->
    let basename = Misc.unitname t.query.filename in
    (* CR: get rid of wrapping_prefix. it is only here for legacy reasons at the moment *)
    begin
      match t.merlin.wrapping_prefix with
      | Some prefix -> Misc.unitname (prefix ^ basename)
      | None ->
        String.Map.find_opt basename t.merlin.unit_name_for
        |> Option.map ~f:Misc.unitname
        |> Option.value ~default:basename
    end
