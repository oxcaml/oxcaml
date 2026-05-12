let () =
  Typeopt.use_shallow_value_kinds := Flambda2_ui.Flambda_features.classic_mode;
  (match Sys.backend_type with
   | Native -> Memtrace.trace_if_requested ~context:"ocamlopt" ()
   | Bytecode | Other _ -> ());
  exit (Optmaindriver.main (module Unix : Compiler_owee.Unix_intf.S)
    Sys.argv Format.err_formatter
    ~flambda2:Flambda2.lambda_to_cmm)
