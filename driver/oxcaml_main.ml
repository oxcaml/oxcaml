external gc_set_idle_floor : int -> unit = "caml_gc_set_idle_floor"

let () = Optmaindriver.Gc_pacing.set_idle_floor_hook := gc_set_idle_floor

let () =
  (match Sys.backend_type with
   | Native -> Memtrace.trace_if_requested ~context:"ocamlopt" ()
   | Bytecode | Other _ -> ());
  exit (Optmaindriver.main (module Unix : Compiler_owee.Unix_intf.S)
    Sys.argv Format.err_formatter
    ~flambda2:Flambda2.lambda_to_cmm)
