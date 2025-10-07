let () =
  Memtrace.trace_if_requested ();
  exit
    (Optmaindriver.main
       (module Unix : Compiler_owee.Unix_intf.S)
       Sys.argv Format.err_formatter
       ~flambda2:(module Flambda2))
