let () =
  exit (Optmaindriver.main (module Unix : Compiler_owee.Unix_intf.S) Sys.argv
    Format.err_formatter
    ~flambda2:(module Flambda2))
