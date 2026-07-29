let () =
  exit (Optmaindriver.main (module Unix : Compiler_owee.Unix_intf.S) Sys.argv
    Format.err_formatter
    ~flambda2:Flambda2.lambda_to_cmm
    ~reaped_flambda2_to_cmm:Flambda2.reaped_flambda2_to_cmm)
