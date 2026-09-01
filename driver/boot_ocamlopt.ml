(* Eta-expansion gives [create_process] and [waitpid] the unannotated types
   required by [Compiler_owee.Unix_intf.S]. *)
module Unix_for_owee = struct
  include Unix

  let create_process prog args stdin stdout stderr =
    Unix.create_process prog args stdin stdout stderr

  let waitpid flags pid = Unix.waitpid flags pid
end

let () =
  exit (Optmaindriver.main (module Unix_for_owee : Compiler_owee.Unix_intf.S)
    Sys.argv Format.err_formatter
    ~flambda2:Flambda2.lambda_to_cmm
    ~reaped_flambda2_to_cmm:Flambda2.reaped_flambda2_to_cmm
    ~reaper_lto_solve:Flambda2.reaper_lto_solve)
