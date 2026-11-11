(* TEST
 flambda2;
 flags += "-O3 -flambda2-reaper -reaper-debug-flags=nostamps";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-reaper;
 check-fexpr-dump;
*)

let run b x =
  let[@inline never][@local never] body return =
    match if b then Some x else return 0 with
    | None -> 0
    | Some x -> x
  in
  body (fun x -> raise Exit)
