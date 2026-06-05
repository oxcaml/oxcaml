(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -flambda2-join-algorithm=n-way";
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
 *)

[@@@ocaml.flambda_o3]

external __dummy2__ : unit -> 'a = "%opaque"

type t = TSeq of unit | TExp of unit

let _ =
 fun z ->
  (let x = __dummy2__ () in
   fun y ->
     (__dummy2__ ())
       ((fun y ->
          match x with
          | None -> None
          | Some (TExp _) -> Some (TExp y)
          | _ -> Some (TSeq y))
          y))
    z ()

