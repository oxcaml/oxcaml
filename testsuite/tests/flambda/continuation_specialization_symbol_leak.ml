(* TEST
 flambda;
 ocamlopt_flags = "-O3 -flambda2-expert-cont-lifting-budget 200 -flambda2-expert-cont-specialization-budget 20";
 native;
 javascript;
*)

(* This test need to *not* be in classic mode to run,
   since it requires match-in-match/continuation specialization *)
[@@@ocaml.flambda_o3]

(* This code triggers continuation specialization in a situation
   where the first downwards pass on the specialized continuation
   can create some lifted constants (specifically a set of closures
   in this case), constants which must be "forgotten" since flambda
   ignores that first pass when specializing a continuation.

   see https://github.com/oxcaml/oxcaml/pull/4617 *)

let[@inline never] foo _ = ()
let none = Sys.opaque_identity None

let[@inline] iter f = function
  | Some x -> f x
  | None -> ()

module Run() =
  struct
    let () =
      match none with
      | Some x -> foo x
      | None -> ()

    let () = iter (fun _filename -> ()) none
  end

