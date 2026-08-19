(* TEST
 flags = "-extension layouts_beta -extension mode -flambda2-kind-checks";
 flambda2;
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
*)

(* Concrete same-sort normal exits (all sort value) reached through an
   [exclave_] branch under a local region compile cleanly. *)

type (_ : any) w =
  | A : int w
  | B : string w

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

let[@inline never] f : type (a : any). a w -> a @ local =
  fun w ->
    let local_ x = ref 0 in
    let local_ _y = opaque_local x in
    match w with
    | A -> exclave_ 0
    | B -> "x"
