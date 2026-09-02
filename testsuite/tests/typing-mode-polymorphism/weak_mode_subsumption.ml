(* TEST
 flags += "-extension mode_polymorphism_alpha";
 setup-ocamlopt.byte-build-env;
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Same unsoundness as curry_mode_subsumption.ml, reached through a weak value
   instead of a polymorphic one: [h] is not generalized, so the modes in its
   type are level-0 variables that are still flexible when [Bad_client] is
   checked. Without subsumption the inclusion is wrongly accepted, [force] then
   constrains [h] to be local, and [keep] ends up typed as a global closure
   over a local one.

   With subsumption, the inclusion is rejected. *)

module Producer = struct
  let f (x : string) (y : int) = y
end

let mk () = fun (x : string) (y : int) -> y

let h = mk ()

module Bad_client : module type of Producer = struct
  let f = h
end

let keep = Bad_client.f "hi"

let force (s @ local) = h s 0
