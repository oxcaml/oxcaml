(* TEST
 flags += "-extension mode_polymorphism_alpha";
 setup-ocamlopt.byte-build-env;
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* Without rigid subsumption, [Bad_client] is wrongly accepted: its [f] is
   compiled with a local curry mode while the signature promises the general
   calling convention. Flambda then flags the partial application below as
   invalid code (Partial_application_mode_mismatch_in_lambda).

   With subsumption, the inclusion is rejected. *)

module Producer = struct
  let f x y = y
end

module Bad_client : module type of Producer = struct
  let f (x @ local) y = y
end

let keep = Bad_client.f 1

let () = assert (keep 2 = 2)
