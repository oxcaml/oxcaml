(* TEST
   flags = "-extension layouts_beta";
   setup-ocamlc.byte-build-env;
   ocamlc_byte_exit_status = "2";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

(* [module type of] defaults the unfilled sorts in a module's types, which makes
   typechecking that tracks sorts variables, including expressions with
   any-records, order-dependent and incomplete. *)

type ('a : any) t = { x : 'a }

(* Without [module type of] *)
module M_ok = struct
  let f x = { x }
end

let _ = M_ok.f #3.14

(* Same example, but with a defaulting [module type of] in the middle *)
module M = struct
  let f x = { x }
end

module type S = module type of M

let _ = M.f #3.14
