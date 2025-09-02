(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 {
   setup-ocamlc.opt-build-env;
   ocamlc_opt_exit_status = "2";
   ocamlc.opt;
   check-ocamlc.opt-output;
 }{
   setup-ocamlopt.opt-build-env;
   ocamlopt_opt_exit_status = "2";
   ocamlopt.opt;
   check-ocamlopt.opt-output;
 }
*)

module type Id = sig
  type t : bits64
  val null : t
  val gen : unit -> t
  val validate : t -> bool
end

module Make_id () : Id = struct
  type t = int64#
  let null = #0L
  let gen () = #1234L
  let validate _id = true
end

module Id_1 = Make_id ()
module Id_2 = Make_id ()

let _ = Id_2.validate (Id_1.gen ())
