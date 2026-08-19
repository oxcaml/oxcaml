(* TEST
 flags = "-extension layouts_beta";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* Calls to unknown- or bottom-result functions inside a try-with whose
   exception continuation carries extra arguments (here, from the elimination
   of the mutable ref) cannot be inlined: the inliner must decline instead of
   failing, in both Simplify and classic mode. *)

let never : type (a : any). unit -> a = fun () -> assert false

let go n =
  let r = ref n in
  try (never () : int) with _ -> !r

let[@inline never] ret_int () = Sys.opaque_identity 26

let[@inline always] fwd : type (a : any). (unit -> a) -> a = fun f -> f ()

let go_fwd n =
  let r = ref n in
  try (fwd ret_int : int) + !r with Not_found -> 0

exception E of int

let[@inline never] thrower n : unit = raise (E n)

let never_exn : type (a : any). int -> a = fun n -> thrower n; assert false

let go_match n =
  let r = ref n in
  match (never_exn !r : int) with x -> x | exception E m -> m + !r

let () =
  assert (go 21 = 21);
  assert (go_fwd 16 = 42);
  assert (go_match 5 = 10)
