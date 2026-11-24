(* TEST
 flambda;
 ocamlopt_flags = "-O3 -flambda2-match-in-match";
 native;
*)

(* This test need to *not* be in classic mode to run,
   since it requires match-in-match/continuation specialization *)
[@@@ocaml.flambda_o3]

(* The region of [g], once inlined in the loop of [f], is begun in a handler
   that match-in-match specialises, and is ended in a continuation that is
   lifted out of that handler. The lifted continuation thus takes the region as
   a parameter, for which each of the specialised copies passes its own
   region. This parameter is only used to end the region, and the region must
   nonetheless be ended. *)

external local_stack_offset : unit -> int = "caml_local_stack_offset"

type t = A of int | B of int

let[@inline never] use (local_ (o : int option)) =
  match o with Some v -> v | None -> 0

let[@inline always] g x n =
  let r = stack_ (Some n) in
  match x with A a -> use r + a | B b -> use r + (2 * b)

let[@inline never] f n =
  let s = ref 0 in
  for i = 1 to n do
    let x = if i land 1 = 0 then A i else B i in
    s := !s + g x i
  done;
  !s

let () =
  let before = local_stack_offset () in
  let result = f 1000 in
  let after = local_stack_offset () in
  assert (result = 1251000);
  assert (before = after)
