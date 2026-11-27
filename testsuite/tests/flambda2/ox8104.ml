(* TEST
   flambda2;
   ocamlopt_flags += "-O3 -flambda2-join-algorithm=n-way";
   native;
 *)

(* from https://github.com/oxcaml/oxcaml/issues/8104 *)

type y = Y0 | Y1 | Y2
type v = V0 of int | V1 of int
type p = P0 of v | P1 of int
type 'a box = { b : 'a }

exception Stop of int

let[@inline never] f c d g n =
  let base = match d with Y0 -> 1000 | Y1 -> 2000 | Y2 -> 3000 in
  let p =
    if c then
      let x = match d with Y0 -> V0 n | Y1 -> V1 n | Y2 -> raise (Stop 99) in
      { b = { b = { b = P0 x } } }
    else { b = { b = { b = P1 n } } }
  in
  let s =
    if g then p
    else
      match d with
      | Y0 -> { b = { b = { b = P0 (V0 (n + 1)) } } }
      | Y1 -> raise (Stop 77)
      | Y2 -> raise (Stop 78)
  in
  let r = match d with Y0 -> n | Y1 -> n * 3 | Y2 -> n * 7 in
  base + r + match s.b.b.b with P0 _ -> 10 | P1 _ -> 20

let () =
  let o = Sys.opaque_identity in
  Printf.printf "result %d\n" (f (o false) (o Y2) (o true) (o 5))
