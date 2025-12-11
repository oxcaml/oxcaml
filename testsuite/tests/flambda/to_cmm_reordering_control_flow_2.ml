(* TEST
 flambda;
 ocamlopt_flags = "-Oclassic -flambda2-expert-cmm-safe-subst";
 exit_status = "2";
 native;
*)

(* Test case from Issue #4803 on the oxcaml repo. *)

type ('a, 'b) eq = Refl : ('a, 'a) eq

type 'a t =
  A : float t
| B : int -> int t

let[@inline never][@local never] f : unit -> (('a, int) eq * int) = fun () -> Gc.full_major (); assert false
let[@inline never][@local never] h x = ()

external (+) : int -> int -> int = "%addint"

let[@inline never][@local never] g (type a) (x : a t) (y : a) =
  let (z : ((a, int) eq * int) * int) @ global = (f (), 1) in
  let (Refl, _), _ = z in
  match x with
  | B _ ->
    let b = y + y + y in
    let res = (z, 1) in
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    b, b, b, b, b, b, b, b, b, b,
    res

let u =
  g A 5.


