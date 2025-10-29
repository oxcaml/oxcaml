(* TEST
 flambda;
 ocamlopt_flags = "-Oclassic";
 exit_status = "2";
 native;
*)

(* Test case from Issue #4803 on the oxcaml repo. *)

type ('a, 'b) eq = Refl : ('a, 'a) eq

type 'a t =
  A : int t
| B : int -> string t

let[@inline never][@local never] f : unit -> (('a, string) eq * int) = fun () -> assert false
let[@inline never][@local never] h x = ()

external (+) : int -> int -> int = "%addint"

let[@inline never][@local never] g (type a) (x : a t) =
  let (z : ((a, string) eq * int) * int) @ global = (f (), 1) in
  let (Refl, _), _ = z in
  match x with
  | B s ->
    let b = s + s in
    let res = (z, 1) in
    b, res

let u = g A
