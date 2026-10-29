(* Mode annotations and the legacy mode keywords are erased. *)

let f x = exclave_ (x, x)

let g () = local_ (1, 2)

let h () = stack_ (3, 4)

let k (x @ local) = x + 1

let m (x : int @ local) : int = x

let r x =
  let y = x in
  exclave_ y

let rec map (f @ local) = function [] -> [] | x :: xs -> f x :: map f xs

let q = fun (type a : value) (x : a) -> x

type t = { global_ a : int }
