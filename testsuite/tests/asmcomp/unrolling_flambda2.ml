(* TEST
   flambda;
   native;
*)

type t = { fn : t -> t -> int -> unit -> unit }

let rec foo f b n x =
  if n < 0
  then ()
  else (
    foo f b (n - 1) x;
    b.fn f b (n - 1) x)
  [@@specialise always]

let rec bar f b n x =
  if n < 0
  then ()
  else (
    bar f b (n - 1) x;
    f.fn f b (n - 1) x)
  [@@specialise always]

let () = foo { fn = foo } { fn = bar } 10 ()
