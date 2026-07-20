(* TEST
 flags = "-w -8";
 {
   native;
 }{
   bytecode;
 }
*)

(* Regression tests: an argument matched by a multi-case GADT match used to
   get its value kind from the first branch's equations, miscompiling callers
   that pick another constructor. *)

(* Regression test: total match unpacking a tuple, unarized *)
type 'a rep6 = Block6 : (int * int) rep6 | Int6 : int rep6

let[@inline never] g6 b =
  let[@local] f : type a. a rep6 * a -> int = function
    | Block6, (a, _b) -> a
    | Int6, x -> x
  in
  if b then f (Block6, (1, 2)) else f (Int6, Sys.opaque_identity 0)

let () =
  assert (g6 true = 1);
  assert (g6 false = 0)

(* Regression test: total match unpacking a tuple,
   not unarized due to the extra argument. *)
type 'a rep9 = Block9 : (int * int) rep9 | Int9 : int rep9

let[@inline never] g9 b =
  let[@local] f : type a. unit -> a rep9 * a -> int = fun () -> function
    | Block9, (a, _b) -> a
    | Int9, x -> x
  in
  if b then f () (Block9, (1, 2)) else f () (Int9, Sys.opaque_identity 0)

let () =
  assert (g9 true = 1);
  assert (g9 false = 0)

(* Regression test: total match unpacking a tuple,
   not unarized due to [as _t]. *)
type 'a rep10 = Int10 : int rep10 | Float10 : float rep10

let[@inline never] g10 b =
  let[@local] f : type a. a rep10 * a -> float = function
    | (Float10, x) as _t -> x +. 1.0
    | Int10, z -> float_of_int z
  in
  if b then f (Float10, Sys.opaque_identity 3.5)
  else f (Int10, Sys.opaque_identity 7)

let () =
  assert (g10 true = 4.5);
  assert (g10 false = 7.0)

(* Regression test: total match unpacking a tuple whose branch kinds
   disagree, compiled with the tupled calling convention. *)
type 'a rep12 = Block12 : (int * int) rep12 | Int12 : int rep12

let[@inline never] f12 : type a. a rep12 * a -> int = function
  | Block12, (a, _b) -> a
  | Int12, x -> x

let () =
  assert (f12 (Block12, (1, 2)) = 1);
  assert (f12 (Int12, Sys.opaque_identity 0) = 0)
