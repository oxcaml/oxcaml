(* TEST
   flambda2;
   flags += "-flambda2-reaper";
   { native; }
 *)

type 'a t = A : (int -> int) t | B : int ref t

let[@inline never][@local never] f (type a) (u : a t) (x : a) =
  match u with
  | A -> x 0
  | B -> !x

let f x =
  f A (fun y -> y + x)