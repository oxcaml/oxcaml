(* TEST
   flambda2;
   flags += "-flambda2-reaper";
   { native; }
 *)

type r = { a : int; b : int; mutable c : int } 
type 'a t = A : (int * int) t | B : r t

let[@inline never][@local never] f (type a) (u : a t) (x : a) =
  match u with
  | A -> let (a, b) = x in a + b
  | B -> let { a; b; c } = x in a + b + c

let f x = f A (x, x)