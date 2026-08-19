(* TEST
   flambda2;
   flags += "-flambda2-reaper";
   { native; }
 *)

external opaque : 'a -> 'a = "%opaque"

type 'a u = X : (int * int) u | Y : (int -> int) u

let f : type a. a u -> _ -> _ = fun u x ->
  let[@local] p : type a. a -> a u -> int = fun z u ->
    match opaque u with
    | X -> 0
    | Y -> z 0
  in
  let z : a = match opaque u with X -> (x, x) | Y -> (fun x -> x) in
  p z u
