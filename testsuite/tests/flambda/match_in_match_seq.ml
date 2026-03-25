(* TEST
   flambda2;
   flags += "-flambda2-match-in-match -O3";
   { native with dump-raw, dump-simplify, dump-reaper; check-fexpr-dump; }
*)

(* This test is here to check that match-in-match triggers and adequately
   simplifies a test case on sequences (slightly different from the
   stdlib's sequences so that they are more easily optimized by flambda,
   while we wait for function specialization). *)

(* The signature is here to reduce the contents of the fexpr after Simplify. *)
module Seq : sig
  val foo : unit -> int
end = struct
  type ('a, 's) node = Empty | Cons of 'a * 's
  type _ t = State : ('s * ('s -> ('a, 's) node)) -> 'a t

  let[@inline] fold_left f acc (State (s, next)) =
    let[@inline][@loop always] rec aux acc s =
      match (next[@inlined hint]) s with
      | Empty -> acc
      | Cons (x, s') -> aux (f acc x) s'
    in
    aux acc s

  let[@inline] map f (State (s, next)) =
    State (s, fun s ->
      match (next[@inlined hint]) s with
      | Empty -> Empty
      | Cons (x, s') -> Cons (f x, s'))

  let[@inline] filter f (State (s, next)) =
    let[@inline][@loop always] rec aux s =
      match (next[@inlined hint]) s with
      | Empty -> Empty
      | Cons (x, s') ->
        if f x then Cons (x, s') else aux s'
    in
    State (s, aux)

  let[@inline] unfold f acc =
    State (acc, f)

  let[@inline] square x = x * x

  let[@inline] ints lo hi =
    unfold (fun i -> if i > hi then Empty else Cons (i, i + 1)) lo

  let[@inline] sum s = fold_left (+) 0 s

  let foo () =
    sum (map square (filter (fun i -> i > 5) (ints 0 11)))

end
