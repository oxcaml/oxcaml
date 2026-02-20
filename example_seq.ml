
module Seq = struct
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

end

let[@inline] square x = x * x

let[@inline] ints lo hi =
  Seq.unfold (fun i -> if i > hi then Seq.Empty else Seq.Cons (i, i + 1)) lo

let[@inline] sum s =
  Seq.fold_left (+) 0 s

let foo () =
  sum (Seq.map square (Seq.filter (fun i -> i > 5) (ints 0 11)))
