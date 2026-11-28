(* TEST *)

type (_, _) eq = Eq : ('a, 'a) eq

let[@inline always] cast (type a b) (x : a) (Eq : (a, b) eq) : b = x

let test (f : 'a @ local -> 'b @ local) (eq : ('a @ local -> 'b @ local, 'a -> 'b) eq) (x : 'a) : 'b =
  (cast f eq) x 

type 'a box = Box of 'a

let localf : ('a @ local -> 'a box @ local) = fun x -> exclave_ (stack_ (Box x))

let g eq x = test localf eq x