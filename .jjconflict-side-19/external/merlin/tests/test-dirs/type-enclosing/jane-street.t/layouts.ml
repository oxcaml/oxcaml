type t0 = int
type t1 : immediate = int
type t2 : immediate = A | B | C
type t3 : value = D | E of int
type t4 : immediate64 = A
type t5 : value
type t6 : value = { v : int } [@@unboxed]

let f_int (x : int) = x
let f0 (x : t0) = x
let f1 (x : t1) = x
let f2 (x : t2) = x
let f3 (x : t3) = x
let f4 (x : t4) = x
let f5 (x : t5) = x
let f6 (x : t6) = x

let poly1 (type a)           (x : a) = x
let poly2 (type a : value)   (x : a) = x
let poly3 (type a : float64) (x : a) = x
let poly4 (type (a : immediate) (b : value)) (f : a -> b -> _) = f

let poly_client1 x = poly1 x
let poly_client2 x = poly2 x
let poly_client3 x = poly3 x
let poly_client4 x = poly4 x

type _                p0 = A0
type 'a               p1 = A1 of 'a
type ('a : immediate) p2 = A2 of 'a [@@unboxed]

let param_client1 (x : 'a p0) (a : 'a) = x, a
let param_client2 (x : 'a p1) (a : 'a) = x, a
let param_client2 (x : 'a p2) (a : 'a) = x, a

let long_type a b c d e f g =
  A2 a,
  A2 b,
  A2 c,
  A2 d,
  A2 e,
  A2 f,
  A2 g,
;;

let unconstrained_f x = x
