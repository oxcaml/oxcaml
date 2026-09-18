(* A unit that is not Closed (the alias target is missing) whose type
   members are large mutually recursive declarations: verifying a mention
   of [t12] must not re-verify a member per reference. *)
module Missing = Nosuch

type t0 = K0

and t1 =
  | A1 of t0 * t0 * t0 * t0 * t0 * t0
  | B1 of t12
  | C1

and t2 =
  | A2 of t1 * t1 * t1 * t1 * t1 * t1
  | B2 of t12
  | C2

and t3 =
  | A3 of t2 * t2 * t2 * t2 * t2 * t2
  | B3 of t12
  | C3

and t4 =
  | A4 of t3 * t3 * t3 * t3 * t3 * t3
  | B4 of t12
  | C4

and t5 =
  | A5 of t4 * t4 * t4 * t4 * t4 * t4
  | B5 of t12
  | C5

and t6 =
  | A6 of t5 * t5 * t5 * t5 * t5 * t5
  | B6 of t12
  | C6

and t7 =
  | A7 of t6 * t6 * t6 * t6 * t6 * t6
  | B7 of t12
  | C7

and t8 =
  | A8 of t7 * t7 * t7 * t7 * t7 * t7
  | B8 of t12
  | C8

and t9 =
  | A9 of t8 * t8 * t8 * t8 * t8 * t8
  | B9 of t12
  | C9

and t10 =
  | A10 of t9 * t9 * t9 * t9 * t9 * t9
  | B10 of t12
  | C10

and t11 =
  | A11 of t10 * t10 * t10 * t10 * t10 * t10
  | B11 of t12
  | C11

and t12 =
  | A12 of t11 * t11 * t11 * t11 * t11 * t11
  | B12 of t12
  | C12

