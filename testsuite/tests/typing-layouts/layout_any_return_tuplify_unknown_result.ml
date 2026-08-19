(* TEST
 flags = "-extension layouts_beta";
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }
*)

module Float_u = Stdlib_upstream_compatible.Float_u
module Int64_u = Stdlib_upstream_compatible.Int64_u

type product = #(int * Float_u.t * string * Int64_u.t)

let check_float_u expected actual =
  assert (Float.equal expected (Float_u.to_float actual))

let check_product (#(i, f, s, i64) : product) =
  assert (Int.equal 23 i);
  check_float_u 6.5 f;
  assert (String.equal "tuplify" s);
  assert (String.equal "29" (Int64_u.to_string i64))

let[@inline never] tupled_forward
    : type (a : any).
      ( (unit -> a)
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int
      * int )
      -> a =
 fun
   ( f,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _,
     _ )
   -> f ()

let[@inline never] call_tupled
    : type (a : any).
      ( ( (unit -> a)
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int
        * int )
        -> a )
      -> (unit -> a)
      -> a =
 fun forward f ->
  forward
    ( f,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11,
      12,
      13,
      14,
      15,
      16,
      17,
      18,
      19,
      20,
      21,
      22,
      23,
      24 )

let[@inline never] return_int () = 42

let[@inline never] return_float () = #3.5

let[@inline never] return_product () = #(23, #6.5, "tuplify", #29L)

let use_int () = call_tupled tupled_forward return_int

let use_float () = call_tupled tupled_forward return_float

let use_product () = call_tupled tupled_forward return_product

let () =
  assert (use_int () = 42);
  check_float_u 3.5 (use_float ());
  check_product (use_product ())
