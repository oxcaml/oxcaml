(* TEST
 include stdlib_beta;
 flambda2;
 {
   flags = "-extension layouts_alpha -extension small_numbers_beta";
   native;
 }{
   flags = "-extension layouts_alpha -extension small_numbers_beta";
   bytecode;
 }
*)

(* This should be read as a continuation of the [unboxed_bits8s.ml] test.
   We can't put them there because:
     - [unboxed_bits8s.ml] is run at all maturities, but
     - these tests use features that only are enabled at the alpha maturity.

   Once mixed blocks move to the "stable" maturity level, we can
   move these tests there.
 *)

(*****************************************)
(* Prelude: Functions on unboxed int8s. *)

module Int8 = Stdlib_beta.Int8

module Int8_u = struct
  include Stdlib_beta.Int8_u

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( // ) = unsigned_div
  let ( % ) = rem
  let ( %% ) = unsigned_rem
  let ( > ) x y = (compare x y) > 0
end

let print_float prefix x = Printf.printf "%s: %.2f\n" prefix x

let to_binary_string x =
  String.init 8 (fun i ->
      if Int.(equal (logand x (shift_left 1 (8 - i - 1))) 0)
      then '0'
      else '1')

let print_int prefix x =
  Printf.printf "%s: %d\n" prefix x

let print_int8u prefix x =
  Printf.printf "%s: %d\n" prefix (Int8_u.to_int x)

let print_int8u_bin prefix x =
  let bx = Int8_u.to_int x in
  Printf.printf "%s: %d = 0b%s\n" prefix bx (to_binary_string bx)

(***********************************)
(* Test: mixed blocks in closures *)

(* Adapted from Test 3 in unboxed_bits8s.ml *)

type block =
  { x0_1 : int;
    x0_2 : int;
    x1 : int8#;
    x2_1 : int;
    x2_2 : int;
    x3 : int8#;
    x4_1 : int;
    x4_2 : int;
    x5 : int8#;
    x6_1 : int;
    x6_2 : int;
    x7 : int8#;
    x8_1 : int;
    x8_2 : int;
    x9 : int8#;
  }

let[@inline_never] f_mixed_blocks_and_closures
    steps ({ x1; x0_1 = start_k; x0_2 = end_k; x8_1; x8_2;
             x5; x6_1; x6_2; } as iargs) () =
  let[@inline never] rec go k =
    if k = end_k
    then (Int8_u.of_int (0))
    else begin
      let (x2_1, x2_2) = iargs.x2_1, iargs.x2_2 in
      let {x4_1; x4_2; _} = iargs in
      let sum = x2_1 + x2_2 + x4_1 + x4_2 + x6_1 + x6_2 + x8_1 + x8_2 in
      let acc = go (k + 1) in
      steps.(k) <- Int8_u.to_int8 acc;
      Int8_u.(acc + ((x1 + iargs.x3 + x5 + iargs.x7 + iargs.x9)
                      * (of_int8 (Int8.of_int sum))))
    end
  in
  go start_k

let test_mixed_blocks_and_closures () =
  (* Test f_mixed_blocks_and_closures

          (1 + 2 + 3 + 5 + 8) = 19
      3 * (1 + 2 + 3 + 5 + 8) = 57
      6 * (1 + 2 + 3 + 5 + 8) = 114
      9 * (1 + 2 + 3 + 5 + 8) = 171
  *)
  let steps = Array.init 10 (fun _ -> (Int8.of_int (0))) in
  let x1 = (Int8_u.of_int (1)) in
  let x3 = (Int8_u.of_int (2)) in
  let x5 = (Int8_u.of_int (3)) in
  let x7 = (Int8_u.of_int (5)) in
  let x9 = (Int8_u.of_int (8)) in

  (* all these 8 numbers together sum to 3 *)
  let x2_1, x2_2 = (7, 42) in
  let x4_1, x4_2 = (-23, 109) in
  let x6_1, x6_2 = (-242, 90) in
  let x8_1, x8_2 = (-2, 22) in

  let f = f_mixed_blocks_and_closures steps
      { x0_1 = 4; x0_2 = 8; x1; x2_1; x2_2; x3; x4_1; x4_2; x5;
        x6_1; x6_2; x7; x8_1; x8_2; x9 }
  in
  print_int8u "Test mixed_blocks_with_closures, 171: " (f ());
  Array.iteri (fun i x -> Printf.printf "  Test mixed_blocks_with_closures, step %d: %d\n" i (Int8.to_int x)) steps

let _ = test_mixed_blocks_and_closures ()

(**************************************)
(* Test: mixed record manipulation *)

type t_mixed_record =
  { a : float;
    mutable b : int;
    c : int8#;
    mutable d : int8#;
    e : int;
    mutable f : int8# }

(* Construction *)
let t_mixed1 = { a = 317.;
              b = 1300;
              c = (Int8_u.of_int (731));
              d = (Int8_u.of_int (141));
              e = 600;
              f = (Int8_u.of_int (2710));
            }

let t_mixed2 = { a = (-317.);
              b = -1300;
              c = (Int8_u.of_int (-731));
              d = (Int8_u.of_int (-141));
              e = -600;
              f = (Int8_u.of_int (-2710));
            }

let print_t_mixed t =
  print_float "  a" t.a;
  print_int "  b" t.b;
  print_int8u "  c" t.c;
  print_int8u "  d" t.d;
  print_int "  e" t.e;
  print_int8u "  f" t.f

let _ =
  Printf.printf "Test mixed record construction:\n";
  print_t_mixed t_mixed1;
  print_t_mixed t_mixed2

(* Matching, projection *)
let f_mixed1 {c; d; f; _} r =
  match r with
  | { a; _ } ->
    { a = Float.of_int r.e;
      b = Int8_u.(to_int (of_float a - d));
      c = Int8_u.(r.c + c);
      d = Int8_u.(d - (of_int r.b));
      e = Int8_u.(to_int (f + (of_int r.e)));
      f = r.f;
    }

let _ =
  Printf.printf "Test mixed record matching and projection:\n";
  print_t_mixed (f_mixed1 t_mixed1 t_mixed2)

(* Record update and mutation *)
let f_mixed2 ({a; d; _} as r1) r2 =
  r1.d <- (Int8_u.of_int (4200));
  let r3 = { r2 with c = r1.d;
                     d = (Int8_u.of_int (2500)); }
  in
  r3.b <- Int8_u.(to_int (of_float a + d));
  r2.b <- 1700;
  r1.f <- r2.c;
  r3

let _ =
  Printf.printf "Test mixed record update and mutation:\n";
  let t_mixed3 = f_mixed2 t_mixed1 t_mixed2 in
  print_t_mixed t_mixed1;
  print_t_mixed t_mixed2;
  print_t_mixed t_mixed3

(**************************************)
(* Test: mixed constructor manipulation *)

type t_mixed_variant =
  | Const
  | T of
      { a : float;
        mutable b : int;
        c : int8#;
        mutable d : int8#;
        e : int;
        mutable f : int8# }

(* Construction *)
let t_mixed_variant1 = T
            { a = 317.;
              b = 1300;
              c = (Int8_u.of_int (731));
              d = (Int8_u.of_int (141));
              e = 600;
              f = (Int8_u.of_int (2710));
            }

let t_mixed_variant2 = T
            { a = (-317.);
              b = -1300;
              c = (Int8_u.of_int (-731));
              d = (Int8_u.of_int (-141));
              e = -600;
              f = (Int8_u.of_int (-2710));
            }

let[@warning "-partial-match"] print_t_mixed_variant (T t) =
  print_float "  a" t.a;
  print_int "  b" t.b;
  print_int8u "  c" t.c;
  print_int8u "  d" t.d;
  print_int "  e" t.e;
  print_int8u "  f" t.f

let _ =
  Printf.printf "Test mixed variant construction:\n";
  print_t_mixed_variant t_mixed_variant1;
  print_t_mixed_variant t_mixed_variant2

(* Matching, projection *)
let[@warning "-partial-match"] f_mixed1 (T {c; d; f; _}) r =
  match r with
  | T ({ a; _ } as r) ->
    T
      { a = Float.of_int r.e;
        b = Int8_u.(to_int (of_float a - d));
        c = Int8_u.(r.c + c);
        d = Int8_u.(d - (of_int r.b));
        e = Int8_u.(to_int (f + (of_int r.e)));
        f = r.f;
      }

let _ =
  Printf.printf "Test mixed variant matching and projection:\n";
  print_t_mixed_variant (f_mixed1 t_mixed_variant1 t_mixed_variant2)

(* Variant update and mutation *)
let[@warning "-partial-match"] f_mixed2 (T ({a; d; _} as r1)) (T r2) =
  r1.d <- (Int8_u.of_int (4200));
  let T r3 = T { r2 with c = r1.d;
                         d = (Int8_u.of_int (2500)); }
  in
  r3.b <- Int8_u.(to_int (of_float a + d));
  r2.b <- 1700;
  r1.f <- r2.c;
  T r3

let _ =
  Printf.printf "Test mixed variant update and mutation:\n";
  let t_mixed_variant3 = f_mixed2 t_mixed_variant1 t_mixed_variant2 in
  print_t_mixed_variant t_mixed_variant1;
  print_t_mixed_variant t_mixed_variant2;
  print_t_mixed_variant t_mixed_variant3


(************************************************************)
(* Test mixed records in recursive groups *)

let rec f r =
  r.d <- Int8_u.of_int t_rec1.b;
  t_rec2.b <- 42;
  t_rec1.f <- Int8_u.of_float t_rec1.a;
  Int8_u.(of_float r.a + of_float t_rec2.a)


and t_rec1 = { a = 11.;
              b = 2;
              c = (Int8_u.of_int (33));
              d = (Int8_u.of_int (44));
              e = 5;
              f = (Int8_u.of_int (66));
  }

and t_rec2 = { a = (- 51.);
              b = -6;
              c = (Int8_u.of_int (-73));
              d = (Int8_u.of_int (-84));
              e = -9;
              f = (Int8_u.of_int (-106));
            }

let _ =
  Printf.printf "Test 18, mixed records in recursive groups:\n";
  print_t_mixed t_rec1;
  print_t_mixed t_rec2;
  let result = f t_rec1 in
  print_int8u "  result (-40)" result;
  print_t_mixed t_rec1;
  print_t_mixed t_rec2
