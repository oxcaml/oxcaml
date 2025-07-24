(* TEST
   flags = "-extension let_mutable";
   include stdlib_upstream_compatible;
   expect; *)

(* Test 1.1: basic usage in a for loop *)
let foo1 y =
  let mutable x = y in
  for i = 1 to 10 do
    x <- x + i
  done;
  x

let () = assert (Int.equal (foo1 0) 55)
let () = assert (Int.equal (foo1 42) 97)

[%%expect{|
val foo1 : int -> int = <fun>
|}]

(* Test 1.2: basic usage with a nested record returning string *)
type t_1_2 = { str_1_2 : string ref }
let x_1_2 =
  let mutable x = { str_1_2 = ref "Hi" } in
  x <- { str_1_2 = ref "Bye" };
  (x.str_1_2.contents : string)
[%%expect{|
type t_1_2 = { str_1_2 : string ref; }
val x_1_2 : string = "Bye"
|}]

(* Test 1.3: returning an immutable record *)
type t_1_3 = { str_1_3 : string }
let x_1_3 =
  let mutable x = { str_1_3 = "Hi" } in
  x <- { str_1_3 = "Bye" };
  x
[%%expect{|
type t_1_3 = { str_1_3 : string; }
val x_1_3 : t_1_3 = {str_1_3 = "Bye"}
|}]

(* Test 1.4: returning a mutable nested record *)
type t_1_4 = { str_1_4 : string ref }
let x_1_4 =
  let mutable x = { str_1_4 = ref "Hi" } in
  x <- { str_1_4 = ref "Bye" };
  x
[%%expect{|
type t_1_4 = { str_1_4 : string ref; }
val x_1_4 : t_1_4 = {str_1_4 = {contents = "Bye"}}
|}]


(* Test 2: Reject use of mutable in closure. *)
let foo2 y =
  let mutable x = y in
  let add_55 () =
    for i = 1 to 10 do
      x <- x + i
    done;
    x
  in
  add_55

[%%expect{|
Line 5, characters 6-16:
5 |       x <- x + i
          ^^^^^^^^^^
Error: Mutable variable cannot be used inside closure.
|}]

(* Test 3: Rejected for same reason as test 2, but this one is actually safe and
   could be allowed with more sophisticated analysis in the future. *)
let foo3 y =
  let mutable x = y in
  let rec add_55 z =
    match z with
    | 0 -> x
    | z -> x <- x + z; add_55 (z-1)
  in
  add_55 10
[%%expect{|
Line 5, characters 11-12:
5 |     | 0 -> x
               ^
Error: Mutable variable cannot be used inside closure.
|}]

(* Tests 3.1 and 3.2: Disallow closures created by [lazy] *)
let disallowed_3_1 =
  let mutable x = 42 in
  lazy x
[%%expect{|
Line 3, characters 7-8:
3 |   lazy x
           ^
Error: Mutable variable cannot be used inside closure.
|}]

let disallowed_3_2 =
  let mutable x = 42 in
  lazy (x + 1)
[%%expect{|
Line 3, characters 8-9:
3 |   lazy (x + 1)
            ^
Error: Mutable variable cannot be used inside closure.
|}]

(* Test 3.3: Locally defined functors *)
module type S_3_3 = sig module F () : sig val x : int end end

let [@warning "-26"] m_3_3 =
  let mutable y = 42 in
  (module (struct module F () = struct let x = y end end) : S_3_3)

[%%expect{|
module type S_3_3 = sig module F : functor () -> sig val x : int end end
Line 5, characters 47-48:
5 |   (module (struct module F () = struct let x = y end end) : S_3_3)
                                                   ^
Error: Mutable variable cannot be used inside closure.
|}]

(* Test 3.4: Disallow closures in monadic operators *)
let disallowed_3_4 =
  let (let*) x f = f x in
  let mutable x = 42 in
  let* y = 0 in
  x + y
[%%expect{|
Line 5, characters 2-3:
5 |   x + y
      ^
Error: Mutable variable cannot be used inside a letop.
|}]


(* Test 4: Disallowed interactions with locals *)
let foo4_1 y =
  let mutable x = [] in
  for i = 1 to y do
    x <- local_ (i :: x)
  done;
  match x with
  | [] -> assert false
  | (x :: xs) -> x

[%%expect{|
Line 4, characters 9-24:
4 |     x <- local_ (i :: x)
             ^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]


let foo4_2 y = (* Can't sneak local out of non-local for loop body region *)
  let mutable x = [] in
  let build_loop () =
    for i = 1 to y do exclave_
      x <- local_ (i :: x)
    done;
    match x with
    | [] -> assert false
    | (x :: xs) -> x
  in
  build_loop ()

[%%expect{|
Line 5, characters 6-26:
5 |       x <- local_ (i :: x)
          ^^^^^^^^^^^^^^^^^^^^
Error: Mutable variable cannot be used inside closure.
|}]


let foo4_3 y = (* Can't sneak local out of non-local while loop body region *)
  let mutable x = y in
  let i = ref 1 in
  while !i <= 10 do
    x <- (local_ (x + !i));
    i := !i + 1;
  done; x

[%%expect{|
Line 5, characters 9-26:
5 |     x <- (local_ (x + !i));
             ^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let foo4_4 y = (* Can't sneak local out of non-local while cond region *)
  let mutable x = y in
  while x <- (local_ (x + 1)); x <= 100 do
    x <- x + x
  done; x

[%%expect{|
Line 3, characters 13-29:
3 |   while x <- (local_ (x + 1)); x <= 100 do
                 ^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* exclave_ closes one region, not two *)
let foo4_5 y =
  let mutable x = [] in
  for i = 1 to y do
    for j = 1 to y do exclave_
      x <- local_ ((i*j) :: x)
    done
  done;
  10
;;
[%%expect{|
Line 5, characters 11-30:
5 |       x <- local_ ((i*j) :: x)
               ^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

let foo4_6 y =
  let mutable x = [] in
  for i = 1 to y do exclave_
    for j = 1 to y do
      x <- local_ ((i*j) :: x)
    done
  done;
  10
;;
[%%expect{|
Line 5, characters 11-30:
5 |       x <- local_ ((i*j) :: x)
               ^^^^^^^^^^^^^^^^^^^
Error: This value escapes its region.
|}]

(* This is valid since both regions are closed *)
let foo4_7 y =
  let mutable x = [] in
  for i = 1 to y do exclave_
    for j = 1 to y do exclave_
      x <- local_ ((i*j) :: x)
    done
  done;
  10
;;
[%%expect{|
val foo4_7 : int -> int = <fun>
|}]

(* Can't return [x] if it is local *)
let foo4_8 () =
  let mutable x = [] in
  (x <- stack_ (1 :: []));
  x
;;
[%%expect{|
Line 4, characters 2-3:
4 |   x
      ^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

(* Can't return [x] if it is local in some cases *)
let foo4_9 b =
  let mutable x = [] in
  (x <- if b then 2 :: [] else stack_ (1 :: []));
  x
;;
[%%expect{|
Line 4, characters 2-3:
4 |   x
      ^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

(* Test 5: Allowed interactions with locals. *)
let foo5_1 y =  (* Assignment of local allowed in same scope *)
  let mutable x = [] in
  x <- (local_ (y :: x));
  x <- (local_ (y :: x));
  match x with
  | [] -> assert false
  | (x :: xs) -> 42

let () = assert Int.(equal 42 (foo5_1 42))
[%%expect{|
val foo5_1 : 'a -> int = <fun>
|}]

let foo5_2 y =  (* Assignment of local works in _local_ for loop body region *)
  let mutable x = [] in
  for i = 1 to y do exclave_
    x <- local_ (i :: x)
  done;
  match x with
  | [] -> assert false
  | (x :: xs) -> 42

let () = assert Int.(equal 42 (foo5_2 42))
[%%expect{|
val foo5_2 : int -> int = <fun>
|}]

let foo5_3 y = (* Assignment of local works in _local_ while body region *)
  let mutable x = y in
  let i = ref 1 in
  while !i <= 10 do exclave_
    x <- (local_ (x + !i));
    i := !i + 1;
  done; (x : int)
[%%expect{|
val foo5_3 : int -> int = <fun>
|}]

let foo5_4 y = (* Assign of local works in _local_ while cond region *)
  let mutable x = y in
  while exclave_ x <- (local_ (x + 1)); x <= 100 do
    x <- x + x
  done; (x : int)

[%%expect{|
val foo5_4 : int -> int = <fun>
|}]

(* Test 6: Regionality *)
(* 6.1: regional <- regional assignment is allowed *)
let [@warning "-26"] allowed_6_1 =
  let mutable x = [] in
  let y = stack_ (1 :: []) in
  for i = 0 to 1 do
    x <- y
  done
[%%expect{|
val allowed_6_1 : unit = ()
|}]

(* 6.2: local <- regional assignment is not allowed *)
let disallowed_6_2 =
  let mutable x = [] in
  for i = 0 to 1 do
    let z = stack_ (2 :: []) in
    for j = 0 to 1 do
      x <- z
    done
  done
[%%expect{|
Line 6, characters 11-12:
6 |       x <- z
               ^
Error: This value escapes its region.
|}]

(* 6.3: The mode system doesn't distinguish higher levels of regionality from
   global, so this is not allowed *)
let disallowed_6_3 =
  let mutable x = [] in
  let y = stack_ (1 :: []) in
  for i = 0 to 1 do
    for j = 0 to 1 do
      x <- y
    done
  done
[%%expect{|
Line 6, characters 11-12:
6 |       x <- y
               ^
Error: This value escapes its region.
|}]

(* Test 11: binding a mutable variable shouldn't be simplified away *)
let f_11 () =
  let mutable x = 10 in
  let y = x in
  x <- x + 10;
  (y, x)

let () = assert (f_11 () = (10,20))
[%%expect{|
val f_11 : unit -> int * int = <fun>
|}]

(* Test 12: like Test 11, but with a constructor *)
type t_12 = Foo_12 of int

let y_12 =
  let mutable x = 42 in
  let y = Foo_12 x in
    x <- 84; y
;;
[%%expect{|
type t_12 = Foo_12 of int
val y_12 : t_12 = Foo_12 42
|}]

(* Test 12.1: Eta-expansion of reordered arguments *)
let x_12_1 =
  let f ~y ~x = (x, y) in
  let mutable x = 42 in
  let g = f ~x in
  x <- 10;
  g ~y:0
[%%expect{|
val x_12_1 : int * int = (42, 0)
|}]

(* Test 13.1: Can't put aliased in unique mutable variable *)
let reset_ref (x @ unique) = x := 0;;
let x_13_1 =
  let y = ref 3 in
  let mutable x @ unique = { contents = 1 } in
  x <- y;
  reset_ref x;
  !y
;;
[%%expect{|
val reset_ref : int ref @ unique -> unit = <fun>
Line 6, characters 12-13:
6 |   reset_ref x;
                ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* Test 13.2: Unique mutable variable *)
let x_13_2 =
  let mutable x @ unique = { contents = 1 } in
  reset_ref x;
  !x
;;
[%%expect{|
Line 3, characters 12-13:
3 |   reset_ref x;
                ^
Error: This value is "aliased" but expected to be "unique".
|}]

(* Test 13.3: [let mutable x @ m] checks only that the initial value of x has
   mode [m]. *)
let x_13_3 = ref 0
let y_13_3 =
  let mutable x @ local = ref (ref 0) in
  x := x_13_3;
  x <- ref x_13_3;
  !x
[%%expect{|
val x_13_3 : int ref = {contents = 0}
val y_13_3 : int ref = {contents = 0}
|}]

let require_portable (f : (int -> unit) @ portable) = ()
[%%expect{|
val require_portable : (int -> unit) @ portable -> unit = <fun>
|}]

(* Tests 13.4 to 13.7: Notice the [@ portable] does not prevent future values
   from being non-portable, but the portability of future values of [f] is still
   tracked. *)
let allowed_13_4 =
  let mutable f @ portable = fun _ -> () in
  (f <- fun z -> ());
  require_portable f
[%%expect{|
val allowed_13_4 : unit = ()
|}]

let [@warning "-26"] allowed_13_5 =
  let mutable f @ portable = fun _ -> () in
  (f <- fun z -> x_13_3 := z)
[%%expect{|
val allowed_13_5 : unit = ()
|}]

let disallowed_13_6 =
  let mutable f @ portable = fun _ -> () in
  (f <- fun z -> x_13_3 := z);
  require_portable f
[%%expect{|
Line 4, characters 19-20:
4 |   require_portable f
                       ^
Error: This value is "nonportable" but expected to be "portable".
|}]

(* [f] remains non-portable even if a portable function is reassigned *)
let disallowed_13_7 =
  let mutable f @ portable = fun _ -> () in
  (f <- fun z -> x_13_3 := z);
  (f <- fun z -> ());
  require_portable f
[%%expect{|
Line 5, characters 19-20:
5 |   require_portable f
                       ^
Error: This value is "nonportable" but expected to be "portable".
|}]


(* Test 14: mutable functions *)
let x_14 =
  let mutable f = fun x -> 2*x in
  let y = f 1 in
  f <- (fun x -> 3*x);
  let z = f 10 in
  y + z
;;
[%%expect{|
val x_14 : int = 32
|}]

(* Test 15: mutable unboxed floats *)
let r_15 =
  let open Stdlib_upstream_compatible.Float_u in
  let mutable r = #256.0 in
  for i = 1 to 10 do
    r <- div r #2.0
  done;
  to_float r
;;
(* 2^8 / 2^10 = 2^-2 *)
[%%expect{|
val r_15 : float = 0.25
|}]

(* Test 16: mutable variables must be representable *)
type t_16 : any;;
let f_16 () = let mutable x = (assert false : t_16) in ();;
[%%expect{|
type t_16 : any
Line 2, characters 30-51:
2 | let f_16 () = let mutable x = (assert false : t_16) in ();;
                                  ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "t_16" but an expression was expected of type
         "('a : '_representable_layout_1)"
       The layout of t_16 is any
         because of the definition of t_16 at line 1, characters 0-15.
       But the layout of t_16 must be representable
         because it's the type of a variable bound by a `let`.
|}, Principal{|
type t_16 : any
Line 2, characters 26-27:
2 | let f_16 () = let mutable x = (assert false : t_16) in ();;
                              ^
Error: This pattern matches values of type "t_16"
       but a pattern was expected which matches values of type
         "('a : '_representable_layout_1)"
       The layout of t_16 is any
         because of the definition of t_16 at line 1, characters 0-15.
       But the layout of t_16 must be representable
         because it's the type of a variable bound by a `let`.
|}]

(* Test 17: mutable variables can't change type *)
let x_17 =
  let mutable x = 3.0 in
  x <- 3;
  x
;;
[%%expect{|
Line 3, characters 7-8:
3 |   x <- 3;
           ^
Error: This expression has type "int" but an expression was expected of type
         "float"
  Hint: Did you mean "3."?
|}]

(* Test 18.1: unmutated mutable variable warning *)
let x_18_1 =
  let mutable x = 3 in x + 1
;;
[%%expect{|
Line 2, characters 14-15:
2 |   let mutable x = 3 in x + 1
                  ^
Warning 186 [unmutated-mutable]: mutable variable x was never mutated.

val x_18_1 : int = 4
|}]

(* Test 18.2: mutation doesn't count as use *)
let x_18_2 =
  let mutable x = 3 in x <- 4; 4
;;
[%%expect{|
Line 2, characters 14-15:
2 |   let mutable x = 3 in x <- 4; 4
                  ^
Warning 26 [unused-var]: variable x was mutated but never used.

val x_18_2 : int = 4
|}]

(* Test 18.3: Mutable variable isn't mutated or used *)
let x_18_3 =
  let mutable x = 3 in 5
;;
[%%expect{|
Line 2, characters 14-15:
2 |   let mutable x = 3 in 5
                  ^
Warning 26 [unused-var]: unused variable x.

val x_18_3 : int = 5
|}]

(* Tests 19 and 20: some mode crossing *)
let f_19 () =
  let mutable x : int = 42 in
  x <- (local_ 24);
  x
[%%expect{|
val f_19 : unit -> int = <fun>
|}]

let foo_20 y =
  let mutable x = y in
  (x <- stack_ (10 :: x));
  x
[%%expect{|
Line 4, characters 2-3:
4 |   x
      ^
Error: This value escapes its region.
  Hint: Cannot return a local value without an "exclave_" annotation.
|}]

(* Test 21: Unboxed products not supported yet *)
let [@warning "-26"] foo_21 =
  let mutable bar = #(123, 456) in
  bar <- #(789, 101);
  42
[%%expect{|
val foo_21 : int = 42
|}]
