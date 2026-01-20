(* TEST
  expect;
*)

let unique_use (local_ unique_ _x) = ()
[%%expect{|
val unique_use : 'a @ local unique -> unit = <fun>
|}]

let global_aliased_use : 'a -> unit = fun _ -> ()
[%%expect{|
val global_aliased_use : 'a -> unit = <fun>
|}]

let local_aliased_use (local_ a) = ()
[%%expect{|
val local_aliased_use : 'a @ local -> unit = <fun>
|}]

let unique_aliased_use (local_ unique_ x) (local_ y) = ()
[%%expect{|
val unique_aliased_use : 'a @ local unique -> 'b @ local -> unit = <fun>
|}]

let aliased_unique_use (local_ x) (local_ unique_ y) = ()
[%%expect{|
val aliased_unique_use : 'a @ local -> 'b @ local unique -> unit = <fun>
|}]

let aliased_aliased_use (local_ x) (local_ y) = ()
[%%expect{|
val aliased_aliased_use : 'a @ local -> 'b @ local -> unit = <fun>
|}]

let local_returning (local_ x) = [%exclave] x
[%%expect{|
val local_returning : 'a @ local -> 'a @ local = <fun>
|}]

(* Cannot borrow at top level let *)
let x = borrow_ "hello"
[%%expect{|
Line 1, characters 8-23:
1 | let x = borrow_ "hello"
            ^^^^^^^^^^^^^^^
Error: Cannot borrow here because there is no borrowing context.
|}]

(* CR-someday zqian: support non-shallow borrowing *)
let foo () =
  let unique_ y = "hello" in
  let y0, y1 = borrow_ y, borrow_ y in
  ()
[%%expect{|
Line 3, characters 15-24:
3 |   let y0, y1 = borrow_ y, borrow_ y in
                   ^^^^^^^^^
Error: Cannot borrow here because there is no borrowing context.
|}]

(* borrowed values are aliased and cannot be used as unique *)
let foo () =
  let unique_ y = "hello" in
  unique_use (borrow_ y);
  ()
[%%expect{|
Line 3, characters 13-24:
3 |   unique_use (borrow_ y);
                 ^^^^^^^^^^^
Error: This value is "aliased"
       because it is borrowed.
       However, the highlighted expression is expected to be "unique".
|}]

(* borrowed values are local and cannot escape *)
let foo () =
  let x = "hello" in
  global_aliased_use (borrow_ x);
  ()
[%%expect{|
Line 3, characters 21-32:
3 |   global_aliased_use (borrow_ x);
                         ^^^^^^^^^^^
Error: This value is "local"
       because it is borrowed.
       However, the highlighted expression is expected to be "global".
|}]

(* borrowed values are local and cannot escape *)
let foo () =
  let _ =
    let x = "hello" in
    let y = borrow_ x in
    y
  in
  ()
[%%expect{|
Line 5, characters 4-5:
5 |     y
        ^
Error: This value is "local"
       because it is borrowed.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
       because it escapes the borrow region at Lines 4-5, characters 4-5.
|}]

(* In the borrow region, you are not allowed to use the original value uniquely *)

let foo () =
  let x = "hello" in
  let y = borrow_ x in
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 10-19:
3 |   let y = borrow_ x in
              ^^^^^^^^^

|}]

(* borrowing is still counted even if you don't bind the borrowed value *)
let foo () =
  let x = "hello" in
  let _ = borrow_ x in
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 10-19:
3 |   let _ = borrow_ x in
              ^^^^^^^^^

|}]

(* In the borrow region, you can use the original value as aliased *)
let foo () =
  let x = "hello" in
  let _y = borrow_ x in
  global_aliased_use x;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* But that aliased usage ruins the borrowing, so you can't uniquely use the
   value even after the borrow region. *)
let foo () =
  let x = "hello" in
  (let y = borrow_ x in
  global_aliased_use x);
  unique_use x;
  ()
[%%expect{|
Line 5, characters 13-14:
5 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 4, characters 21-22:
4 |   global_aliased_use x);
                         ^

|}]

(* Typical usage - borrowed followed by unique is ok. *)
let foo () =
  let x = "hello" in
  (let y = borrow_ x in
  local_aliased_use y);
  unique_use x
[%%expect{|
val foo : unit -> unit = <fun>
|}]


(* nested borrowing - two regions below,
z cannot escape its region
*)
let foo () =
  let x = "hello" in
  let y = borrow_ x in
  (let z = borrow_ y in z)
[%%expect{|
Line 4, characters 24-25:
4 |   (let z = borrow_ y in z)
                            ^
Error: This value is "local"
       because it is borrowed.
       However, the highlighted expression is expected to be "global".
|}]

let foo () =
  let x = "hello" in
  let y = borrow_ x in
  (let z = borrow_ x in ());
  local_aliased_use y
[%%expect{|
Line 4, characters 7-8:
4 |   (let z = borrow_ x in ());
           ^
Warning 26 [unused-var]: unused variable z.

val foo : unit -> unit = <fun>
|}]

let foo () =
  let x = "hello" in
  let y = borrow_ x in
  (let z = borrow_ x in ());
  unique_use x
[%%expect{|
Line 5, characters 13-14:
5 |   unique_use x
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 10-19:
3 |   let y = borrow_ x in
              ^^^^^^^^^

|}]

let foo () =
  let x = "hello" in
  (let y = borrow_ x in
  (let z = borrow_ x in ()));
  unique_use x
[%%expect{|
Line 3, characters 7-8:
3 |   (let y = borrow_ x in
           ^
Warning 26 [unused-var]: unused variable y.

Line 4, characters 7-8:
4 |   (let z = borrow_ x in ()));
           ^
Warning 26 [unused-var]: unused variable z.

val foo : unit -> unit = <fun>
|}]



(* CR-someday zqian: the following should be allowed by finer regionality *)
let foo () =
  let x = "hello" in
  let y = borrow_ x in
  ignore (let z = borrow_ y in y)
[%%expect{|
Line 4, characters 31-32:
4 |   ignore (let z = borrow_ y in y)
                                   ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

(* CR-someday zqian: the following should be allowed by distinguishing stack region and ghost region *)
let foo () =
  let x = "hello" in
  let _t =
    let y = borrow_ x in
    stack_ (42, 24)
  in
  ()
[%%expect{|
Line 5, characters 4-19:
5 |     stack_ (42, 24)
        ^^^^^^^^^^^^^^^
Error: This value is "local"
       because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
       because it escapes the borrow region at Lines 4-5, characters 4-19.
|}]

(* Borrowing in Texp_match/Texp_apply/Texp_let are very similar,
   there is no need to duplicate every test for each of them.
   Below we only test the common cases. *)

(* borrowed values are aliased and cannot be used as unique *)
let foo () =
  let y = "hello" in
  match borrow_ y with
  | x -> ignore (unique_use x)
[%%expect{|
Line 4, characters 28-29:
4 |   | x -> ignore (unique_use x)
                                ^
Error: This value is "aliased"
       because it is borrowed.
       However, the highlighted expression is expected to be "unique".
|}]

(* borrowed values are local and cannot escape *)
let foo () =
  let unique_ y = "hello" in
  match borrow_ y with
  | x -> ignore (global_aliased_use x)
[%%expect{|
Line 4, characters 36-37:
4 |   | x -> ignore (global_aliased_use x)
                                        ^
Error: This value is "local"
       because it is borrowed.
       However, the highlighted expression is expected to be "global".
|}]

(* During borrowing, you are not allowed to use the original value uniquely *)
let foo () =
  let x = "hello" in
  match borrow_ x with
  | _y -> ignore (unique_use x)
[%%expect{|
Line 4, characters 29-30:
4 |   | _y -> ignore (unique_use x)
                                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 8-17:
3 |   match borrow_ x with
            ^^^^^^^^^

|}]


(* Due to our implementation, borrowing is still counted even if you don't
really bind the borrowed value *)
let foo () =
  let x = "hello" in
  match borrow_ x with
  | _ -> ignore (unique_use x)

[%%expect{|
Line 4, characters 28-29:
4 |   | _ -> ignore (unique_use x)
                                ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 8-17:
3 |   match borrow_ x with
            ^^^^^^^^^

|}]

(* but aliased use is fine *)
let foo () =
  let x = "hello" in
  match borrow_ x with
  | _ -> ignore (global_aliased_use x)
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* Moreover, that aliased use will clash with later unique use *)
let foo () =
  let x = "hello" in
  (match borrow_ x with
  | _ -> global_aliased_use x
  );
  ignore (unique_use x)
[%%expect{|
Line 6, characters 21-22:
6 |   ignore (unique_use x)
                         ^
Error: This value is used here as unique, but it has already been used:
Line 4, characters 28-29:
4 |   | _ -> global_aliased_use x
                                ^

|}]

(* function application borrowing *)

(* You can't use x uniquely after aliased usage *)
let foo () =
  let unique_ x = "hello" in
  global_aliased_use x;
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 21-22:
3 |   global_aliased_use x;
                         ^

|}]

(* unless if you borrow it *)
let foo () =
  let unique_ x = "hello" in
  local_aliased_use (borrow_ x);
  unique_use x;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* borrow after unique usage is bad *)
let foo () =
  let unique_ x = "hello" in
  unique_use x;
  local_aliased_use (borrow_ x);
  ()
[%%expect{|
Line 4, characters 20-31:
4 |   local_aliased_use (borrow_ x);
                        ^^^^^^^^^^^
Error: This value is used here, but it has already been used as unique:
Line 3, characters 13-14:
3 |   unique_use x;
                 ^

|}]

(* multiple borrowing is fine *)
let foo () =
  let unique_ x = "hello" in
  aliased_aliased_use (borrow_ x) (borrow_ x);
  unique_use x;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]

(* but you need to borrow both of course *)
let foo () =
  let unique_ x = "hello" in
  aliased_aliased_use (borrow_ x) x;
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 34-35:
3 |   aliased_aliased_use (borrow_ x) x;
                                      ^

|}]

let foo () =
  let unique_ x = "hello" in
  local_aliased_use (borrow_ (global_aliased_use x));
  unique_use x;
  ()
[%%expect{|
Line 4, characters 13-14:
4 |   unique_use x;
                 ^
Error: This value is used here as unique, but it has already been used:
Line 3, characters 49-50:
3 |   local_aliased_use (borrow_ (global_aliased_use x));
                                                     ^

|}]

(* borrowed values are aliased *)
let foo () =
  let unique_ x = "hello" in
  unique_use (borrow_ x);
  ()
[%%expect{|
Line 3, characters 13-24:
3 |   unique_use (borrow_ x);
                 ^^^^^^^^^^^
Error: This value is "aliased"
       because it is borrowed.
       However, the highlighted expression is expected to be "unique".
|}]

(* borrowed values are local and cannot escape *)
let foo () =
  let unique_ x = "hello" in
  global_aliased_use (borrow_ x);
  ()
[%%expect{|
Line 3, characters 21-32:
3 |   global_aliased_use (borrow_ x);
                         ^^^^^^^^^^^
Error: This value is "local"
       because it is borrowed.
       However, the highlighted expression is expected to be "global".
|}]

(* CR-soon zqian: The following should pass, once we distinguish stack region
and ghost region, and allow functions to have "stack" as return mode. *)
let foo () =
  let unique_ x = "hello" in
  local_returning (borrow_ x);
  ()
[%%expect{|
Line 3, characters 2-29:
3 |   local_returning (borrow_ x);
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "local"
       but is expected to be "local" to the parent region or "global"
       because it escapes the borrow region at Line 3, characters 2-29.
|}]

let foo () =
  let unique_ x = "hello" in
  aliased_unique_use (borrow_ x) x;
  ()
[%%expect{|
Line 3, characters 33-34:
3 |   aliased_unique_use (borrow_ x) x;
                                     ^
Error: This value is used here as unique, but it is already being used:
Line 3, characters 21-32:
3 |   aliased_unique_use (borrow_ x) x;
                         ^^^^^^^^^^^

|}]

(* Closing over borrowing *)

(* In the following, [bar] is never called, but it closes over borrowing,
   and thus is also borrowing. *)
(* CR-someday zqian: this should type error *)
let foo () =
  let x = "hello" in
  let bar () =
    let bar' () =
      local_aliased_use (borrow_ x);
      ()
    in
    ()
  in
  unique_use x
[%%expect{|
Line 10, characters 13-14:
10 |   unique_use x
                  ^
Error: This value is used here as unique, but it has already been borrowed:
Line 5, characters 24-35:
5 |       local_aliased_use (borrow_ x);
                            ^^^^^^^^^^^

|}]

(* Unique use is allowed after leaving the borrow region *)
let foo () =
  let x = "hello" in
  let _z =
    (let _bar () =
      local_aliased_use (borrow_ x);
      ()
     in
    42)
  in
  unique_use x
[%%expect{|
Line 10, characters 13-14:
10 |   unique_use x
                  ^
Error: This value is used here as unique, but it has already been borrowed:
Line 5, characters 24-35:
5 |       local_aliased_use (borrow_ x);
                            ^^^^^^^^^^^

|}]

(* The function is local *)
(* CR-someday zqian: this should type error *)
let foo () =
  let x = "hello" in
  let bar () =
    local_aliased_use (borrow_ x);
    ()
  in
  ref bar
[%%expect{|
val foo : unit -> (unit -> unit) ref = <fun>
|}]

(* The function is aliased *)
(* CR-someday zqian: this should type error *)
let foo () =
  let x = "hello" in
  let bar () =
    local_aliased_use (borrow_ x);
    ()
  in
  unique_use bar;
  ()
[%%expect{|
val foo : unit -> unit = <fun>
|}]
