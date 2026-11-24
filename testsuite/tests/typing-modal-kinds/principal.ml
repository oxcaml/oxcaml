(* TEST
 expect;
*)

type 'a pair = Pair of 'a * 'a

[%%expect{|
type 'a pair = Pair of 'a * 'a
|}]

let string_escape_l (y @ local) = let Pair (x, _) = Pair (y, "hello") in x

[%%expect{|
Line 1, characters 73-74:
1 | let string_escape_l (y @ local) = let Pair (x, _) = Pair (y, "hello") in x
                                                                             ^
Error: This value is "local"
         because it is contained (via constructor "Pair") in the value at line 1, characters 38-49
         which is "local"
         because it is allocated at line 1, characters 52-69 containing data
         which is "local" to the parent region
         because it contains (via constructor "Pair") the expression at line 1, characters 58-59
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let string_escape_r (y @ local) = let Pair (x, _) = Pair ("hello", y) in x

[%%expect{|
Line 1, characters 73-74:
1 | let string_escape_r (y @ local) = let Pair (x, _) = Pair ("hello", y) in x
                                                                             ^
Error: This value is "local"
         because it is contained (via constructor "Pair") in the value at line 1, characters 38-49
         which is "local"
         because it is allocated at line 1, characters 52-69 containing data
         which is "local" to the parent region
         because it contains (via constructor "Pair") the expression at line 1, characters 67-68
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let int_escape_l (y @ local) = let Pair (x, _) = Pair (y, 5) in x

[%%expect{|
val int_escape_l : int @ local -> int = <fun>
|}, Principal{|
Line 1, characters 64-65:
1 | let int_escape_l (y @ local) = let Pair (x, _) = Pair (y, 5) in x
                                                                    ^
Error: This value is "local"
         because it is contained (via constructor "Pair") in the value at line 1, characters 35-46
         which is "local"
         because it is allocated at line 1, characters 49-60 containing data
         which is "local" to the parent region
         because it contains (via constructor "Pair") the expression at line 1, characters 55-56
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let int_escape_r (y @ local) = let Pair (x, _) = Pair (5, y) in x

[%%expect{|
val int_escape_r : int @ local -> int = <fun>
|}, Principal{|
Line 1, characters 64-65:
1 | let int_escape_r (y @ local) = let Pair (x, _) = Pair (5, y) in x
                                                                    ^
Error: This value is "local"
         because it is contained (via constructor "Pair") in the value at line 1, characters 35-46
         which is "local"
         because it is allocated at line 1, characters 49-60 containing data
         which is "local" to the parent region
         because it contains (via constructor "Pair") the expression at line 1, characters 58-59
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
         because it is a function return value.
         Hint: Use exclave_ to return a local value.
|}]

let string_escape_expected_l : _ @ local -> _ pair = fun x -> Pair (x, "hello")

[%%expect{|
Line 1, characters 68-69:
1 | let string_escape_expected_l : _ @ local -> _ pair = fun x -> Pair (x, "hello")
                                                                        ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "Pair") in the value at line 1, characters 62-79
         which is expected to be "global".
|}]

let string_escape_expected_r : _ @ local -> _ pair = fun x -> Pair ("hello", x)

[%%expect{|
Line 1, characters 77-78:
1 | let string_escape_expected_r : _ @ local -> _ pair = fun x -> Pair ("hello", x)
                                                                                 ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "Pair") in the value at line 1, characters 62-79
         which is expected to be "global".
|}]


(* If this one ends up being accepted under non-principal mode, that's fine. *)
let int_escape_expected_l : _ @ local -> _ pair = fun x -> Pair (x, 5)

[%%expect{|
Line 1, characters 65-66:
1 | let int_escape_expected_l : _ @ local -> _ pair = fun x -> Pair (x, 5)
                                                                     ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "Pair") in the value at line 1, characters 59-70
         which is expected to be "global".
|}]

let int_escape_expected_r : _ @ local -> _ pair = fun x -> Pair (5, x)

[%%expect{|
val int_escape_expected_r : int @ local -> int pair = <fun>
|}, Principal{|
Line 1, characters 68-69:
1 | let int_escape_expected_r : _ @ local -> _ pair = fun x -> Pair (5, x)
                                                                        ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "Pair") in the value at line 1, characters 59-70
         which is expected to be "global".
|}]

let escape : 'a -> unit = fun _ -> ()

[%%expect{|
val escape : 'a -> unit = <fun>
|}]

let pattern_l (x @ local) =
  match x with
  | Pair (y, 0) -> escape y
  | _ -> ()

[%%expect{|
val pattern_l : int pair @ local -> unit = <fun>
|}, Principal{|
Line 3, characters 26-27:
3 |   | Pair (y, 0) -> escape y
                              ^
Error: This value is "local" to the parent region
         because it is contained (via constructor "Pair") in the value at line 3, characters 4-15
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let pattern_r (x @ local) =
  match x with
  | Pair (0, y) -> escape y
  | _ -> ()

[%%expect{|
val pattern_r : int pair @ local -> unit = <fun>
|}, Principal{|
Line 3, characters 26-27:
3 |   | Pair (0, y) -> escape y
                              ^
Error: This value is "local" to the parent region
         because it is contained (via constructor "Pair") in the value at line 3, characters 4-15
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]
