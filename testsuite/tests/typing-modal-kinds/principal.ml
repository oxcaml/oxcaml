(* TEST
 expect;
*)

type 'a pair = Pair of 'a * 'a

[%%expect{|
type 'a pair = Pair of 'a * 'a
|}]

let string_escape_l (local_ y) = let Pair (x, _) = Pair (y, "hello") in x

[%%expect{|
Line 1, characters 72-73:
1 | let string_escape_l (local_ y) = let Pair (x, _) = Pair (y, "hello") in x
                                                                            ^
Error: This value is "local"
       because it is contained (via constructor "Pair") in the value at Line 1, characters 37-48
       which is "local"
       because it is allocated at Line 1, characters 51-68 containing data
       which is "local" to the parent region
       because it contains (via constructor "Pair") the expression at Line 1, characters 57-58
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
       because it is a function return value.
       Hint: Use exclave_ to return a local value.
|}]

let string_escape_r (local_ y) = let Pair (x, _) = Pair ("hello", y) in x

[%%expect{|
Line 1, characters 72-73:
1 | let string_escape_r (local_ y) = let Pair (x, _) = Pair ("hello", y) in x
                                                                            ^
Error: This value is "local"
       because it is contained (via constructor "Pair") in the value at Line 1, characters 37-48
       which is "local"
       because it is allocated at Line 1, characters 51-68 containing data
       which is "local" to the parent region
       because it contains (via constructor "Pair") the expression at Line 1, characters 66-67
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
       because it is a function return value.
       Hint: Use exclave_ to return a local value.
|}]

let int_escape_l (local_ y) = let Pair (x, _) = Pair (y, 5) in x

[%%expect{|
val int_escape_l : int @ local -> int = <fun>
|}, Principal{|
Line 1, characters 63-64:
1 | let int_escape_l (local_ y) = let Pair (x, _) = Pair (y, 5) in x
                                                                   ^
Error: This value is "local"
       because it is contained (via constructor "Pair") in the value at Line 1, characters 34-45
       which is "local"
       because it is allocated at Line 1, characters 48-59 containing data
       which is "local" to the parent region
       because it contains (via constructor "Pair") the expression at Line 1, characters 54-55
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
       because it is a function return value.
       Hint: Use exclave_ to return a local value.
|}]

let int_escape_r (local_ y) = let Pair (x, _) = Pair (5, y) in x

[%%expect{|
val int_escape_r : int @ local -> int = <fun>
|}, Principal{|
Line 1, characters 63-64:
1 | let int_escape_r (local_ y) = let Pair (x, _) = Pair (5, y) in x
                                                                   ^
Error: This value is "local"
       because it is contained (via constructor "Pair") in the value at Line 1, characters 34-45
       which is "local"
       because it is allocated at Line 1, characters 48-59 containing data
       which is "local" to the parent region
       because it contains (via constructor "Pair") the expression at Line 1, characters 57-58
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "local" to the parent region or "global"
       because it is a function return value.
       Hint: Use exclave_ to return a local value.
|}]

let string_escape_expected_l : local_ _ -> _ pair = fun x -> Pair (x, "hello")

[%%expect{|
Line 1, characters 67-68:
1 | let string_escape_expected_l : local_ _ -> _ pair = fun x -> Pair (x, "hello")
                                                                       ^
Error: This value is "local" to the parent region but is expected to be "global"
       because it is contained (via constructor "Pair") in the value at Line 1, characters 61-78
       which is expected to be "global".
|}]

let string_escape_expected_r : local_ _ -> _ pair = fun x -> Pair ("hello", x)

[%%expect{|
Line 1, characters 76-77:
1 | let string_escape_expected_r : local_ _ -> _ pair = fun x -> Pair ("hello", x)
                                                                                ^
Error: This value is "local" to the parent region but is expected to be "global"
       because it is contained (via constructor "Pair") in the value at Line 1, characters 61-78
       which is expected to be "global".
|}]


(* If this one ends up being accepted under non-principal mode, that's fine. *)
let int_escape_expected_l : local_ _ -> _ pair = fun x -> Pair (x, 5)

[%%expect{|
Line 1, characters 64-65:
1 | let int_escape_expected_l : local_ _ -> _ pair = fun x -> Pair (x, 5)
                                                                    ^
Error: This value is "local" to the parent region but is expected to be "global"
       because it is contained (via constructor "Pair") in the value at Line 1, characters 58-69
       which is expected to be "global".
|}]

let int_escape_expected_r : local_ _ -> _ pair = fun x -> Pair (5, x)

[%%expect{|
val int_escape_expected_r : int @ local -> int pair = <fun>
|}, Principal{|
Line 1, characters 67-68:
1 | let int_escape_expected_r : local_ _ -> _ pair = fun x -> Pair (5, x)
                                                                       ^
Error: This value is "local" to the parent region but is expected to be "global"
       because it is contained (via constructor "Pair") in the value at Line 1, characters 58-69
       which is expected to be "global".
|}]

let escape : 'a -> unit = fun _ -> ()

[%%expect{|
val escape : 'a -> unit = <fun>
|}]

let pattern_l (local_ x) =
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
       because it is contained (via constructor "Pair") in the value at Line 3, characters 4-15
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let pattern_r (local_ x) =
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
       because it is contained (via constructor "Pair") in the value at Line 3, characters 4-15
       which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]
