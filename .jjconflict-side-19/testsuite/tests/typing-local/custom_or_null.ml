(* TEST
 expect;
*)

type 'a g_or_null =
  | Null
  | This of 'a @@ global
[@@or_null]

[%%expect{|
type 'a g_or_null = Null | This of 'a @@ global [@@or_null]
|}]

let f (local_ s : string) = This s

[%%expect{|
Line 1, characters 33-34:
1 | let f (local_ s : string) = This s
                                     ^
Error: This value is "local" to the parent region
       but is expected to be "global"
         because it is contained (via constructor "This") (with some modality) in the value at line 1, characters 28-34.
|}]

let f (s : string) =
  let local_ x = This s in
  match x with
  | Null -> assert false
  | This s' -> ref s'

[%%expect{|
val f : string -> string ref = <fun>
|}]

type 'a plain_or_null =
  | Null'
  | This' of 'a
[@@or_null]

[%%expect{|
type 'a plain_or_null = Null' | This' of 'a [@@or_null]
|}]

let f (s : string) =
  let local_ x = This' s in
  match x with
  | Null' -> assert false
  | This' s' -> ref s'

[%%expect{|
Line 5, characters 20-22:
5 |   | This' s' -> ref s'
                        ^^
Error: This value is "local"
         because it is contained (via constructor "This'") in the value at line 5, characters 4-12
         which is "local".
       However, the highlighted expression is expected to be "global".
|}]
