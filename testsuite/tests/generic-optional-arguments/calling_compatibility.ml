(* TEST
flags = "-extension-universe alpha ";

 expect;
*)

(* Comprehensive test of calling compatibility for optional arguments *)

(* Three function definitions *)
let f_vanilla ?(x : int option) () = match x with None -> 0 | Some v -> v
let f_option Stdlib.Option.?'(x = 0) () = x
let f_or_null Stdlib.Or_null.?'(x = 0) () = x

[%%expect{|
val f_vanilla : ?x:int -> unit -> int = <fun>
val f_option : Stdlib.Option.?'x:int -> unit -> int = <fun>
val f_or_null : Stdlib.Or_null.?'x:int -> unit -> int = <fun>
|}]

(* ===================================================================== *)
(* Testing f_vanilla with 7 calling styles                               *)
(* ===================================================================== *)

(* 1. f_vanilla with ~x:1 *)
let _ = f_vanilla ~x:1 ()
[%%expect{|
- : int = 1
|}]

(* 2. f_vanilla with ?x:(Some 1) *)
let _ = f_vanilla ?x:(Some 1) ()
[%%expect{|
- : int = 1
|}]

(* 3. f_vanilla with ?x:(This 1) *)
let _ = f_vanilla ?x:(This 1) ()
[%%expect{|
Line 1, characters 22-26:
1 | let _ = f_vanilla ?x:(This 1) ()
                          ^^^^
Error: This variant expression is expected to have type "int option"
       There is no constructor "This" within type "option"
|}]

(* 4. f_vanilla with Stdlib.Option.?'x:(Some 1) *)
let _ = f_vanilla Stdlib.Option.?'x:(Some 1) ()
[%%expect{|
- : int = 1
|}]

(* 5. f_vanilla with Stdlib.Option.?'x:(This 1) *)
let _ = f_vanilla Stdlib.Option.?'x:(This 1) ()
[%%expect{|
Line 1, characters 37-41:
1 | let _ = f_vanilla Stdlib.Option.?'x:(This 1) ()
                                         ^^^^
Error: This variant expression is expected to have type "int option"
       There is no constructor "This" within type "option"
|}]

(* 6. f_vanilla with Stdlib.Or_null.?'x:(Some 1) *)
let _ = f_vanilla Stdlib.Or_null.?'x:(Some 1) ()
[%%expect{|
Line 1, characters 37-45:
1 | let _ = f_vanilla Stdlib.Or_null.?'x:(Some 1) ()
                                         ^^^^^^^^
Error: The function applied to this argument has type ?x:int -> int
This argument cannot be applied with label "Stdlib.Or_null.?'x"
|}]

(* 7. f_vanilla with Stdlib.Or_null.?'x:(This 1) *)
let _ = f_vanilla Stdlib.Or_null.?'x:(This 1) ()
[%%expect{|
Line 1, characters 37-45:
1 | let _ = f_vanilla Stdlib.Or_null.?'x:(This 1) ()
                                         ^^^^^^^^
Error: The function applied to this argument has type ?x:int -> int
This argument cannot be applied with label "Stdlib.Or_null.?'x"
|}]

(* ===================================================================== *)
(* Testing f_option with 7 calling styles                                *)
(* ===================================================================== *)

(* 1. f_option with ~x:1 *)
let _ = f_option ~x:1 ()
[%%expect{|
- : int = 1
|}]

(* 2. f_option with ?x:(Some 1) *)
let _ = f_option ?x:(Some 1) ()
[%%expect{|
- : int = 1
|}]

(* 3. f_option with ?x:(This 1) *)
let _ = f_option ?x:(This 1) ()
[%%expect{|
Line 1, characters 21-25:
1 | let _ = f_option ?x:(This 1) ()
                         ^^^^
Error: This variant expression is expected to have type "int option"
       There is no constructor "This" within type "option"
|}]

(* 4. f_option with Stdlib.Option.?'x:(Some 1) *)
let _ = f_option Stdlib.Option.?'x:(Some 1) ()
[%%expect{|
- : int = 1
|}]

(* 5. f_option with Stdlib.Option.?'x:(This 1) *)
let _ = f_option Stdlib.Option.?'x:(This 1) ()
[%%expect{|
Line 1, characters 36-40:
1 | let _ = f_option Stdlib.Option.?'x:(This 1) ()
                                        ^^^^
Error: This variant expression is expected to have type "int option"
       There is no constructor "This" within type "option"
|}]

(* 6. f_option with Stdlib.Or_null.?'x:(Some 1) *)
let _ = f_option Stdlib.Or_null.?'x:(Some 1) ()
[%%expect{|
Line 1, characters 36-44:
1 | let _ = f_option Stdlib.Or_null.?'x:(Some 1) ()
                                        ^^^^^^^^
Error: The function applied to this argument has type
         Stdlib.Option.?'x:int -> int
This argument cannot be applied with label "Stdlib.Or_null.?'x"
|}]

(* 7. f_option with Stdlib.Or_null.?'x:(This 1) *)
let _ = f_option Stdlib.Or_null.?'x:(This 1) ()
[%%expect{|
Line 1, characters 36-44:
1 | let _ = f_option Stdlib.Or_null.?'x:(This 1) ()
                                        ^^^^^^^^
Error: The function applied to this argument has type
         Stdlib.Option.?'x:int -> int
This argument cannot be applied with label "Stdlib.Or_null.?'x"
|}]

(* ===================================================================== *)
(* Testing f_or_null with 7 calling styles                               *)
(* ===================================================================== *)

(*= CR generic-optional : These test cases will cause a segfault. *)

(* 1. f_or_null with ~x:1 *)
let _ = f_or_null ~x:1 ()
[%%expect{|
- : int = 1
|}]

(* 2. f_or_null with ?x:(Some 1) *)
let _ = f_or_null ?x:(Some 1) ()
[%%expect{|
Line 1, characters 21-29:
1 | let _ = f_or_null ?x:(Some 1) ()
                         ^^^^^^^^
Error: The function applied to this argument has type
         Stdlib.Or_null.?'x:int -> int
This argument cannot be applied with label "?x"
|}]

(* 3. f_or_null with ?x:(This 1) *)
let _ = f_or_null ?x:(This 1) ()
[%%expect{|
Line 1, characters 21-29:
1 | let _ = f_or_null ?x:(This 1) ()
                         ^^^^^^^^
Error: The function applied to this argument has type
         Stdlib.Or_null.?'x:int -> int
This argument cannot be applied with label "?x"
|}]

(* 4. f_or_null with Stdlib.Option.?'x:(Some 1) *)
let _ = f_or_null Stdlib.Option.?'x:(Some 1) ()
[%%expect{|
Line 1, characters 36-44:
1 | let _ = f_or_null Stdlib.Option.?'x:(Some 1) ()
                                        ^^^^^^^^
Error: The function applied to this argument has type
         Stdlib.Or_null.?'x:int -> int
This argument cannot be applied with label "Stdlib.Option.?'x"
|}]


(* 5. f_or_null with Stdlib.Option.?'x:(This 1) *)
let _ = f_or_null Stdlib.Option.?'x:(This 1) ()
[%%expect{|
Line 1, characters 36-44:
1 | let _ = f_or_null Stdlib.Option.?'x:(This 1) ()
                                        ^^^^^^^^
Error: The function applied to this argument has type
         Stdlib.Or_null.?'x:int -> int
This argument cannot be applied with label "Stdlib.Option.?'x"
|}]


(* 6. f_or_null with Stdlib.Or_null.?'x:(Some 1) *)
let _ = f_or_null Stdlib.Or_null.?'x:(Some 1) ()
[%%expect{|
Line 1, characters 38-42:
1 | let _ = f_or_null Stdlib.Or_null.?'x:(Some 1) ()
                                          ^^^^
Error: This variant expression is expected to have type "int or_null"
       There is no constructor "Some" within type "or_null"
|}]


(* 7. f_or_null with Stdlib.Or_null.?'x:(This 1) *)
let _ = f_or_null Stdlib.Or_null.?'x:(This 1) ()
[%%expect{|
- : int = 1
|}]
