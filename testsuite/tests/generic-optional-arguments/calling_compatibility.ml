(* TEST
flags = "-extension-universe alpha ";

 expect;
*)

(* Comprehensive test of calling compatibility for optional arguments *)

(* Three function definitions *)
let f_vanilla ?(x : int option) () = match x with None -> 0 | Some v -> v
let f_option (?(x = 0) : int option) () = x
let f_or_null (?(x = 0) : int or_null) () = x

[%%expect{|
val f_vanilla : ?x:int -> unit -> int = <fun>
val f_option : (?x):int option -> unit -> int = <fun>
val f_or_null : (?x):int or_null -> unit -> int = <fun>
|}]

(* ===================================================================== *)
(* Testing f_vanilla with 3 calling styles                               *)
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


(* ===================================================================== *)
(* Testing f_option with 3 calling styles                                *)
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

(* ===================================================================== *)
(* Testing f_or_null with 3 calling styles                               *)
(* ===================================================================== *)

(* 1. f_or_null with ~x:1 *)
let _ = f_or_null ~x:1 ()
[%%expect{|
- : int = 1
|}]

(* 2. f_or_null with ?x:(Some 1) *)
let _ = f_or_null ?x:(Some 1) ()
[%%expect{|
Line 1, characters 22-26:
1 | let _ = f_or_null ?x:(Some 1) ()
                          ^^^^
Error: This variant expression is expected to have type "int or_null"
       There is no constructor "Some" within type "or_null"
|}]

(* 3. f_or_null with ?x:(This 1) *)
let _ = f_or_null ?x:(This 1) ()
[%%expect{|
- : int = 1
|}]
