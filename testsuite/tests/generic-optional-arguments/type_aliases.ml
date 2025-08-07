(* TEST
flags = "-extension-universe alpha ";

 expect;
*)


module type T = sig
  val g : (?x): int -> unit -> int
end
[%%expect {|
Line 2, characters 16-19:
2 |   val g : (?x): int -> unit -> int
                    ^^^
Error: Unknown generic optional argument type
|}]

(* CR: generic-optional: This should succeed *)
type int_option = int option
type int_or_null = int option

module type T = sig
  val g : (?x): int_option -> unit -> int
  val h : (?x): int_or_null -> unit -> int
end

module M : T = struct
  let g (?(x = 42) : int_option) () = x
  let h (?(x = 42) : int_or_null) () = x
end

[%%expect {|
type int_option = int option
type int_or_null = int option
Line 5, characters 16-26:
5 |   val g : (?x): int_option -> unit -> int
                    ^^^^^^^^^^
Error: Unknown generic optional argument type
|}]

(* CR: generic-optional: This should succeed *)

type 'a option2 = 'a option
type 'a or_null2 = 'a or_null
type ('a, 'b) or_null3 = 'a or_null2
type int_int_option = (int * int) option2
type int_int_int_or_null = (int * int * int, float) or_null3

module type T = sig
  val fst : (?x): (int * int) option2 -> unit -> int
  val third : (?x): (int * int * int) or_null -> unit -> int
end

module M : T = struct
  let fst (?x:((x : int), y) : int_int_option) () = x
  let h (?x:(x, y, (z : int)) : int_int_int_or_null) () = z
end

[%%expect {|
type 'a option2 = 'a option
type 'a or_null2 = 'a or_null
type ('a, 'b) or_null3 = 'a or_null2
type int_int_option = (int * int) option2
type int_int_int_or_null = (int * int * int, float) or_null3
Line 8, characters 18-37:
8 |   val fst : (?x): (int * int) option2 -> unit -> int
                      ^^^^^^^^^^^^^^^^^^^
Error: Unknown generic optional argument type
|}]

(* CR: generic-optional: This should fail *)

type 'a option = float

module type T = sig
  val g : (?x): int option -> unit -> int
end

module M : T = struct
  let g (?(x = 42) : int option) () = x
end

[%%expect {|
type 'a option = float
module type T = sig val g : (?x):int option -> unit -> int end
Lines 7-9, characters 15-3:
7 | ...............struct
8 |   let g (?(x = 42) : int option) () = x
9 | end
Error: Signature mismatch:
       Modules do not match:
         sig val g : (?x):int option/2 -> unit -> int @@ stateless end
       is not included in
         T
       Values do not match:
         val g : (?x):int option/2 -> unit -> int @@ stateless
       is not included in
         val g : (?x):int option/1 -> unit -> int
       The type "(?x):int option/2 -> unit -> int"
       is not compatible with the type "(?x):int option/1 -> unit -> int"
       Type "int option/2" is not compatible with type "int option/1" = "float"
       Line 1, characters 0-22:
         Definition of type "option/1"
       File "_none_", line 1:
         Definition of type "option/2"
|}]
