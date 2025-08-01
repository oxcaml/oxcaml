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
Error: Unknown generic optional argument type: int
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
Error: Unknown generic optional argument type: int_option
|}]

module M = struct
  let g (?(x = 42) : int_option) () = x
  let h (?(x = 42) : int_or_null) () = x
end
[%%expect {|
Line 2, characters 21-31:
2 |   let g (?(x = 42) : int_option) () = x
                         ^^^^^^^^^^
Error: Unknown generic optional argument type: int_option
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
Error: Generic optional arguments require types with the [@option_like] attribute.
       Type "option2" is not marked as option-like
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
Line 4, characters 16-26:
4 |   val g : (?x): int option -> unit -> int
                    ^^^^^^^^^^
Error: Generic optional arguments require types with the [@option_like] attribute.
       Type "option" is not marked as option-like
|}]
