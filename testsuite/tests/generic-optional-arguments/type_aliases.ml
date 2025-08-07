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

(* This should succeed *)
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
module type T =
  sig
    val g : (?x):int_option -> unit -> int
    val h : (?x):int_or_null -> unit -> int
  end
module M : T @@ stateless
|}]

module M = struct
  let g (?(x = 42) : int_option) () = x
  let h (?(x = 42) : int_or_null) () = x
end
[%%expect {|
module M :
  sig
    val g : (?x):int option -> unit -> int
    val h : (?x):int option -> unit -> int
  end @@ stateless
|}]

(* This should succeed *)

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
  let fst (?x:(((x : int), y) = 1, 2) : int_int_option) () = x
  let third (?x:((x, y, (z : int)) = 1, 2, 3) : int_int_int_or_null) () = z
end

[%%expect {|
type 'a option2 = 'a option
type 'a or_null2 = 'a or_null
type ('a, 'b) or_null3 = 'a or_null2
type int_int_option = (int * int) option2
type int_int_int_or_null = (int * int * int, float) or_null3
module type T =
  sig
    val fst : (?x):(int * int) option2 -> unit -> int
    val third : (?x):(int * int * int) or_null -> unit -> int
  end
module M : T @@ stateless
|}]

(* This should fail *)

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
Error: Unknown generic optional argument type: float
|}]

type 'b t2 = None | Some of int [@@option_like]
type 'a t3 = float t2

(* CR generic-optional: This should work once we look at the constructors *)
let f (?(x = 42) : string t3) () = x

[%%expect{|
Line 1, characters 28-31:
1 | type 'b t2 = None | Some of int [@@option_like]
                                ^^^
Error: This type cannot be marked as option-like because
       the constructor argument must be a type variable (e.g. 'a).
|}]
