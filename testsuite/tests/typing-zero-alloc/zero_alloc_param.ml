(* TEST
   flags = "-w +198";
   expect.opt;
*)

(* This tests the typing behavior of `[@zero_alloc]` attributes on function arguments.

   These tests are just about what is allowed and not allowed by the
   type checker.  The implementation of the actual `[@zero_alloc]` backend checks
   (including how the annotations in signatures affect those checks) are tested
   in the `tests/backend/checkmach` directory at the root of the project.
*)

module M : sig
  val f': ((int -> 'b) [@zero_alloc]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' (g' [@zero_alloc]) = g' 123
end;;
[%%expect {|
Line 4, characters 22-40:
4 |   let[@zero_alloc] f' (g' [@zero_alloc]) = g' 123
                          ^^^^^^^^^^^^^^^^^^
Error: Zero-alloc annotations on function arguments must specify arity.
|}];;

module M' = struct
  let[@zero_alloc] f' (g' [@zero_alloc]) = g' 123 456
end;;
[%%expect {|
Line 2, characters 22-40:
2 |   let[@zero_alloc] f' (g' [@zero_alloc]) = g' 123 456
                          ^^^^^^^^^^^^^^^^^^
Error: Zero-alloc annotations on function arguments must specify arity.
|}];;

module M2 : sig
  val f': ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' (g' [@zero_alloc arity 2]) = g' 123 456
end;;
[%%expect {|
module M2 :
  sig
    val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b [@@zero_alloc]
  end
|}];;

module M2' : sig
  val f': ((int -> int -> 'b) [@zero_alloc]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' (g' [@zero_alloc arity 2]) = g' 123 456
end;;
[%%expect {|
Line 2, characters 12-28:
2 |   val f': ((int -> int -> 'b) [@zero_alloc]) -> 'b [@@zero_alloc]
                ^^^^^^^^^^^^^^^^
Error: Zero-alloc annotations on function arguments must specify arity.
|}];;

module M2'' : sig
  val f': ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' (g' [@zero_alloc arity 1]) = g' 123 456
end;;
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let[@zero_alloc] f' (g' [@zero_alloc arity 1]) = g' 123 456
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           val f' : (int -> int -> 'a) [@zero_alloc arity 1] -> 'a
             [@@zero_alloc]
         end
       is not included in
         sig
           val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b
             [@@zero_alloc]
         end
       Values do not match:
         val f' : (int -> int -> 'a) [@zero_alloc arity 1] -> 'a
           [@@zero_alloc]
       is not included in
         val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b
           [@@zero_alloc]
       The type "(int -> int -> 'a) [@zero_alloc arity 1] -> 'a"
       is not compatible with the type
         "(int -> int -> 'b) [@zero_alloc arity 2] -> 'b"
       Zero-alloc attributes should match.
|}];;

module M2''' : sig
  val f': ((int -> int -> 'b) [@zero_alloc arity 2]) -> 'b [@@zero_alloc]
end = struct
  let[@zero_alloc] f' g' = g' 123 456
end;;
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let[@zero_alloc] f' g' = g' 123 456
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig val f' : (int -> int -> 'a) -> 'a [@@zero_alloc] end
       is not included in
         sig
           val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b
             [@@zero_alloc]
         end
       Values do not match:
         val f' : (int -> int -> 'a) -> 'a [@@zero_alloc]
       is not included in
         val f' : (int -> int -> 'b) [@zero_alloc arity 2] -> 'b
           [@@zero_alloc]
       The type "(int -> int -> 'a) -> 'a" is not compatible with the type
         "(int -> int -> 'b) [@zero_alloc arity 2] -> 'b"
       Zero-alloc attributes should match.
|}];;


type t = int -> int;;
[%%expect {|
type t = int -> int
|}];;

module type S = sig
  val f : (t [@zero_alloc arity 1]) -> int
end;;
[%%expect {|
module type S = sig val f : t -> int end
|}];;

module type S' = sig
  val f : t [@zero_alloc arity 1]
end;;
[%%expect {|
module type S' = sig val f : t end
|}];;

let[@zero_alloc] f x = x;;
[%%expect {|
val f : 'a -> 'a [@@zero_alloc] = <fun>
|}];;

let[@zero_alloc] g (f [@zero_alloc arity 1]) = f 42;;
[%%expect {|
val g : (int -> 'a) [@zero_alloc arity 1] -> 'a [@@zero_alloc] = <fun>
|}];;

let _ = g f;;
[%%expect {|
- : int = 42
|}];;

let _ =
  let[@zero_alloc] f x = x in
  g f;;
[%%expect {|
- : int = 42
|}];;

let w : ('a -> int) [@zero_alloc] = fun[@zero_alloc] x -> 444
[%%expect {|
val w : 'a -> int [@@zero_alloc] = <fun>
|}];;

let w : 'a. ('a -> int) [@zero_alloc] = fun[@zero_alloc] x -> 555
[%%expect {|
val w : 'a -> int [@@zero_alloc] = <fun>
|}];;

let w : 'a. 'a -> int = fun[@zero_alloc] x -> 555
[%%expect {|
val w : 'a -> int [@@zero_alloc] = <fun>
|}];;

let _ = g (fun[@zero_alloc] x -> 42);;
[%%expect {|
- : int = 42
|}];;

let _ = g (fun[@zero_alloc] x -> (x, 123));;
[%%expect {|
- : int * int = (42, 123)
|}];;

(* Function abstraction argument that is not marked zero_alloc. *)
let _ = g (fun x -> 1);;
[%%expect {|
- : int = 1
|}];;

let _ = g (fun x -> x);;
[%%expect {|
- : int = 42
|}];;

let _ = g (fun x -> [x]);;
[%%expect {|
- : int list = [42]
|}];;

(* Identifier argument that is not zero_alloc. *)
let _ =
  let s x = x in
  g s;;
[%%expect {|
Line 3, characters 4-5:
3 |   g s;;
        ^
Error: Function argument zero alloc assumption violated.
       There is a mismatch between the two "zero_alloc" guarantees.
|}];;

(* Examples where the zero-alloc argument requirement fails, when the argument
   is neither a function abstraction nor an identifier. *)
let _ = g ((fun x -> x) (fun x -> x));;
[%%expect {|
Line 1, characters 10-37:
1 | let _ = g ((fun x -> x) (fun x -> x));;
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This function application expects an argument that does not allocate.
       Argument must be an identifier or a function binding.
|}];;

let _ =
  let t = ((fun x -> x + 1), 42) in
  g (fst t);;
[%%expect {|
Line 3, characters 4-11:
3 |   g (fst t);;
        ^^^^^^^
Error: This function application expects an argument that does not allocate.
       Argument must be an identifier or a function binding.
|}];;

type t = {x : int -> int};;
[%%expect {|
type t = { x : int -> int; }
|}];;

let _ = fun t -> g t.x;;
[%%expect {|
Line 1, characters 19-22:
1 | let _ = fun t -> g t.x;;
                       ^^^
Error: This function application expects an argument that does not allocate.
       Argument must be an identifier or a function binding.
|}];;
