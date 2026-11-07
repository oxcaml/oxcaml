(* TEST
 flags = "-nopervasives -bytecode-hints";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 {
   flat-float-array;
   reference = "${test_source_directory}/hints.flat-float-array.reference";
   check-program-output;
 }{
   no-flat-float-array;
   reference = "${test_source_directory}/hints.no-flat-float-array.reference";
   check-program-output;
 }
*)

(* Hints on the reads of values known to be immediates *)

external get : 'a array -> int -> 'a = "%array_safe_get"
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( = ) : int -> int -> bool = "%equal"

type r = { a : int; b : string }

(* Field containing an immediate *)
let field_immediate (r : r) = r.a

(* Field that may contain a pointer: no hint *)
let field_pointer (r : r) = r.b

(* Element of an integer array *)
let int_array_get (t : int array) i = get t i

let int_array_unsafe_get (t : int array) i = unsafe_get t i

(* Element of an array of pointers: no hint *)
let string_array_get (t : string array) i = get t i

(* Components of a tuple pattern *)
let tuple_components (p : int * string) = let x, y = p in x, y

type t = A of int * string

(* Arguments of a constructor pattern *)
let constructor_arguments (A (x, y)) = x, y

type _ w = W_int : int w | W_string : string w

(* Tuple component whose type is only known to be immediate thanks to the
   GADT equation of one row: no hint *)
let gadt_tuple_component (type a) (p : a * a w) : a =
  match p with
  | (x, W_int) -> x
  | (x, W_string) -> x

(* Equality between integers *)
let int_equal (x : int) y = x = y

(* Physical comparison: no hint *)
let physical_equal (x : string) y = x == y

type mixed = { m_int : int; m_float : float# }

(* Field of a mixed record containing an immediate *)
let mixed_field_immediate (r : mixed) = r.m_int

type 'a mixed_poly = { p_val : 'a; p_float : float# }

(* Field of a mixed record of unknown type: no hint *)
let mixed_field_pointer (r : 'a mixed_poly) = r.p_val

(* Field of a mixed record bound to a variable of immediate layout *)
let mixed_field_let_bound (r : int mixed_poly) =
  let x : int = r.p_val in
  x, x

type u = #{ u_int : int; u_string : string }

type with_product = { u : u; s : string }

(* Component of an unboxed product containing an immediate *)
let product_field_immediate (r : with_product) = r.u.#u_int

(* Component of an unboxed product that may contain a pointer: no hint *)
let product_field_pointer (r : with_product) = r.u.#u_string

(* Inequality between integers *)
external ( <> ) : int -> int -> bool = "%notequal"

let int_not_equal (x : int) y = x <> y

(* Physical comparisons between immediates are integer comparisons *)
external ( != ) : 'a -> 'a -> bool = "%noteq"

let int_physical_equal (x : int) y = x == y

let int_physical_not_equal (x : int) y = x != y

(* Allocation of an immutable float block *)
type float_record = { x : float; y : float }

let float_record x y = { x; y }

(* Array lengths *)
external length : 'a array -> int = "%array_length"

let int_array_length (t : int array) = length t

let float_array_length (t : float array) = length t

let generic_array_length t = length t

(* Closure attributes *)
external ( + ) : int -> int -> int = "%addint"

let[@inline] inline x = x + 1

let[@inline never] inline_never x = x + 1

let[@specialise] specialise f x = f x

module Functor (X : sig end) = struct end

(* Mutually recursive closures *)
let rec even n = if n = 0 then true else odd (n + -1)

and odd n = if n = 0 then false else even (n + -1)
