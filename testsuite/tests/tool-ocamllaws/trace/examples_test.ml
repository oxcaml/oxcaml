(* TEST
 readonly_files = "law_trace.ml examples.ml";
 setup-ocamlc.byte-build-env;
 flags = "-extension laws";
 module = "examples.ml";
 ocamlc.byte;
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws extract \
   -format laws-file -o examples_laws.ml examples.cmo";
 script;
 module = "examples_laws.ml law_trace.ml";
 ocamlc.byte;
 flags = "-I ocamlc.byte ocamlc.byte/examples.cmo \
   ocamlc.byte/examples_laws.cmo ocamlc.byte/law_trace.cmo";
 expect;
*)

(* This test executes the laws of [examples.ml] on the inputs of [Choices] below
   and records their trace. It validates the generated files without requiring
   anyone to inspect the generated .ml code.  *)

open Examples_laws

(* The choices of the parameter of [Max], each with the inputs of its laws.
   With a [compare] that always returns 1, [max a b] is [a]. *)
module type Max_choices = Instantiate(Law_trace.Choice).Max_choices

module Max_inputs (X : sig
    include Examples.Comparable
    val show : t -> string
    val pairs : (t * t) list
  end) = struct
  module X = X
  type max_comm_input =
    | Max_comm_input : { a : X.t; b : X.t } -> max_comm_input
  let max_comm =
    List.map
      (fun (a, b) -> (X.show a ^ " " ^ X.show b, Max_comm_input { a; b }))
      X.pairs
end

module Choices = struct
  let trivial = [ ("()", Trivial_input) ]
  let double_grows =
    [ ("0", Double_grows_input { x = 0 }); ("3", Double_grows_input { x = 3 }) ]
  let sum_grows =
    [ ("0 1", Sum_grows_input { x = 0; y = 1 });
      ("1 0", Sum_grows_input { x = 1; y = 0 });
      ("1 1", Sum_grows_input { x = 1; y = 1 }) ]

  let poly = [ ("[1; 2], succ", Poly_input { xs = [ 1; 2 ]; f = succ }) ]
  let kinded =
    [ ("1", Kinded_input { x = 1 }); ("\"a\"", Kinded_input { x = "a" }) ]
  let modes = [ ("fun x -> x", Modes_input { g = (fun x -> x) }) ]
  let unboxed = [ ("#1.5", Unboxed_input { x = #1.5 }) ]
  let alias = [ ("3", Alias_input { x = 3 }) ]
  let \#type = [ ("0", Type_input { \#method = 0 }) ]

  let max =
    [ ( "Int",
        (module Max_inputs (struct
           type t = int
           let compare = Int.compare
           let show = Int.to_string
           let pairs = [ (1, 2); (2, 1) ]
         end) : Max_choices) );
      ( "String",
        (module Max_inputs (struct
           type t = string
           let compare = String.compare
           let show = Printf.sprintf "%S"
           let pairs = [ ("a", "b") ]
         end) : Max_choices) );
      ( "compare always 1",
        (module Max_inputs (struct
           type t = int
           let compare _ _ = 1
           let show = Int.to_string
           let pairs = [ (1, 2) ]
         end) : Max_choices) ) ]
end

let law =
  let module L = Instantiate (Law_trace.Choice) in
  Law_trace.law ~names:(to_list names) ~locations:(to_list locations)
    (to_list (L.laws (module Choices)))
[%%expect{|
module type Max_choices =
  Examples_laws.Instantiate(Law_trace.Choice).Max_choices
module Max_inputs :
  functor
    (X : sig
           type t
           val compare : t -> t -> int
           val show : t -> string
           val pairs : (t * t) list
         end)
    ->
    sig
      module X :
        sig
          type t = X.t
          val compare : t -> t -> int
          val show : t -> string
          val pairs : (t * t) list
        end
      type max_comm_input =
          Max_comm_input : { a : X.t; b : X.t; } -> max_comm_input
      val max_comm : (string * max_comm_input) list
    end
module Choices :
  sig
    val trivial : (string * Examples_laws.trivial_input) list
    val double_grows : (string * Examples_laws.double_grows_input) list
    val sum_grows : (string * Examples_laws.sum_grows_input) list
    val poly : (string * Examples_laws.poly_input) list
    val kinded : (string * Examples_laws.kinded_input) list
    val modes : (string * Examples_laws.modes_input) list
    val unboxed : (string * Examples_laws.unboxed_input) list
    val alias : (string * Examples_laws.alias_input) list
    val \#type : (string * Examples_laws.type_input) list
    val max : (string * (module Max_choices)) list
  end
val law : string -> unit = <fun>
|}]

let () = law "trivial"
[%%expect{|
law_? trivial : 1 = 1
  input ()
    assert 1 = 1: holds
|}]

let () = law "double_grows"
[%%expect{|
law_? double_grows (x : int) : x > 0 implies_ double x > x
  input 0
    assume x > 0: fails
  input 3
    assume x > 0: holds
    assert double x > x: holds
|}]

let () = law "sum_grows"
[%%expect{|
law_? sum_grows (x : int) (y : int) :
    x > 0 implies_ y > 0 implies_ x + y > x
  input 0 1
    assume x > 0: fails
  input 1 0
    assume x > 0: holds
    assume y > 0: fails
  input 1 1
    assume x > 0: holds
    assume y > 0: holds
    assert x + y > x: holds
|}]

let () = law "poly"
[%%expect{|
law_? poly (xs : 'a list) (f : 'a -> 'b) :
    List.length (List.map f xs) = List.length xs
  input [1; 2], succ
    assert List.length (List.map f xs) = List.length xs: holds
|}]

let () = law "kinded"
[%%expect{|
law_? kinded (x : ('a : immutable_data)) : immutable x
  input 1
    assert immutable x: holds
  input "a"
    assert immutable x: holds
|}]

let () = law "modes"
[%%expect{|
law_? modes (g : local_ int -> int) : apply g = apply g
  input fun x -> x
    assert apply g = apply g: holds
|}]

let () = law "unboxed"
[%%expect{|
law_? unboxed (x : float#) : of_unboxed x = of_unboxed x
  input #1.5
    assert of_unboxed x = of_unboxed x: holds
|}]

let () = law "alias"
[%%expect{|
law_? alias (x : (int as 'a)) : x = x
  input 3
    assert x = x: holds
|}]

let () = law "type"
[%%expect{|
law_? \#type (\#method : int) : \#method = \#method
  input 0
    assert \#method = \#method: holds
|}]

let () = law "max_max_comm"
[%%expect{|
law_? max_comm (a : X.t) (b : X.t) : X.compare (max a b) (max b a) = 0
  input Int
    input 1 2
      assert X.compare (max a b) (max b a) = 0: holds
    input 2 1
      assert X.compare (max a b) (max b a) = 0: holds
  input String
    input "a" "b"
      assert X.compare (max a b) (max b a) = 0: holds
  input compare always 1
    input 1 2
      assert X.compare (max a b) (max b a) = 0: fails
|}]
