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
end

let law =
  let module L = Instantiate (Law_trace.Choice) in
  Law_trace.law ~names:(to_list names) ~locations:(to_list locations)
    (to_list (L.laws (module Choices)))
[%%expect{|
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
