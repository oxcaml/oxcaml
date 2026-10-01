(* TEST
 flags = "-I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

(* The parse tree of a law prints with [Pprintast] to a source that parses
   back to the same tree. [roundtrip] errors if they are not the same. *)

let roundtrip source =
  let parse s = Parse.implementation (Lexing.from_string s) in
  let print str = Format.asprintf "%a" Pprintast.structure str in
  let tree str =
    Clflags.locations := false;
    Format.asprintf "%a" Printast.implementation str
  in
  let str = parse source in
  let printed = print str in
  let str' = parse printed in
  if print str' <> printed then Format.printf "PRINTING IS NOT A FIXPOINT@.";
  if tree str' <> tree str then Format.printf "TREES DIFFER@."
;;
[%%expect {|
val roundtrip : string -> unit = <fun>
|}]

let () = roundtrip "law? trivial : true"
[%%expect {|
|}]

let () = roundtrip {|
  law? assoc (x : int) (y : int) (z : int) :
    x >= 0 ===> y >= 0 ===> x + (y + z) = (x + y) + z
|}
[%%expect {|
|}]

(* Laws in every structure and signature position. *)

let () = roundtrip {|
  module M = struct
    law? in_module : true
    module N = struct law? in_nested_module : true end
    module type T = sig law? in_nested_module_type : true end
  end
  module F (X : sig law? in_functor_parameter : true end) = struct
    law? in_functor_body : true
  end
  module type FT = functor (X : sig end) -> sig
    law? in_functor_result : true
  end
  module G (X : sig end) : sig law? in_result_constraint : true end = struct
    law? in_result_constraint : true
  end
  module type FC = sig law? in_first_class_module : true end
  let first_class = (module M : FC)
  include (struct law? in_included_structure : true end)
|}
[%%expect {|
|}]

(* Parameters with and without type annotations. *)

let () = roundtrip {|
  law? mixed x (y : int) z (t : 'a list) : x + y = z && t = []
  law? one_param (x : int) : x = x
  law? unannotated x y z : x + y + z = z + y + x
  law? polymorphic (xs : 'a list) (f : 'a -> 'b) :
    List.length (List.map f xs) = List.length xs
  law? complex_types
    (p : int * string) (f : int -> int -> int) (o : int option)
    (r : < m : int >) (v : [ `A | `B of int ]) (t : (int, string) Hashtbl.t) :
    true
|}
[%%expect {|
|}]

(* Any number of assumptions. *)

let () = roundtrip {|
  law? one (x : int) : x > 0 ===> x <> 0
  law? three (x : int) (y : int) (z : int) :
    x > 0 ===> y > 0 ===> z > 0 ===> x * y * z > 0
|}
[%%expect {|
|}]

(* Attributes on laws and on parameter types. *)

let () = roundtrip {|
  law? attributed : true [@@attr]
  law? several_attributes (x : int) : x = x [@@attr1] [@@attr2 payload]
  law? attributed_param_type (x : int [@attr]) : true
|}
[%%expect {|
|}]

(* Clauses are [expr]s. The test below contains far reaching expressions ([let],
   [if], [match], [fun], [try] and sequences). The parentheses make their ranges
   visible in the input. The test ensures that the clauses parse back to the
   same. *)

let () = roundtrip {|
  law? clauses (a : bool) (n : int) :
    (let b = a in b) ===> (if a then n else 0) >= 0 ===>
    (match n with 0 -> true | _ -> a) ===> (ignore n; a) ===>
    (fun b -> b) a ===> (try a with _ -> false)
|}
[%%expect {|
|}]
