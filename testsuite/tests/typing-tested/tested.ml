(* TEST
 expect;
*)

module type Stringable = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
end

(* The example from the pattern this replaces. *)
module Id : sig
  [@@@testing_required]
  type t
  include Stringable with type t := t [@@testing_not_required]
  val is_valid : t -> bool
end = struct
  type t = string
  let of_string s = s
  let to_string s = s
  let is_valid s = String.length s > 0
end
[%%expect{|
module type Stringable =
  sig type t val of_string : string -> t val to_string : t -> string end
module Id :
  sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val is_valid : t -> bool
  end
|}]

module Test_id = struct
  [@@@covers_required_tests_of Id]
  let is_valid = Id.is_valid
  let () = assert (Id.is_valid (Id.of_string "x"))
end
[%%expect{|
module Test_id : sig val is_valid : Id.t -> bool end
|}]

module _ = struct
  [@@@covers_required_tests_of Id]
end
[%%expect{|
Line 2, characters 2-34:
2 |   [@@@covers_required_tests_of Id]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The following items of "Id" require testing
       but are not bound by this structure:
         val "Id.is_valid"
       Hint: Bind each one here alongside its tests
       (a module alias binds a whole module),
       or mark it "[@@testing_not_required]" in the interface of "Id".
|}]

(* Items are untested unless something says otherwise. *)
module No_annotations : sig
  val x : int
end = struct
  let x = 1
end

module _ = struct
  [@@@covers_required_tests_of No_annotations]
end
[%%expect{|
module No_annotations : sig val x : int end
Line 8, characters 2-46:
8 |   [@@@covers_required_tests_of No_annotations]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: No values of "No_annotations" require testing.
       Mark them with "[@@testing_required]" or "[@@@testing_required]" in its interface.
|}]

(* Individual items, and changing the default part way through. *)
module Items : sig
  val a : int [@@testing_required]
  val b : int
  [@@@testing_required]
  val c : int
  val d : int [@@testing_not_required]
  [@@@testing_not_required]
  val e : int
  external f : int -> int = "%identity" [@@testing_required]
end = struct
  let a = 1
  let b = 2
  let c = 3
  let d = 4
  let e = 5
  external f : int -> int = "%identity"
end

module _ = struct
  [@@@covers_required_tests_of Items]
end
[%%expect{|
module Items :
  sig
    val a : int
    val b : int
    val c : int
    val d : int
    val e : int
    external f : int -> int = "%identity"
  end
Line 20, characters 2-37:
20 |   [@@@covers_required_tests_of Items]
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The following items of "Items" require testing
       but are not bound by this structure:
         val "Items.a"
         val "Items.c"
         val "Items.f"
       Hint: Bind each one here alongside its tests
       (a module alias binds a whole module),
       or mark it "[@@testing_not_required]" in the interface of "Items".
|}]

(* Submodules inherit the testedness of their declaration, and can override
   it. *)
module Nested : sig
  [@@@testing_required]
  module Tested : sig
    val x : int
    val y : int [@@testing_not_required]
  end
  module Carved_out : sig
    val x : int
    val y : int [@@testing_required]
  end [@@testing_not_required]
  module Floating : sig
    val x : int
    [@@@testing_not_required]
    val y : int
  end
  module Only_types : sig
    type t
  end
  module Untested_only : sig
    val x : int
  end [@@testing_not_required]
end = struct
  module Tested = struct let x = 1 let y = 2 end
  module Carved_out = struct let x = 1 let y = 2 end
  module Floating = struct let x = 1 let y = 2 end
  module Only_types = struct type t end
  module Untested_only = struct let x = 1 end
end

module _ = struct
  [@@@covers_required_tests_of Nested]
  module Floating = struct end
end
[%%expect{|
module Nested :
  sig
    module Tested : sig val x : int val y : int end
    module Carved_out : sig val x : int val y : int end
    module Floating : sig val x : int val y : int end
    module Only_types : sig type t end
    module Untested_only : sig val x : int end
  end
Line 31, characters 2-38:
31 |   [@@@covers_required_tests_of Nested]
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The following items of "Nested" require testing
       but are not bound by this structure:
         module "Nested.Tested" (containing "x")
         module "Nested.Carved_out" (containing "y")
         val "Nested.Floating.x"
       Hint: Bind each one here alongside its tests
       (a module alias binds a whole module),
       or mark it "[@@testing_not_required]" in the interface of "Nested".
|}]

module _ = struct
  [@@@covers_required_tests_of Nested]
  module Tested = struct let x = Nested.Tested.x end
  (* A module alias binds everything in the module. *)
  module Carved_out = Nested.Carved_out
  module Floating = struct let x = Nested.Floating.x end
end
[%%expect{|
|}]

(* A submodule can be tested on its own. *)
module _ = struct
  [@@@covers_required_tests_of Nested.Tested]
end
[%%expect{|
Line 2, characters 2-45:
2 |   [@@@covers_required_tests_of Nested.Tested]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The following items of "Nested.Tested" require testing
       but are not bound by this structure:
         val "Nested.Tested.x"
       Hint: Bind each one here alongside its tests
       (a module alias binds a whole module),
       or mark it "[@@testing_not_required]" in the interface of "Nested.Tested".
|}]

module _ = struct
  [@@@covers_required_tests_of Nested.Untested_only]
end
[%%expect{|
Line 2, characters 2-52:
2 |   [@@@covers_required_tests_of Nested.Untested_only]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: No values of "Nested.Untested_only" require testing.
       Mark them with "[@@testing_required]" or "[@@@testing_required]" in its interface.
|}]

(* Module types are not affected by the default where they are defined, but
   explicit testedness inside them is kept wherever they are used. *)
module type S = sig
  val plain : int
  val explicitly_tested : int [@@testing_required]
  val explicitly_untested : int [@@testing_not_required]
  [@@@testing_required]
  val tested_by_default : int
end

module Includes : sig
  module Not_tested_by_default : S
  [@@@testing_required]
  module Tested_by_default : S
  include S [@@testing_not_required]
  module type T = sig val in_t : int end
  module Uses_t : T
end = struct
  module M = struct
    let plain = 1
    let explicitly_tested = 2
    let explicitly_untested = 3
    let tested_by_default = 4
  end
  module Not_tested_by_default = M
  module Tested_by_default = M
  include M
  module type T = sig val in_t : int end
  module Uses_t = struct let in_t = 1 end
end

module _ = struct
  [@@@covers_required_tests_of Includes]
end
[%%expect{|
module type S =
  sig
    val plain : int
    val explicitly_tested : int
    val explicitly_untested : int
    val tested_by_default : int
  end
module Includes :
  sig
    module Not_tested_by_default : S
    module Tested_by_default : S
    val plain : int
    val explicitly_tested : int
    val explicitly_untested : int
    val tested_by_default : int
    module type T = sig val in_t : int end
    module Uses_t : T
  end
Line 31, characters 2-40:
31 |   [@@@covers_required_tests_of Includes]
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The following items of "Includes" require testing
       but are not bound by this structure:
         module "Includes.Not_tested_by_default" (containing "explicitly_tested",
           "tested_by_default")
         module "Includes.Tested_by_default" (containing "plain",
           "explicitly_tested", "tested_by_default")
         val "Includes.explicitly_tested"
         val "Includes.tested_by_default"
         module "Includes.Uses_t" (containing "in_t")
       Hint: Bind each one here alongside its tests
       (a module alias binds a whole module),
       or mark it "[@@testing_not_required]" in the interface of "Includes".
|}]

(* Including an annotated signature keeps its annotations. *)
module Reexport : sig
  include module type of Id
end = Id

module _ = struct
  [@@@covers_required_tests_of Reexport]
end
[%%expect{|
module Reexport :
  sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val is_valid : t -> bool
  end
Line 6, characters 2-40:
6 |   [@@@covers_required_tests_of Reexport]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The following items of "Reexport" require testing
       but are not bound by this structure:
         val "Reexport.is_valid"
       Hint: Bind each one here alongside its tests
       (a module alias binds a whole module),
       or mark it "[@@testing_not_required]" in the interface of "Reexport".
|}]

(* Functors only need to be bound, if they contain tested items. *)
module Functors : sig
  [@@@testing_required]
  module Make (X : sig end) : sig val x : int end
  module Make_untested (X : sig end) : sig val x : int end
  [@@testing_not_required]
  module Make_types (X : sig end) : sig type t end
end = struct
  module Make (X : sig end) = struct let x = 1 end
  module Make_untested (X : sig end) = struct let x = 1 end
  module Make_types (X : sig end) = struct type t end
end

module _ = struct
  [@@@covers_required_tests_of Functors]
end
[%%expect{|
module Functors :
  sig
    module Make : functor (X : sig end) -> sig val x : int end
    module Make_untested : functor (X : sig end) -> sig val x : int end
    module Make_types : functor (X : sig end) -> sig type t end
  end
Line 14, characters 2-40:
14 |   [@@@covers_required_tests_of Functors]
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The following items of "Functors" require testing
       but are not bound by this structure:
         module "Functors.Make"
       Hint: Bind each one here alongside its tests
       (a module alias binds a whole module),
       or mark it "[@@testing_not_required]" in the interface of "Functors".
|}]

module _ = struct
  [@@@covers_required_tests_of Functors]
  module Make = Functors.Make
end
[%%expect{|
|}]

(* Shadowing the tested module does not change what is checked. *)
module _ = struct
  [@@@covers_required_tests_of Id]
  module Id = struct end
  let is_valid = 1
end
[%%expect{|
|}]

module _ = struct
  [@@@covers_required_tests_of Does_not_exist]
end
[%%expect{|
Line 2, characters 31-45:
2 |   [@@@covers_required_tests_of Does_not_exist]
                                   ^^^^^^^^^^^^^^
Error: Unbound module "Does_not_exist"
|}]

module _ = struct
  [@@@covers_required_tests_of "Id"]
end
[%%expect{|
Line 2, characters 2-36:
2 |   [@@@covers_required_tests_of "Id"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute "covers_required_tests_of".
  A module path was expected
|}]

module _ : sig
  val x : int [@@testing_required "yes"]
  val y : int [@@testing_required] [@@testing_not_required]
end = struct
  let x = 1
  let y = 2
end
[%%expect{|
Line 3, characters 38-58:
3 |   val y : int [@@testing_required] [@@testing_not_required]
                                          ^^^^^^^^^^^^^^^^^^^^
Warning 54 [duplicated-attribute]: the "testing_not_required" attribute is used more than once
  on this expression
|}]

(* A local alias of the tested module is checked like the module. *)
module Nested_alias = Nested.Tested

module _ = struct
  [@@@covers_required_tests_of Nested_alias]
end
[%%expect{|
module Nested_alias = Nested.Tested
Line 4, characters 2-44:
4 |   [@@@covers_required_tests_of Nested_alias]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The following items of "Nested_alias" require testing
       but are not bound by this structure:
         val "Nested_alias.x"
       Hint: Bind each one here alongside its tests
       (a module alias binds a whole module),
       or mark it "[@@testing_not_required]" in the interface of "Nested_alias".
|}]

(* A submodule checked by a nested structure needn't be bound by the outer
   one, so test modules don't have to shadow the modules they test. *)
module _ = struct
  [@@@covers_required_tests_of Nested]
  module _ = struct
    [@@@covers_required_tests_of Nested.Tested]
    let x = Nested.Tested.x
  end
  module Carved_out = Nested.Carved_out
  module _ = struct
    [@@@covers_required_tests_of Nested.Floating]
  end
end
[%%expect{|
Line 9, characters 4-49:
9 |     [@@@covers_required_tests_of Nested.Floating]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The following items of "Nested.Floating" require testing
       but are not bound by this structure:
         val "Nested.Floating.x"
       Hint: Bind each one here alongside its tests
       (a module alias binds a whole module),
       or mark it "[@@testing_not_required]" in the interface of "Nested.Floating".
|}]

(* The reason can be given as a payload. *)
module Reasons : sig
  [@@@testing_required]
  val x : int [@@testing_not_required "Too slow to test."]
  val y : int [@@testing_not_required 1]
end = struct
  let x = 1
  let y = 2
end
[%%expect{|
Line 4, characters 14-40:
4 |   val y : int [@@testing_not_required 1]
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute "testing_not_required".
  A string literal (the reason) or no payload was expected

module Reasons : sig val x : int val y : int end
|}]
