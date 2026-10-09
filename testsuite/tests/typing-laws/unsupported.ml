(* TEST
 flags = "-extension laws";
 expect;
*)

(* The constructs outside the specification language are rejected with a
   located error naming the construct (see [Translspec]). *)

law? newtype (x : int) : (fun (type a) (y : a) -> true) x
[%%expect {|
Line 1, characters 36-37:
1 | law? newtype (x : int) : (fun (type a) (y : a) -> true) x
                                        ^
Error: Laws do not support locally abstract types.
|}]

(* A constructor pattern binding existential types. *)

type ex = Ex : 'a -> ex
law? existential (e : ex) : (match e with Ex _ -> true)
[%%expect {|
type ex = Ex : 'a -> ex
Line 2, characters 42-46:
2 | law? existential (e : ex) : (match e with Ex _ -> true)
                                              ^^^^
Error: Laws do not support constructors with existential types in patterns.
|}]

(* Arrays. *)

law? array (n : int) : Array.length [| n |] = 1
[%%expect {|
Line 1, characters 36-43:
1 | law? array (n : int) : Array.length [| n |] = 1
                                        ^^^^^^^
Error: Laws do not support arrays.
|}]

(* [__LINE__] and the like, whose values depend on where they occur. *)

law? source_line : __LINE__ = 1
[%%expect {|
Line 1, characters 19-27:
1 | law? source_line : __LINE__ = 1
                       ^^^^^^^^
Error: Laws do not support source locations.
|}]

(* The primitive is what counts, not its name. *)

external here : string * int * int * int = "%loc_POS"
law? source_position : here = here
[%%expect {|
external here : string * int * int * int = "%loc_POS"
Line 2, characters 23-27:
2 | law? source_position : here = here
                           ^^^^
Error: Laws do not support source locations.
|}]

(* A local module bound to a structure (only module paths are accepted,
   see printing.ml). *)

law? local_structure : (let module M = struct let x = 0 end in M.x = 0)
[%%expect {|
Line 1, characters 23-71:
1 | law? local_structure : (let module M = struct let x = 0 end in M.x = 0)
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Laws do not support local modules that are not module paths.
|}]
