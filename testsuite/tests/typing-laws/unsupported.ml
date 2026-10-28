(* TEST
 flags = "-extension laws";
 expect;
*)

(* The constructs outside the specification language are rejected with a
   located error naming the construct (see [Translspec]). *)

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
Error: Laws do not support location primitives.
|}]

(* The primitive is what counts, not its name. *)

external here : string * int * int * int = "%loc_POS"
law? source_position : here = here
[%%expect {|
external here : string * int * int * int = "%loc_POS"
Line 2, characters 23-27:
2 | law? source_position : here = here
                           ^^^^
Error: Laws do not support location primitives.
|}]

(* A locally abstract type. *)

law? newtype (x : int) : (fun (type a) (y : a) -> true) x
[%%expect {|
Line 1, characters 36-37:
1 | law? newtype (x : int) : (fun (type a) (y : a) -> true) x
                                        ^
Error: Laws do not support locally abstract types.
|}]

(* Local modules, and local opens of anything but a module path. *)

law? local_module : (let module L = List in L.length [] = 0)
[%%expect {|
Line 1, characters 20-60:
1 | law? local_module : (let module L = List in L.length [] = 0)
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Laws do not support local modules.
|}]

law? open_structure : (let open struct let x = 0 end in x = 0)
[%%expect {|
Line 1, characters 22-62:
1 | law? open_structure : (let open struct let x = 0 end in x = 0)
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Laws do not support local opens.
|}]
