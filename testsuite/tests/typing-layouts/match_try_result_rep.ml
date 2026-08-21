(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* The result of a [try], or of a [match] with exception cases,
   must be representable. *)

type t : any
[%%expect{|
type t : any
|}]

let f (g : unit -> t) : t = try g () with Not_found -> g ()
[%%expect{|
Line 1, characters 28-59:
1 | let f (g : unit -> t) : t = try g () with Not_found -> g ()
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The result of a try expression must have a representable layout.
       The layout of t is any
         because of the definition of t at line 1, characters 0-12.
       But the layout of t must be representable
         because it's the result of a try expression or of a match with
         exception patterns or effect handlers.
|}]

let f (g : unit -> t) : t =
  match () with () -> g () | exception Not_found -> g ()
[%%expect{|
Line 2, characters 2-56:
2 |   match () with () -> g () | exception Not_found -> g ()
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The result of a match with exception patterns must have a representable layout.
       The layout of t is any
         because of the definition of t at line 1, characters 0-12.
       But the layout of t must be representable
         because it's the result of a try expression or of a match with
         exception patterns or effect handlers.
|}]

(* Representable results are accepted. *)

let ok_try (g : unit -> int) = try g () with Not_found -> 0

let ok_match (g : unit -> int) =
  match g () with n -> n | exception Not_found -> 0
[%%expect{|
val ok_try : (unit -> int) -> int = <fun>
val ok_match : (unit -> int) -> int = <fun>
|}]
