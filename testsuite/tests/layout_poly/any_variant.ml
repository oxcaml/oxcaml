(* TEST
 flags = "-extension layout_poly_alpha -extension layouts_beta";
 expect;
*)

type ('a : any) t =
  | None
  | Some of 'a

[%%expect{|
type ('a : any) t = None | Some of 'a
|}]

let poly_ map_or f y = function
  | None -> y
  | Some x -> f x
[%%expect{|
val poly_ map_or : 'a. ('a -> 'b) -> 'b -> 'a t -> 'b = <lpoly>
|}]

module _ : sig
  val map_or : int
end = struct
  let poly_ map_or f y = function
    | None -> y
    | Some x -> f x
end
[%%expect{|
Lines 3-7, characters 6-3:
3 | ......struct
4 |   let poly_ map_or f y = function
5 |     | None -> y
6 |     | Some x -> f x
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig val poly_ map_or : 'a. ('a -> 'b) -> 'b -> 'a t -> 'b end
       is not included in
         sig val map_or : int end
       Values do not match:
         val poly_ map_or : 'a. ('a -> 'b) -> 'b -> 'a t -> 'b
       is not included in
         val map_or : int
       The type "('a -> 'b) -> 'b -> 'a t -> 'b"
       is not compatible with the type "int"
|}]

type ('a : any) r = { x : 'a }
[%%expect{|
type ('a : any) r = { x : 'a; }
|}]

(* A layout-polymorphic field is maybe-void, so this record is not known to
   contain a runtime value. *)
let poly_ mk v = { x = v }
[%%expect{|
Line 1, characters 17-26:
1 | let poly_ mk v = { x = v }
                     ^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]

type ('a : any) r2 = { x : 'a; y : int }
[%%expect{|
type ('a : any) r2 = { x : 'a; y : int; }
|}]

(* With a field that is certainly a runtime value, it is instead the
   representation error that is reported. *)
let poly_ mk2 v = { x = v; y = 1 }
[%%expect{|
Line 1, characters 18-34:
1 | let poly_ mk2 v = { x = v; y = 1 }
                      ^^^^^^^^^^^^^^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
|}]
