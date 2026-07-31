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
