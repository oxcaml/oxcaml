(* TEST
 flags = "-extension layout_poly_alpha -extension layouts_beta";
 expect;
*)

(* Records and variants with an [any]-field used under [poly_] bindings, so
   the field's sort generalizes and its representation is not determined
   until the binding is instantiated. See [any_variant_run.ml] for runtime
   tests. *)

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
val poly_ map_or : ('a -> 'b) -> 'b -> 'a t -> 'b = <lpoly>
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
         sig val poly_ map_or : ('a -> 'b) -> 'b -> 'a t -> 'b end
       is not included in
         sig val map_or : int end
       Values do not match:
         val poly_ map_or : ('a -> 'b) -> 'b -> 'a t -> 'b
       is not included in
         val map_or : int
       The type "('a -> 'b) -> 'b -> 'a t -> 'b"
       is not compatible with the type "int"
|}]

let poly_ mk x = Some x
[%%expect{|
val poly_ mk : 'a -> 'a t = <lpoly>
|}]

type ('a : any) r = { v : 'a; n : int }
[%%expect{|
type ('a : any) r = { v : 'a; n : int; }
|}]

let poly_ mk_r v = { v; n = 0 }
[%%expect{|
val poly_ mk_r : 'a -> 'a r = <lpoly>
|}]

let poly_ get_n (r : _ r) = r.n
[%%expect{|
val poly_ get_n : 'a r -> int = <lpoly>
|}]

let poly_ get_v (r : _ r) = r.v
[%%expect{|
val poly_ get_v : 'a r -> 'a = <lpoly>
|}]

let poly_ match_r ({ v; n } : _ r) =
  ignore n;
  v
[%%expect{|
val poly_ match_r : 'a r -> 'a = <lpoly>
|}]

let poly_ with_n (r : _ r) = { r with n = 1 }
[%%expect{|
val poly_ with_n : 'a r -> 'a r = <lpoly>
|}]

type ('a : any) c = { mutable payload : 'a; tag : int }
[%%expect{|
type ('a : any) c = { mutable payload : 'a; tag : int; }
|}]

let poly_ set_payload (c : _ c) x = c.payload <- x
[%%expect{|
val poly_ set_payload : 'a c -> 'a -> unit = <lpoly>
|}]

type ('a : any) ir =
  | A of { x : 'a; y : int }
  | B
[%%expect{|
type ('a : any) ir = A of { x : 'a; y : int; } | B
|}]

let poly_ mk_ir x = A { x; y = 1 }
[%%expect{|
val poly_ mk_ir : 'a -> 'a ir = <lpoly>
|}]

let poly_ get_y = function
  | A { y; _ } -> y
  | B -> 0
[%%expect{|
val poly_ get_y : 'a ir -> int = <lpoly>
|}]
