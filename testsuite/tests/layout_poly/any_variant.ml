(* TEST
 flags = "-extension layout_poly_alpha -extension layouts_beta";
 expect;
*)

(* We don't yet support variable-representation records and variants with
   generalized sort variables. See [Typedecl.finalize_typechecked_shape]. *)

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
Line 3, characters 4-10:
3 |   | Some x -> f x
        ^^^^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
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
Line 1, characters 17-23:
1 | let poly_ mk x = Some x
                     ^^^^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
|}]

type ('a : any) r = { v : 'a; n : int }
[%%expect{|
type ('a : any) r = { v : 'a; n : int; }
|}]

let poly_ mk_r v = { v; n = 0 }
[%%expect{|
Line 1, characters 19-31:
1 | let poly_ mk_r v = { v; n = 0 }
                       ^^^^^^^^^^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
|}]

let poly_ get_n (r : _ r) = r.n
[%%expect{|
Line 1, characters 28-31:
1 | let poly_ get_n (r : _ r) = r.n
                                ^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
|}]

let poly_ get_v (r : _ r) = r.v
[%%expect{|
Line 1, characters 28-31:
1 | let poly_ get_v (r : _ r) = r.v
                                ^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
|}]

let poly_ match_r ({ v; n } : _ r) =
  ignore n;
  v
[%%expect{|
Line 1, characters 19-27:
1 | let poly_ match_r ({ v; n } : _ r) =
                       ^^^^^^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
|}]

let poly_ with_n (r : _ r) = { r with n = 1 }
[%%expect{|
Line 1, characters 29-45:
1 | let poly_ with_n (r : _ r) = { r with n = 1 }
                                 ^^^^^^^^^^^^^^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
|}]

type ('a : any) c = { mutable payload : 'a; tag : int }
[%%expect{|
type ('a : any) c = { mutable payload : 'a; tag : int; }
|}]

let poly_ set_payload (c : _ c) x = c.payload <- x
[%%expect{|
Line 1, characters 36-50:
1 | let poly_ set_payload (c : _ c) x = c.payload <- x
                                        ^^^^^^^^^^^^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
|}]

type ('a : any) ir =
  | A of { x : 'a; y : int }
  | B
[%%expect{|
type ('a : any) ir = A of { x : 'a; y : int; } | B
|}]

let poly_ mk_ir x = A { x; y = 1 }
[%%expect{|
Line 1, characters 22-34:
1 | let poly_ mk_ir x = A { x; y = 1 }
                          ^^^^^^^^^^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
|}]

let poly_ get_y = function
  | A { y; _ } -> y
  | B -> 0
[%%expect{|
Line 2, characters 6-14:
2 |   | A { y; _ } -> y
          ^^^^^^^^
Error: The representation of this record or variant depends on a
       layout-polymorphic type, which is not yet supported.
|}]
