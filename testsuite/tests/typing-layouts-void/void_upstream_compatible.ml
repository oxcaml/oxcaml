(* TEST
   flags = "-extension-universe upstream_compatible";
   expect;
*)

type t : void

external void_id : t -> t = "%identity"
[%%expect{|
type t : void
external void_id : t -> t = "%identity"
|}]

external void_id : t -> t = "%identity" [@@unboxed]
[%%expect{|
Line 1, characters 19-20:
1 | external void_id : t -> t = "%identity" [@@unboxed]
                       ^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}]

external[@layout_poly] poly_id : ('a : any). 'a -> 'a = "%identity"
let void_id (x : t) = poly_id x
[%%expect{|
external poly_id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
val void_id : t -> t = <fun>
|}]
