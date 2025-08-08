(* TEST
   flags = "-extension-universe upstream_compatible";
   expect;
*)

type t : void

external void_id : t -> t = "%identity"
[%%expect{|
type t : void
external void_id : t -> t = "%identity" [@@unboxed]
|}]

external void_id : t -> t = "%identity" [@@unboxed]
[%%expect{|
Line 1, characters 19-20:
1 | external void_id : t -> t = "%identity" [@@unboxed]
                       ^
Warning 187 [incompatible-with-upstream]: External declaration here is not upstream compatible.
The only types with non-value layouts allowed are float#,
int32#, int64#, and nativeint#. Unknown type with layout
void encountered.

Line 1, characters 24-25:
1 | external void_id : t -> t = "%identity" [@@unboxed]
                            ^
Warning 187 [incompatible-with-upstream]: External declaration here is not upstream compatible.
The only types with non-value layouts allowed are float#,
int32#, int64#, and nativeint#. Unknown type with layout
void encountered.

external void_id : t -> t = "%identity" [@@unboxed]
|}]

external[@layout_poly] poly_id : ('a : any). 'a -> 'a = "%identity"
let void_id (x : t) = poly_id x
[%%expect{|
external poly_id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
val void_id : t -> t = <fun>
|}]
