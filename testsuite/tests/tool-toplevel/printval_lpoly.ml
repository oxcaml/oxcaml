(* TEST
 flags = "-extension layout_poly_alpha";
 expect;
*)

(* Layout-poly values should print as <lpoly> in the toplevel. *)
let poly_ id x = x
[%%expect{|
val poly_ id : 'a -> 'a = <lpoly>
|}]

type ('a : any) holder = Hold of 'a
type mixed = Text of string | Bits of int64_u [@@unboxed]
type flag = First | Second [@@unboxed]
let payload = Hold (Text "payload")
let tag = Hold First
[%%expect{|
type ('a : any) holder = Hold of 'a
type mixed = Text of string | Bits of int64_u [@@unboxed]
type flag = First | Second [@@unboxed]
val payload : mixed holder = Hold <unboxed sum>
val tag : flag holder = Hold <unboxed sum>
|}]
