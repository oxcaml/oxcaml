(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* All-void records are supported even when a field's layout is determined
   only by inference after the record's use. *)

type ('a : any) r = { x : 'a }
[%%expect{|
type ('a : any) r = { x : 'a; }
|}]

(* The field's sort is determined after the use, by inference *)
let f x =
  let r = { x } in
  let (_ : unit# r) = r in
  r
[%%expect{|
val f : unit# -> unit# r = <fun>
|}]

type ('a : any) w = A of { x : 'a }
[%%expect{|
type ('a : any) w = A of { x : 'a; }
|}]

let h x =
  let w = A { x } in
  let (_ : unit# w) = w in
  w
[%%expect{|
val h : unit# -> unit# w = <fun>
|}]

(* All-void constructors (not inlined records) are permitted *)
type ('a : any) t = A of 'a
[%%expect{|
type ('a : any) t = A of 'a
|}]

let ok x =
  let t = A x in
  let (_ : unit# t) = t in
  t
[%%expect{|
val ok : unit# -> unit# t = <fun>
|}]
