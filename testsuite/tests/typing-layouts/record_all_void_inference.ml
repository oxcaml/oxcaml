(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Records must have at least one runtime value, even when a field's sort is
   determined only by inference after the record's use *)

type ('a : any) r = { x : 'a }
external unbox_unit : unit -> unit# = "%unbox_unit"
[%%expect{|
type ('a : any) r = { x : 'a; }
external unbox_unit : unit -> unit# = "%unbox_unit"
|}]

(* The field's sort is determined at the use site *)
let g () = { x = unbox_unit () }
[%%expect{|
Line 1, characters 11-32:
1 | let g () = { x = unbox_unit () }
               ^^^^^^^^^^^^^^^^^^^^^
Error: Records must contain at least one runtime value.
|}]

(* The field's sort is determined after the use, by inference *)
let f x =
  let r = { x } in
  let (_ : unit# r) = r in
  r
[%%expect{|
Line 2, characters 10-15:
2 |   let r = { x } in
              ^^^^^
Error: Records must contain at least one runtime value.
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
Line 2, characters 12-17:
2 |   let w = A { x } in
                ^^^^^
Error: Records must contain at least one runtime value.
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
