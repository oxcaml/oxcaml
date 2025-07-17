(* TEST
flags = "-extension-universe alpha ";

 expect;
*)

(* Comprehensive test of calling compatibility for optional arguments *)

(*= let f_or_null Stdlib.Or_null.?'(x: int or_null) () = x *)

let f_or_null Stdlib.Or_null.?'(x = 35) () = x

(* 7. f_or_null with Stdlib.Or_null.?'x:(This 1) *)
let _ = f_or_null  ()
(*= [%%expect{|
val f_or_null : Stdlib.Or_null.?'x:int -> unit -> int or_null = <fun>
- : int or_null = This 1
|}] *)
