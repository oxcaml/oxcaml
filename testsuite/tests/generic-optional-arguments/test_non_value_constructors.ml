(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

type ('b : bits32) bits32_option =
  | None32
  | Some32 of 'b
[@@option_like]
[%%expect{|
type ('b : bits32) bits32_option = None32 | Some32 of 'b
|}]

let f (?x : 'a bits32_option) () = x
let v = f ()
let v = f ~x:(#3l) ()
let v = f ?x:(Some32 #3l) ()
let v = f ?x:(None32) ()
let g (?(x = #2l) : int32# bits32_option) () = x
let v = Some32 (g())
let v = Some32 (g ~x:#3l ())
let v = Some32 (g ?x:(Some32 #3l) ())
let v = Some32 (g ?x:(None32) ())
[%%expect{|
val f : ('a : bits32). (?x):'a bits32_option -> unit -> 'a bits32_option =
  <fun>
val v : ('a : bits32). 'a bits32_option = None32
val v : int32# bits32_option = Some32 <abstr>
val v : int32# bits32_option = Some32 <abstr>
val v : ('a : bits32). 'a bits32_option = None32
val g : (?x):int32# bits32_option -> unit -> int32# = <fun>
val v : int32# bits32_option = Some32 <abstr>
val v : int32# bits32_option = Some32 <abstr>
val v : int32# bits32_option = Some32 <abstr>
val v : int32# bits32_option = Some32 <abstr>
|}]
(* CR generic-optional: check matching.ml access patterns *)
