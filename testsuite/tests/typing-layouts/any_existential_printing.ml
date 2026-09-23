(* TEST
 flags = "-extension layouts_alpha";
 ocamlrunparam += ",s=4k";
 expect.opt;
*)

(* Printing values with existential fields of layout [any]. Each construction
   site grounds the existential to its own layout, so the block may or may
   not be a mixed block and the native toplevel cannot locate the fields: it
   must neither read them nor crash (the small minor heap above makes a GC
   during printing likely). *)

type ebox = E : ('a : any). { v : 'a; k : int } -> ebox
type etuple = F : ('a : any). 'a * int -> etuple
[%%expect{|
type ebox = E : ('a : any). { v : 'a; k : int; } -> ebox
type etuple = F : ('a : any). 'a * int -> etuple
|}]

let boxed = E { v = 5; k = 1 }, F (5, 1)
[%%expect{|
val boxed : ebox * etuple = (E <abstr>, F (<unknown>, <unknown>))
|}]

let unboxed = E { v = #42.5; k = 1 }, F (#42.5, 1)
[%%expect{|
val unboxed : ebox * etuple = (E <abstr>, F (<unknown>, <unknown>))
|}]
