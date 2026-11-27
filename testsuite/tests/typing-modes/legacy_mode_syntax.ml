(* TEST
 flags = "-legacy-mode-syntax";
 expect;
*)

(* With [-legacy-mode-syntax], the legacy [local_] and [global_] syntax is
   still accepted. [syntax-error.ml] checks that it is rejected with
   [-no-legacy-mode-syntax]. *)

let f (local_ x) = x
[%%expect{|
val f : 'a @ local -> 'a @ local = <fun>
|}]

let g x = exclave_ local_ (x, x)
[%%expect{|
val g : 'a -> 'a * 'a @ local = <fun>
|}]

type t = local_ int -> int
[%%expect{|
type t = int @ local -> int
|}]

type r = { global_ x : int }
[%%expect{|
type r = { x : int @@ global; }
|}]

type v = Foo of global_ int
[%%expect{|
type v = Foo of int @@ global
|}]

(* [local_] is still a keyword, which can be used as a raw identifier and as
   an attribute name. (Using it as a label is a lexer error, which expect tests
   cannot check; see [syntax-error.ml].) *)
let \#local_ = 1
[%%expect{|
val \#local_ : int = 1
|}]

let _ = (1 [@local_])
[%%expect{|
- : int = 1
|}]
