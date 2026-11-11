(* TEST
 flags = "-extension-universe upstream_compatible";
 expect;
*)

(* Expected-type propagation into applications is a non-erasable typing
   change, so it is disabled when only erasable extensions are allowed:
   these programs must be rejected exactly as upstream rejects them. *)

type t = A | B
type s = A | C

let id x = x

let _ = (id A : t), (id A : s)
[%%expect{|
type t = A | B
type s = A | C
val id : 'a -> 'a = <fun>
Line 6, characters 9-13:
6 | let _ = (id A : t), (id A : s)
             ^^^^
Error: This expression has type "s" but an expression was expected of type "t"
|}]

(* Coercing an application result still eliminates optional arguments. *)

let with_optional x ?(opt = 0) () = x + opt
let h : unit -> int = with_optional 1
let () = assert (h () = 1)
[%%expect{|
val with_optional : int -> ?opt:int -> unit -> int = <fun>
val h : unit -> int = <fun>
|}]

(* Propagation must not introduce coercion into a pair component here. *)

let with_default ?(opt = 0) x = opt + x
[%%expect{|
val with_default : ?opt:int -> int -> int = <fun>
|}]

let h : (int -> int) * int = (fun x -> x, 0) with_default
[%%expect{|
Line 1, characters 29-57:
1 | let h : (int -> int) * int = (fun x -> x, 0) with_default
                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(?opt:int -> int -> int) * int"
       but an expression was expected of type "(int -> int) * int"
       The first argument is labeled "?opt",
       but an unlabeled argument was expected
Hint: This function application is partial, maybe some arguments are missing.
|}]
