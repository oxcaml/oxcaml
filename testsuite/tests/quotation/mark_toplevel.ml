(* TEST
 flags = "-extension runtime_metaprogramming -extension comprehensions";
 expect;
*)

(* Test the [#mark_toplevel_in_quotations] directive,
   intended for making names available as top-level in quotation tests. *)

#syntax quotations on

(** Types **)
type t = int
[%%expect {|
type t = int
|}];;
(* Fails, as [t] is not top-level *)
let (x : <[t]> expr) = <[42]>
[%%expect {|
Line 1, characters 11-12:
1 | let (x : <[t]> expr) = <[42]>
               ^
Error: Identifier "t" is used at line 1, characters 11-12,
       inside a quotation (<[ ... ]>);
       it is introduced at line 1, characters 0-12, outside any quotations.
|}];;
#mark_toplevel_in_quotations;;
(* [t] is now considered top-level *)
let (x : <[t]> expr) = <[42]>
[%%expect {|
val x : <[t]> expr = <[42]>
|}];;

(* The directive does not apply to future definitions *)
type s = int
let (x : <[s]> expr) = <[42]>
[%%expect {|
type s = int
Line 2, characters 11-12:
2 | let (x : <[s]> expr) = <[42]>
               ^
Error: Identifier "s" is used at line 2, characters 11-12,
       inside a quotation (<[ ... ]>);
       it is introduced at line 1, characters 0-12, outside any quotations.
|}];;

(** Modules **)
module M : sig
  type t
end = struct
  type t = int
end
[%%expect {|
module M : sig type t end
|}];;
let id (x : <[M.t]> expr) = x
[%%expect {|
Line 1, characters 14-17:
1 | let id (x : <[M.t]> expr) = x
                  ^^^
Error: Identifier "M" is used at line 1, characters 14-17,
       inside a quotation (<[ ... ]>);
       it is introduced at file "_none_", line 1, outside any quotations.
|}];;
#mark_toplevel_in_quotations;;
let id (x : <[M.t]> expr) = x
[%%expect {|
val id : <[M.t]> expr -> <[M.t]> expr = <fun>
|}];;

(** Records **)
type r = { x : int }
[%%expect {|
type r = { x : int; }
|}];;
let r = <[ { x = 42 } ]>
[%%expect {|
Line 1, characters 13-14:
1 | let r = <[ { x = 42 } ]>
                 ^
Error: Label "x" used at line 1, characters 13-14
       cannot be used in this context;
       "x" is not defined inside a quotation (<[ ... ]>).
Hint: Label "x" is defined outside any quotations.
|}];;
#mark_toplevel_in_quotations;;
let r = <[ { x = 42 } ]>
[%%expect {|
val r : <[r]> expr = <[{ x = 42; }]>
|}];;
