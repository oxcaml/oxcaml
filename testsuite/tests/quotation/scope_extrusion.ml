(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

let ignore (_ @ once) = ()
[%%expect {|
val ignore : 'a @ once -> unit = <fun>
|}];;

(* Scope extrusion with a ref-cell *)

let cell = ref <[ 0 ]>
let gensym_ref () =
  ignore <[ let x = 42 in $(cell := <[x]>; <[()]>) ]>;
  !cell
[%%expect {|
val cell : <[int]> expr ref = {contents = <[0]>}
val gensym_ref : unit -> <[int]> expr = <fun>
|}];;

(* caught by printing *)
gensym_ref ()
[%%expect {|
- : <[int]> expr =
Uncaught exception: Failure("Identifier x bound at file , line 3, characters 12-50\nis extruded outside its scope:\nit is used at file , line 3, characters 38-39\ninside the quote at file , line 3, characters 36-41")

|}];;

(* caught by splicing *)
let () = ignore <[ $(gensym_ref ()) ]>
[%%expect {|
Exception:
Failure
 "Identifier x bound at file , line 3, characters 12-50\nis extruded outside its scope:\nit is used at file , line 3, characters 38-39\ninside the quote at file , line 3, characters 36-41".
|}];;

(* Scope extrusion with an exception *)

exception Extrude of <[int]> expr
let gensym_exn () =
  try <[ let x = 42 in $(raise (Extrude <[x]>)) ]>
  with Extrude e -> e
[%%expect {|
exception Extrude of <[int]> expr
val gensym_exn : unit -> <[int]> expr @ once = <fun>
|}];;

(* caught by printing *)
gensym_exn ()
[%%expect {|
- : <[int]> expr =
Uncaught exception: Failure("Identifier x bound at file , line 3, characters 9-47\nis extruded outside its scope:\nit is used at file , line 3, characters 42-43\ninside the quote at file , line 3, characters 40-45")

|}];;

(* caught by splicing *)
let () = ignore <[ $(gensym_exn ()) ]>
[%%expect {|
Exception:
Failure
 "Identifier x bound at file , line 3, characters 9-47\nis extruded outside its scope:\nit is used at file , line 3, characters 42-43\ninside the quote at file , line 3, characters 40-45".
|}];;

(* should be caught early if we splice the [expr] earlier *)
let gensym_exn () =
  try <[ let x = 42 in $(raise (Extrude <[x]>)) ]>
  with Extrude e -> <[ $e ]>
;;
gensym_exn ()
[%%expect {|
val gensym_exn : unit -> <[int]> expr @ once = <fun>
Exception:
Failure
 "Identifier x bound at file , line 2, characters 9-47\nis extruded outside its scope:\nit is used at file , line 2, characters 42-43\ninside the quote at file , line 2, characters 40-45".
|}];;

(* No scope extrusion in continuing effect handler *)

type _ Effect.t += Extrude : <[int]> expr -> <[int]> expr Effect.t
[%%expect {|
type _ Stdlib.Effect.t += Extrude : <[int]> expr -> <[int]> expr Effect.t
|}];;

(* Even though [$x] is syntactically outside its binder,
   it appears in a handler that continues back under the binder. *)
let safe_eff () =
  match <[ fun () -> let x = 42 in $(Effect.perform (Extrude <[x]>)) ]> with
  | x -> x
  | effect Extrude x, k -> Effect.Deep.continue k (Obj.magic_many <[ $x ]>)
;;
safe_eff ()
[%%expect {|
val safe_eff : unit -> <[unit -> int]> expr = <fun>
- : <[unit -> int]> expr = <[fun () -> let x = 42 in x]>
|}];;

(* We can print if we are still in scope *)
let cell = ref ""
let safe_eff () =
  match <[ fun () -> let x = 42 in $(Effect.perform (Extrude <[x]>)) ]> with
  | x -> x
  | effect Extrude x, k ->
    cell := Quote.string_of_expr x;
    Effect.Deep.continue k (Obj.magic_many <[ $x ]>)
;;
safe_eff ();; !cell
[%%expect {|
val cell : string ref = {contents = ""}
val safe_eff : unit -> <[unit -> int]> expr = <fun>
- : <[unit -> int]> expr = <[fun () -> let x = 42 in x]>
- : string = "x"
|}];;
