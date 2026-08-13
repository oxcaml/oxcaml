(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* Tests that we can encode eval-constraints ['a eval = 'b] using GADTs. *)

type ('a, 'b) evals = Persistent : ('a, 'a eval) evals
[%%expect {|
type ('a, 'b) evals = Persistent : ('a, 'a eval) evals
|}]

(* [eval] stub, with and without the witness *)
open (struct
  let eval w x = x |> Obj.magic_many |> Obj.magic
end : sig
  val eval : ('a, 'b) evals -> 'a expr @ once -> 'b
end)
let eval' x = eval Persistent x
[%%expect {|
val eval : ('a, 'b) evals -> 'a expr @ once -> 'b = <fun>
val eval' : 'a expr -> 'a eval = <fun>
|}]

(*** Basics ***)

let f (x : <[int]> expr) = eval Persistent x
[%%expect {|
val f : <[int]> expr -> int = <fun>
|}]

let f x : int = eval Persistent x
[%%expect {|
Line 1, characters 16-33:
1 | let f x : int = eval Persistent x
                    ^^^^^^^^^^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "int"
|}]

let f x = eval (Persistent : (<[int]>, _) evals) x
[%%expect {|
val f : <[int]> expr -> int = <fun>
|}]

(*** Delaying persistence proofs ***)

(** A simple example that also type-checks without explicit witnesses **)

let f u v x y = if eval u x then eval v y else 0
let g () = f Persistent Persistent <[true]> <[42]>
[%%expect {|
val f : ('a, bool) evals -> ('b, int) evals -> 'a expr -> 'b expr -> int =
  <fun>
Line 4, characters 13-23:
4 | let g () = f Persistent Persistent <[true]> <[42]>
                 ^^^^^^^^^^
Error: The constructor "Persistent" has type "('a, 'a eval) evals"
       but an expression was expected of type "('a, bool) evals"
       Type "'a eval" is not compatible with type "bool"
|}]
let f' x y = if eval' x then eval' y else 0
let g' () = f' <[true]> <[42]>
[%%expect {|
Line 1, characters 16-23:
1 | let f' x y = if eval' x then eval' y else 0
                    ^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "bool"
       because it is in the condition of an if-statement
|}]

(** More complex example which needs a witness for ['a eval = 'b list] **)

let f w xs e = eval w xs |> List.map e
[%%expect {|
val f : ('a, 'b list) evals -> 'a expr -> ('b -> 'c) -> 'c list = <fun>
|}]
let g () =
  f Persistent
    <[ [0; 1; 2] ]> (fun x -> x + 1)
[%%expect {|
Line 2, characters 4-14:
2 |   f Persistent
        ^^^^^^^^^^
Error: The constructor "Persistent" has type "('a, 'a eval) evals"
       but an expression was expected of type "('a, 'b list) evals"
       Type "'a eval" is not compatible with type "'b list"
|}]
(* A type annotation is necessary to narrow ['a] to [int list] *)
let g () =
    f (Persistent : (<[int list]>, _) evals)
      <[ [0; 1; 2] ]> (fun x -> x + 1)
[%%expect {|
val g : unit -> int list = <fun>
|}]

(* Without a witness, we cannot constrain ['a eval = 'b list]:
   we would have to make ['a] non-generalizable to make it persistent *)
let f' xs e = eval' xs |> List.map e
[%%expect {|
Line 1, characters 14-22:
1 | let f' xs e = eval' xs |> List.map e
                  ^^^^^^^^
Error: This expression has type "'a eval"
       but an expression was expected of type "'b list"
|}]
