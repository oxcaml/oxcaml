(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

(* This test file tests that constructing a higher-rank function
   of quotes under quotes fails gracefully until they are supported,
   and the corresponding test in [poly.ml] can be enabled. *)

(* This type should be the same as [B.t3''] *)
let (f : <[
  ('a 'b. unit -> ('a expr -> 'b expr) -> <[unit -> $('a) -> $('b)]> expr) ->
  <[unit -> int -> int]> expr * <[unit -> int -> string]> expr
]> expr) =
  <[fun f -> (
    f () (fun x -> <[$x + 1]>),
    f () (fun x -> <[Int.to_string $x]>))]>

[%%expect {|
Line 6, characters 19-29:
6 |     f () (fun x -> <[$x + 1]>),
                       ^^^^^^^^^^
Error: This expression has type "<['a]> expr"
       but an expression was expected of type "'b expr"
       The layout of <['a]> is any
         because it's the type of an expression inside of a quote.
       But the layout of <['a]> must be a sublayout of value
         because it is or unifies with an unannotated universal variable.
|}]
