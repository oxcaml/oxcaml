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
>> Fatal error: Translquote [at lines 5-7, characters 4-41]: splices cannot appear in the spine of a quoted higher-rank function type
Uncaught exception: Misc.Fatal_error

|}]
