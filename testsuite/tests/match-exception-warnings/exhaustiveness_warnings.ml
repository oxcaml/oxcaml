(* TEST
   expect;
*)

(** Test exhaustiveness.

    match clauses should continue to give warnings about inexhaustive
    value-matching clauses when there is an exception-matching clause
 *)

let test_match_exhaustiveness () =
  match None with exception e -> () | Some false -> () | None -> ()

[%%expect
{|
Line 8, characters 2-67:
8 |   match None with exception e -> () | Some false -> () | None -> ()
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some true

val test_match_exhaustiveness : unit -> unit = <fun>
|}]

let test_match_exhaustiveness_nest1 () =
  match None with Some false -> () | None | (exception _) -> ()

[%%expect
{|
Line 2, characters 2-63:
2 |   match None with Some false -> () | None | (exception _) -> ()
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some true

val test_match_exhaustiveness_nest1 : unit -> unit = <fun>
|}]

let test_match_exhaustiveness_nest2 () =
  match None with Some false | (exception _) -> () | None -> ()

[%%expect
{|
Line 2, characters 2-63:
2 |   match None with Some false | (exception _) -> () | None -> ()
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some true

val test_match_exhaustiveness_nest2 : unit -> unit = <fun>
|}]

let test_match_exhaustiveness_full () =
  match None with
  | exception e -> ()
  | Some false | (exception _) -> ()
  | None | (exception _) -> ()

[%%expect
{|
Lines 2-5, characters 2-30:
2 | ..match None with
3 |   | exception e -> ()
4 |   | Some false | (exception _) -> ()
5 |   | None | (exception _) -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some true

Line 4, characters 28-29:
4 |   | Some false | (exception _) -> ()
                                ^
Warning 11 [redundant-case]: this match case is unused.

Line 5, characters 22-23:
5 |   | None | (exception _) -> ()
                          ^
Warning 11 [redundant-case]: this match case is unused.

val test_match_exhaustiveness_full : unit -> unit = <fun>
|}]
