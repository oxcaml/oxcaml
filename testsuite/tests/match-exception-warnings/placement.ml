(* TEST
   expect;
*)

(*****************************************************)
(* Restrict where "exception P" patterns can appear. *)
(*****************************************************)

(* should be accepted *)

let f x = match x () with _ -> () | exception _ -> ()

[%%expect {|
val f : (unit -> 'a) -> unit = <fun>
|}]

let f x = match x () with _ | (exception _) -> ()

[%%expect {|
val f : (unit -> 'a) -> unit = <fun>
|}]

let f x = match x () with Arg.(Set _ | (exception Bad _)) -> () | _ -> ()

[%%expect {|
val f : (unit -> Arg.spec) -> unit = <fun>
|}]

let f x = match x () with _ -> () | ((exception (_ : exn)) : int) -> ()

[%%expect {|
val f : (unit -> int) -> unit = <fun>
|}]

(* should be rejected *)

let f x =
  try
    x ();
    ()
  with exception _ -> ()

[%%expect
{|
Line 5, characters 7-18:
5 |   with exception _ -> ()
           ^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]

let f x = match x () with (exception _) as _pat -> () | _ -> ()

[%%expect
{|
Line 1, characters 26-39:
1 | let f x = match x () with (exception _) as _pat -> () | _ -> ()
                              ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]

let f x = match x () with _, (exception _), _ -> ()

[%%expect
{|
Line 1, characters 29-42:
1 | let f x = match x () with _, (exception _), _ -> ()
                                 ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]

let f x = match x () with (lazy (exception _)) -> () | _ -> ()

[%%expect
{|
Line 1, characters 32-45:
1 | let f x = match x () with (lazy (exception _)) -> () | _ -> ()
                                    ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]

let f x = match x () with { contents = (exception _) } -> ()

[%%expect
{|
Line 1, characters 39-52:
1 | let f x = match x () with { contents = (exception _) } -> ()
                                           ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]

let f x = match x () with [| (exception _) |] -> ()

[%%expect
{|
Line 1, characters 29-42:
1 | let f x = match x () with [| (exception _) |] -> ()
                                 ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]

let f x = match x () with Some (exception _) -> ()

[%%expect
{|
Line 1, characters 31-44:
1 | let f x = match x () with Some (exception _) -> ()
                                   ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]

let f x = match x () with `A (exception _) -> ()

[%%expect
{|
Line 1, characters 29-42:
1 | let f x = match x () with `A (exception _) -> ()
                                 ^^^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]

let f = function exception _ -> () | _ -> ()

[%%expect
{|
Line 1, characters 17-28:
1 | let f = function exception _ -> () | _ -> ()
                     ^^^^^^^^^^^
Error: Exception patterns are not allowed in this position.
|}]
