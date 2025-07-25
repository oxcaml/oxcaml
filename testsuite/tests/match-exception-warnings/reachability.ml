(* TEST
   expect;
*)

let f x = match x with _ -> () | exception _ -> .

[%%expect
{|
Line 1, characters 43-44:
1 | let f x = match x with _ -> () | exception _ -> .
                                               ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "_"
|}]

let f x = match x with _ -> () | None | (exception _) -> .

[%%expect
{|
Line 1, characters 51-52:
1 | let f x = match x with _ -> () | None | (exception _) -> .
                                                       ^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "_"
|}]

let f x = match x with _ -> () | (exception Not_found) | None -> .

[%%expect
{|
Line 1, characters 44-53:
1 | let f x = match x with _ -> () | (exception Not_found) | None -> .
                                                ^^^^^^^^^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "Not_found"
|}]

let f x = match x with _ | (exception _) -> () | exception Not_found -> .

[%%expect {|
val f : 'a -> unit = <fun>
|}]
