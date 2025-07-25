(* TEST
   expect;
*)

let test f = match f () with exception Not_found -> ()

[%%expect
{|
Line 1, characters 13-54:
1 | let test f = match f () with exception Not_found -> ()
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: None of the patterns in this "match" expression match values.
|}]
