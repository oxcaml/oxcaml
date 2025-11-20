(* TEST
 readonly_files = "hash_type_aliases.ml";
 setup-ocamlc.byte-build-env;
 module = "hash_type_aliases.ml";
 flags = "-short-paths";
 ocamlc.byte;
 expect;
*)

#directory "ocamlc.byte";;
#load "hash_type_aliases.cmo";;

(* [float#] beats [Hash_type_aliases.f64] *)
let f : unit -> Hash_type_aliases.f64 = fun () -> ()

[%%expect {|
Line 1, characters 50-52:
1 | let f : unit -> Hash_type_aliases.f64 = fun () -> ()
                                                      ^^
Error: This expression has type "unit" but an expression was expected of type
         "float#"
|}]

(* [f64] beats [float#] *)
open Hash_type_aliases
let f : unit -> f64 = fun () -> ()

[%%expect {|
Line 2, characters 32-34:
2 | let f : unit -> f64 = fun () -> ()
                                    ^^
Error: This expression has type "unit" but an expression was expected of type
         "f64"
|}]

(* [U.u] beats [R.r#] *)
let f : unit -> R.r# = fun x -> x 

[%%expect {|
Line 1, characters 32-33:
1 | let f : unit -> R.r# = fun x -> x
                                    ^
Error: This expression has type "unit" but an expression was expected of type
         "U.u"
|}]

(* [r#] beats [U.u] *)
open R
let f : unit -> U.u = fun x -> x 

[%%expect {|
Line 2, characters 31-32:
2 | let f : unit -> U.u = fun x -> x
                                   ^
Error: This expression has type "unit" but an expression was expected of type
         "r#"
|}]

(* [u] beats [r#] *)
open U
let f : unit -> r# = fun x -> x 

[%%expect {|
Line 2, characters 30-31:
2 | let f : unit -> r# = fun x -> x
                                  ^
Error: This expression has type "unit" but an expression was expected of type "u"
|}]
