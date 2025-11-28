(* TEST
 modules = "a.ml util.ml";
 flags = "-extension runtime_metaprogramming";
 native;
*)

#syntax quotations on

open Util

let () =
  let open A in
  test <[ ignore Foo ]>
;;

let () =
  let open A in
  test <[ ignore ({ foo = 123; bar = "abc"}.foo) ]>
;;
