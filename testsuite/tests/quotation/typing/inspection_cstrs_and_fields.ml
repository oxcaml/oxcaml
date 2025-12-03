(* TEST
 modules = "a.ml util.ml";
 flags = "-extension runtime_metaprogramming";
 native;
*)

#syntax quotations on

open Util

let () =
  let open A in
  let (v : <[vrt]> expr) = <[Foo]> in
  test <[ ignore ((fun x -> x) $v : vrt) ]>
;;

let () =
  let open A in
  let (r : <[rcd]> expr) = <[{ foo = 123; bar = "abc"}]> in
  test <[ ignore (((fun x -> x) $r) : rcd) ]>
;;

let () =
  let open A in
  let (r : <[rcd -> int]> expr) = <[fun r -> r.foo]> in
  test <[ ignore (((fun x -> x) $r) : rcd -> int) ]>
;;
