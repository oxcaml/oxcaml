(* TEST
 modules = "labels_types.ml util.ml";
 flags = "-extension runtime_metaprogramming";
 arch_amd64;
 native;
*)

#syntax quotations on

open Util
open Labels_types

(* These tests generally all follow the same structure:
   they introduce a quoted expression in a context where we can disambiguate a type,
   and later splice it under a constraint, but where disambiguation can't happen. *)


(* Records *)

(* Construction *)
let () =
  let (r : <[rcd]> expr) = <[{ foo = 123; bar = "abc"}]> in
  test <[ ignore (((fun x -> x) $r) : rcd) ]>
;;

(* Field access *)
let () =
  let (r : <[rcd -> int]> expr) = <[fun r -> r.foo]> in
  test <[ ignore (((fun x -> x) $r) : rcd -> int) ]>
;;

(* Field set *)
let () =
  let (r : <[rcd -> unit]> expr) = <[fun r -> r.bar <- "xyz"]> in
  test <[ ignore (((fun x -> x) $r) : rcd -> unit) ]>
;;

(* Pattern *)
let () =
  let (r : <[rcd -> int]> expr) = <[function { foo; _ } -> foo]> in
  test <[ ignore (((fun x -> x) $r) : rcd -> int) ]>
;;


(* Variants *)

(* Constructor *)
let () =
  let (v : <[vrt]> expr) = <[Foo]> in
  test <[ ignore ((fun x -> x) $v : vrt) ]>
;;

(* Pattern *)
let () =
  let (v : <[vrt -> int]> expr) = <[function Foo -> 0 | Bar -> 1 ]> in
  test <[ ignore ((fun x -> x) $v : vrt -> int) ]>
;;


(* Unboxed records *)

(* Construction *)
let () =
  let (r : <[urcd box_imm_imm]> expr) = <[{box = #{ foo = 0; bar = 0}}]> in
  test <[ ignore (((fun x -> x) $r) : urcd box_imm_imm) ]>
;;

(* Field access *)
let () =
  let (r : <[urcd -> unit]> expr) = <[fun r -> let _ = r.#foo in ()]> in
  test <[ ignore (((fun x -> x) $r) : urcd -> unit) ]>
;;

(* Pattern *)
let () =
  let (r : <[urcd -> int]> expr) = <[function #{ foo; _ } -> foo]> in
  test <[ ignore (((fun x -> x) $r) : urcd -> int) ]>
;;


(* Parameterised type constructors *)

(* Unary *)
let () =
  let (r : <[_ rcd1]> expr) = <[{ foo = 123; bar = "abc"}]> in
  test <[ ignore (((fun x -> x) $r) : _ rcd1) ]>
;;

(* Binary *)
let () =
  let (r : <[(_, _) rcd2]> expr) = <[{ foo = 123; bar = "abc"}]> in
  test <[ ignore (((fun x -> x) $r) : (_, _) rcd2) ]>
;;

(* Binary - short-hand wildcard *)
let () =
  let (r : <[_ rcd2]> expr) = <[{ foo = 123; bar = "abc"}]> in
  test <[ ignore (((fun x -> x) $r) : _ rcd2) ]>
;;

(* Ternary *)
let () =
  let (r : <[(_, _, _) rcd3]> expr) = <[{ foo = 123; bar = "abc"}]> in
  test <[ ignore (((fun x -> x) $r) : (_, _, _) rcd3) ]>
;;
