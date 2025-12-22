(* TEST
 modules = "poly_types.ml util.ml";
 flags = "-extension runtime_metaprogramming";
 arch_amd64;
 native;
*)

#syntax quotations on

open Util
open Poly_types


(** Record with polymorphic field **)

let () =
  let (f : <[tr1]> expr) = <[fun rcd -> (rcd.f 42, rcd.f "abc")]> in
  test <[ ignore (((fun x -> x) $f) : tr1) ]>
;;


(** Object with polymorphic method **)

(* Method polymorphic in one variable *)
let () =
  let (f : <[to1]> expr) = <[fun obj -> (obj#f 42, obj#f "abc")]> in
  test <[ ignore (((fun x -> x) $f) : to1) ]>
;;

(* Method polymorphic in two variables *)
let () =
  let (f : <[to2]> expr) = <[fun obj -> (obj#f 42 false, obj#f "abc" 0)]> in
  test <[ ignore (((fun x -> x) $f) : to2) ]>
;;

(* Structured polymorphic spine: arrows, tuples, objects and variants *)
let () =
  let (f : <[to3]> expr) = <[fun obj -> (obj#f 42 0, obj#f "abc" 1)]> in
  test <[ ignore (((fun x -> x) $f) : to3) ]>
;;

(* Method polymorphic in a jkind-annotated variable *)
(* FIXME: Enable this test when TypePoly keeps track of jkind annotation *)
let () =
  let (f : <[to4]> expr) = <[fun obj -> (obj#f 42, obj#f true)]> in
  test ~eval:false <[ ignore (((fun x -> x) $f) : to4) ]>
;;

(* Method with recursive type *)
let () =
  let (f : <[tr]> expr) = <[fun obj -> (obj#f (obj#f 42 obj) obj, obj#f "abc" obj)]> in
  test <[ ignore (((fun x -> x) $f) : tr) ]>
;;


(** Polymorphic parameters: higher-rank function introduction **)

(* Parameter polymorphic in one variable *)
let () =
  let (f : <[t1]> expr) = <[fun f -> (f 42, f "abc")]>
  in
  test <[ ignore (((fun x -> x) $f) : t1) ]>
;;

(* Parameter polymorphic in two variables *)
let () =
  let (f : <[t2]> expr) = <[fun f -> (f 42 false, f "abc" 0)]> in
  test <[ ignore (((fun x -> x) $f) : t2) ]>
;;

(* Structured polymorphic spine *)
(* arrows, tuples, objects and polymorphic variants *)
let () =
  let (f : <[t3]> expr) = <[fun f -> (f 42 0, f "abc" 1)]> in
  test <[ ignore (((fun x -> x) $f) : t3) ]>
;;
(* polymorphic variants *)
let () =
  let (f : <[t3']> expr) = <[fun f -> (f 42 0, f "abc" 1)]> in
  test <[ ignore (((fun x -> x) $f) : t3') ]>
;;
(* FIXME: quotes and splices *)
(* Once enabled and passing, [poly_quote.ml] can be removed *)
(*
let () =
  let (f : <[t3'']> expr) =
    <[fun f -> (
      f () (fun x -> <[$x + 1]>),
      f () (fun x -> <[Int.to_string $x]>))]>
  in
  test <[ ignore (((fun x -> x) $f) : t3'') ]>
;;
*)
(* package types *)
let () =
  let (f : <[t3''']> expr) = <[fun f -> (f 42, f "abc")]> in
  test <[ ignore (((fun x -> x) $f) : t3''') ]>
;;

(* funky object types *)
let () =
  let (f : <[('a. < x : 'a; .. > -> 'a) -> int * string]> expr) =
    <[fun f -> (42, "abc")]>
  in
  test <[ignore (
      ((fun x -> x) $f) : ('a. < x : 'a; .. > -> 'a) -> int * string
    )]>
;;

(* funky polymorphic variant types *)
let () =
  let (f : <[('a 'b. [< `Foo of 'a & 'b | 'b tbar ]
                  -> [> `Foo of 'a | `Baz ]) -> unit]> expr) =
    <[fun f -> ()]>
  in
  test <[ignore (
      ((fun x -> x) $f) : ('a 'b. [< `Foo of 'a & 'b | 'b tbar ]
                               -> [> `Foo of 'a | `Baz ]) -> unit
    )]>
;;

(* Parameter polymorphic in a jkind-annotated variable *)
(* FIXME: Enable this test when TypePoly keeps track of jkind annotation *)
let () =
  let (f : <[t4]> expr) = <[fun f -> (f 42, f true)]> in
  test ~eval:false <[ ignore (((fun x -> x) $f) : t4) ]>
;;


(** Polymorphic parameters: higher-rank function elimination **)

(* Parameter polymorphic in one variable *)

let () =
  let (f : <[te1]> expr) = <[fun f -> f (fun x -> x)]> in
  test <[ ignore (((fun x -> x) $f) : te1) ]>
;;

(* Parameter polymorphic in two variables *)
let () =
  let (f : <[te2]> expr) = <[fun f -> f (fun x y -> x)]> in
  test <[ ignore (((fun x -> x) $f) : te2) ]>
;;

(* Structured polymorphic spine: arrows, tuples and variants *)
let () =
  let (f : <[te3']> expr) = <[fun f -> f (fun x ->
    Sys.opaque_identity (fun y -> (`Foo x, y)))]> in
  test <[ ignore (((fun x -> x) $f) : te3') ]>
;;

(* Parameter polymorphic in a jkind-annotated variable *)
(* FIXME: Enable this test when TypePoly keeps track of jkind annotation *)
let () =
  let (f : <[te4]> expr) = <[fun f -> f (fun x -> x)]> in
  test ~eval:false <[ ignore (((fun x -> x) $f) : te4) ]>
;;
