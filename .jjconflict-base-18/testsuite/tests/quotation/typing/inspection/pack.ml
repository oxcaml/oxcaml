(* TEST
 include eval;
 modules = "pack_types.ml util.ml";
 flags = "-extension runtime_metaprogramming -uses-metaprogramming";
 native;
*)

#syntax quotations on

open Util
open Pack_types

(* Trivial cases without package constraints *)

let () =
  let (f : <[(module S) -> int]> expr) =
    <[fun (module M) -> M.i]>
  in
  test <[ ignore (((fun x -> x) $f) : (module S) -> int) ]>
;;

let () =
  let (f : <[(module S) -> (module S)]> expr) =
    <[fun (module M) -> (module M)]>
  in
  test <[ ignore (((fun x -> x) $f) : (module S) -> (module S)) ]>
;;

(* Package constraint in pattern *)

let () =
  let (f : <[(module S with type t = 'a) -> 'a]> expr) =
    <[fun (type a) (module M : S with type t = a) -> M.x]>
  in
  test <[ ignore (((fun x -> x) $f) : (module S with type t = 'a) -> 'a) ]>
;;
let () =
  let (f : <[((module S with type t = string) as 'b) -> 'b]> expr) =
    <[fun (module M : S with type t = string) -> (module M)]>
  in
  test <[
    ignore (((fun x -> x) $f) :
      (module S with type t = string) -> (module S with type t = string)) ]>
;;
let () =
  let (f : <[((module S with type t = 'a) as 'b) -> 'b]> expr) =
    <[fun (type a) (module M : S with type t = a) ->
        (module M : S with type t = a)]>
  in
  test <[
    ignore (((fun x -> x) $f) :
      (module S with type t = 'a) -> (module S with type t = 'a)) ]>
;;

(* Package constraint inferred from the rest of the pattern *)

(* M.t = string *)
let () =
  let (f : <[w -> int]> expr) =
    <[fun (W (module M)) -> M.i]>
  in
  test <[ ignore (((fun x -> x) $f) : w -> int) ]>
;;
let () =
  let (f : <[w -> string]> expr) =
    <[fun (W (module M)) -> M.x]>
  in
  test <[ ignore (((fun x -> x) $f) : w -> string) ]>
;;
let () =
  let (f : <[w -> w]> expr) =
    <[fun (W (module M)) -> W (module M)]>
  in
  test <[ ignore (((fun x -> x) $f) : w -> w) ]>
;;

(* M.t = int * string *)
let () =
  let (f : <[v -> int]> expr) =
    <[fun (V (module M)) -> M.i]>
  in
  test <[ ignore (((fun x -> x) $f) : v -> int) ]>
;;
let () =
  let (f : <[v -> string]> expr) =
    <[fun (V (module M)) -> snd M.x]>
  in
  test <[ ignore (((fun x -> x) $f) : v -> string) ]>
;;
let () =
  let (f : <[v -> v]> expr) =
    <[fun (V (module M)) -> V (module M)]>
  in
  test <[ ignore (((fun x -> x) $f) : v -> v) ]>
;;

(* M.t = 'a * b, where 'a and 'b are type parameters:
   These tests will fail if the inserted annotation
   relies on inferring the concrete type constraint. *)
let () =
  let (f : <[(int, string) u -> int]> expr) =
    <[fun (U (module M)) -> M.i]>
  in
  test <[ ignore (((fun x -> x) $f) : (int, string) u -> int) ]>
;;
let () =
  let (f : <[(int, string) u -> string]> expr) =
    <[fun (U (module M)) -> snd M.x]>
  in
  test <[ ignore (((fun x -> x) $f) : (int, string) u -> string) ]>
;;
let () =
  let (f : <[(int, string) u -> (int, string) u]> expr) =
    <[fun (U (module M)) -> U (module M)]>
  in
  test <[ ignore (((fun x -> x) $f) : (int, string) u -> (int, string) u) ]>
;;
