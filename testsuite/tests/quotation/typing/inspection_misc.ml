(* TEST
 modules = "util.ml b.ml";
 flags = "-extension runtime_metaprogramming";
 native;
*)

#syntax quotations on

open Util

(* Propagating constraints on value bindings to pattern *)
let e_prop = test <[
    let x = ("abc" : string @ local) in
    let _ : string @ local = x in ()
  ]>


(* Inferring format6 on a string *)
let e_format6 =
  let f (x : <[ _ format6 ]> expr) = x in
  test <[let p = $(f <[ "e_format6 says: %s %s\n\n" ]>) in Format.printf p "abc" "xyz" ]>

(* Eliminating optional arguments *)

(* CR jbachurski: To eval this example, we need to prevent the eta expansion
   introduced by optional argument elimination from introducing
   a local-returning exclave *)

let e_elim =
  let g = <[ fun ?(x = 0) y -> Format.printf "%d\n" (x + y) ]> in
  test ~eval:false <[ let f = $g in List.iter f [1; 2; 3] ]>

(* Module packing *)

(* CR jbachurski: struct..end not supported in quotes *)
(*
let e_pack =
  let m = <[ let module M = struct let f x y = x + y end in (module M) ]> in
  test <[ let (module M) = $m in M.f 2 2 ]>
*)

(* Polymorphic method calls *)

(* CR jbachurski: object..end not supported in quotes *)
(*
let e_poly =
  let (o : <[ <f:'a. 'a B.nested -> _ > ]> expr) = <[
    object method f:'a. 'a B.nested -> 'b = B.len end
  ]> in
  test <[ let f = o#f in f (List [1; 2; 3]) ]>
*)

(* Higher-rank function definition *)

let e_higher_rank =
  let len = <[
      let map_and_sum f = List.fold_left (fun acc x -> acc + f x) 0 in
      let rec len : 'a. ('a list -> int) -> 'a B.nested -> int =
        fun nested_len n ->
          match n with
          | List l -> nested_len l
          | Nested n -> len (map_and_sum nested_len) n
      in
      len
    ]>
  in
  test <[
    let len = $len in
    len List.length (B.Nested (List [ [1;2;3]; [4;5;6;7]; [] ]))
    |> Format.printf "%d\n"
  ]>
