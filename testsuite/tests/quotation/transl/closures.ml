(* TEST
  flags = "-extension runtime_metaprogramming -gno-upstream-dwarf -g";
  native;
*)

#syntax quotations on

(* This file contains some test-cases where we ended up with heap-to-stack
   pointers when constructing [Lfunction]s for binders. *)

(* Make sure nesting binder-generated closures behaves *)

let _ = <[let a = 1 in a + 1]>

let _ = <[let a = 1 in let b = 2 in a + b + 1]>

let _ = <[let a = 1 in let b = 2 in let c = 3 in a + b + c + 1]>

(* The binders translate into closures, which mention stack-allocated [x]
   inside the splice below. They must not heap-allocate the closures. *)

let _ =
  let x = stack_ Some 42 in
  <[ let a = 1 in let b = 2 in
     $(match x with
       | Some x -> <[ a + b * $(Quote.Expr.int x) ]>
       | None -> <[ a / b ]>) ]>

(* Original test that broke when heap-allocating binder-generated closures *)
let _ =
  let n = 10000 in
  <[ fun () ->
     let knocked_out = ref 0 in
     let _ : _ =
      (* It is key that [loop] is stack-allocated, so this test might pass
         with [loop @ global]. *)
       $(let rec (loop @ local) : _ @ global -> _ @ local once = fun #(acc, n) ->
           if n <= 0
           then <[ [] ]>
           else
             <[ let knocked_out_date =
                  try raise Not_found with
                  | Not_found -> 42
                in
                let date = knocked_out_date in
                knocked_out := date;
                $(loop #(acc, n - 1)) ]>
         in
         loop #(<[ [] ]>, n))
     in
     42 ]>
;;
