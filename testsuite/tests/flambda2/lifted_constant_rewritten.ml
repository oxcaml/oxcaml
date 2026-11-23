(* TEST *)

(* Regression test for lifted constants being dropped in the [Rewritten] case
   of [Simplify_let_expr.simplify_let0]. *)

let x = Sys.opaque_identity 1
let g = Sys.opaque_identity (fun (_ : bool) -> ())

let () =
  g
    (Bool.equal false
       (let _k = Sys.opaque_identity (fun (y : int) -> y) in
        x = 2))
