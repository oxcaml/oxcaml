(* TEST
 flambda2;
 flags = "-dflambda-heavy-invariants";
 flags += " -dcmm-invariants -dcfg-invariants";
 {
   ocamlopt_flags = "-Oclassic";
   native;
 }{
   ocamlopt_flags = "-O3";
   native;
 }
*)

(* The following program fails with kind checks, but should pass
   with other invariant checks. *)

type (_, _) t =
  | Refl : ('a, 'a) t

let g () =
  let[@local] f (type a) (x : a) =
    match (Sys.opaque_identity None : (a, int -> int) t option) with
    | Some Refl -> x 0
    | None -> 0
  in
  f 0
