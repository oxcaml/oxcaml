(* TEST
   compile_only = "true";
   flambda2;
   ocamlopt_flags += " -flambda2-join-points";
   setup-ocamlopt.opt-build-env;
   ocamlopt.opt;
 *)

(* This test is ensuring that we don't build array types with a Bottom
   element kind and non-empty contents, which was a regression introduced in
   commit fa3e7c1db8660b36aa0a94f7b5a99d6a2368329e (PR #5771).
   
   Such array types would cause crashes when trying to compute the join with
   another array that as a different kind in its fields. *)

type _ w =
  | Tagged_immediate : int iarray w
  | Naked_float : float# iarray w

let fail (type a) (w1 : a w) (w2 : a w) =
  let[@local] tagged_immediate (arr : int iarray) = ignore arr in
  let[@local] cast_naked_float (type a) (w : a w) (arr : a) =
    match w with
    | Tagged_immediate ->
        (* Here, [arr] is an array with element kind [Naked_float] and
           contents [[: #0.0 :]], but we are passing it to [tagged_immediate],
           which expects an array with element kind [Value].
           
           This means that it is actually impossible to execute this code path!
           It would be ideal to be able to remove it, but at the very least we
           should certainly not think that [arr] is a non-empty array with
           [Bottom] element kind.

           bclement: We are able to prove that this is impossible, but only
           when computing the typing envs for the join points, and we consider
           this sufficiently unusual that we don't try to actually remove the
           use. *)
      tagged_immediate arr
    | Naked_float -> assert false
  in
  match w1 with
  | Tagged_immediate -> tagged_immediate [: 0 :]
  | Naked_float -> cast_naked_float w2 [: #0.0 :]
