(* TEST
   flambda2;
   flags += "-flambda2-reaper -reaper-debug-flags=nostamps";
   {
     native with dump-reaper;
     check-fexpr-dump;
   }{
     flags += " -X reaper-lambda-lifting=1";
     fexpr_reference_suffix = "lambda-lifting.reference";
     native with dump-reaper;
     check-fexpr-dump;
   }
 *)

let f1 x =
  (* The closure for [g] should not be unboxed by default (only with
     [-X reaper-lambda-lifting=1]). That way, resimplifying [f1] will
     cause the body of [g] to be resimplified in the context where [x] is
     known to be equal to the argument. *)
  let[@local never][@inline never] g y = x + y in
  g x

let f2 x =
  (* However, the closure for [g] here should be unboxed even by default,
     because [g] is inlined into [h]. *)
  let[@local never][@inline always] g y = x + y in
  let[@local never][@inline never] h y = g y in
  h x
