(* TEST
   flambda2;
   flags += "-flambda2-reaper";
   { native; }
 *)

(* This test is concerned about the case where an argument of an
   apply is a dead variable (no sources), and is as such not
   unboxed, while the corresponding parameter of the function is
   unboxed. *)

let[@inline never][@local never] nope () = assert false

(* [id] is here to prevent simplify from noticing this function
   never returns. From the point of view of the reaper, this
   function returns a value, but projecting the first field of
   this value will always result in a variable without sources. *)
let[@inline never][@local never] nope2 () =
  let[@inline never][@local never] id g = g in
  (id nope (), 0)

let go x =
  (* Here, [f] will have its argument unboxed: there is only a
     single matching allocation, [(x, x)] below, as [u] has no
     sources. We are in an inconsistent context when rewriting
     the call to [f u ()], and must take care not to crash, as
     [u] is not an unboxed variable. *)
  let[@inline never][@local never] f (a, b) () = a + b in
  let (u, _) = nope2 () in
  f u () + f (x, x) ()