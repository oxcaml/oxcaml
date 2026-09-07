(* TEST
   modules = "specialise_lifted_functor_helper_lib.ml";
   flambda2;
   flags += "-O4 -flambda2-reaper -reaper-local-fields";
   { native; }
 *)

module M = Specialise_lifted_functor_helper_lib.Make (struct
  let f x = x * x
end)

(* [M.apply_twice] must call a copy of [helper] specialised on [x * x]; the
   generic helper's unknown callback fails [@zero_alloc]. *)
let[@zero_alloc] fourth_power x = M.apply_twice x

let () = Printf.printf "%d\n" (fourth_power 3)
