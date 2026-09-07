(* The callbacks are still unknown when these functors are compiled.  The
   specialisation sites for their lifted functions must survive this
   intermediate unit. *)

module[@inline] Helper (X : Specialise_lifted_functor_helper_lib.S) =
  Specialise_lifted_functor_helper_lib.Make (X)

module[@inline] Body (X : Specialise_lifted_functor_body_lib.S) =
  Specialise_lifted_functor_body_lib.Make (X)

(* The fresh module block is unboxed here. Its live callback must retain the
   specialised parameter annotation after this second reaper pass. *)
module[@inline] Wrapped (X : Specialise_lifted_functor_helper_lib.S) = struct
  let[@inline] apply_twice x =
    let module M = Specialise_lifted_functor_helper_lib.Make
      (struct let f = X.f end)
    in
    M.apply_twice x
end
