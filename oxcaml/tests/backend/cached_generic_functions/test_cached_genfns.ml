(* Links with [-use-cached-generic-functions]: concrete-result generic
   functions (e.g. [caml_apply2]) come from the precompiled cache archive,
   while unknown-result variants (e.g. [caml_apply2_Runknown],
   [caml_curry2_Runknown]) are not cached and must be emitted into the
   startup object at link time. *)

let[@inline never] get_int () = Sys.opaque_identity 42

let[@inline never] forward_pair : type (a : any). (int -> int -> a) -> a =
 fun f -> f 1 2

let[@inline never] forwarder2 : type (a : any). (unit -> a) -> unit -> a =
 fun g () -> g ()

let[@inline never] concrete_apply2 f x y = f x y

let () =
  let add = Sys.opaque_identity ( + ) in
  Printf.printf "forward_pair: %d\n" (forward_pair add);
  let partial = (Sys.opaque_identity forwarder2) get_int in
  Printf.printf "forwarder2: %d\n" (partial ());
  Printf.printf "concrete_apply2: %d\n" (concrete_apply2 add 20 22)
