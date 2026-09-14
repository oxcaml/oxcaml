(* TEST
 flambda2;
 flags += "-Oclassic -flambda2-reaper -X reaper-oclassic=1 -reaper-local-fields -reaper-debug-flags=nostamps";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte with dump-raw, dump-reaper;
 check-fexpr-dump;
*)

(* A closure that is not needed in a given function can still be unboxed if
   another closure of its set of closures is unboxed. In case the callee is
   erased, we still must not crash. *)

external dummy : unit -> 'a = "%opaque"

let bind : (unit -> int) -> unit = dummy ()

let rec f () = bind (fun () -> g () ())

and g y = fun z -> 0

let (_ : unit -> int) = g ()

let _ = f ()
