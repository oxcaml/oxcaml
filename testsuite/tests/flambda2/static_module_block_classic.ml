(* TEST
 flambda2;
 no-frame_pointers;
 compile_only = "true";
 flags = "-Oclassic -dcmm -dno-locations -dno-unique-ids";
 setup-ocamlopt.byte-build-env;
 unset OCAMLPARAM;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

(* In Classic mode the fields of the module block are the parameters of the
   return continuation, so they reach To_cmm as variables. Those bound to
   constants (immediates, closures, lifted constant blocks, aliases of them)
   should be emitted directly in the module block's data rather than being
   filled in by [caml_initialize] in the module initialiser. Only [dyn] below
   should still need a runtime initialisation. *)

type t =
  | A
  | B

let a = A
let b = B
let is_a = function
  | A -> true
  | B -> false

type r =
  { name : string;
    rank : int;
    ctor : t
  }

let ra = { name = "A"; rank = 0; ctor = a }
let alias = is_a
let dyn = Sys.opaque_identity 42
