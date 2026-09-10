(* TEST
 compile_only = "true";
 flambda2;
 no-tsan;
 no-frame_pointers;
 setup-ocamlopt.opt-build-env;
 ocamlopt_flags = "-dcmm -dno-locations -dno-unique-ids";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* A first-class use of a never-returning builtin declared with a
   layout-[any] result eta-expands in translprim to a wrapper that itself
   never returns. The wrapper's return layout comes from the declared
   [Repr_never_returns] repr and is bottom: its Cmm function declares the
   unit (void machtype) return type instead of val. *)

exception E

external raise_any : ('a : any). exn -> 'a = "%raise"

let first_class = raise_any

let use (b : bool) : int = if b then 1 else first_class E
