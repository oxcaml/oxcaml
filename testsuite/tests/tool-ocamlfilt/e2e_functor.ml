(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-name-mangling-scheme structured -c";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 output = "e2e_functor.table";
 script = "sh ${test_source_directory}/e2e_table.sh \
           ${test_build_directory}/e2e_functor.o \
           '(caml|U[0-9]+)(Stdlib|Camlinternal)'";
 script;
 reference = "${test_source_directory}/e2e_functor.reference";
 check-program-output;
*)

(* Functor instances are told apart by their specialisation context.

   Applying [Make] twice produces two compiled copies of [run]. They share
   the same source (the body of [Make]), so on their own they would mangle
   identically. The structured scheme records the specialisation context --
   the module the copy was produced for -- as an [Inline_marker] prefix
   (demangled [<specialization_of>]):

   - [E2e_functor.Make.run]                                (the template)
   - [E2e_functor.Int_inst.<specialization_of>.Make.run]  (specialised for [Int_inst])
   - [E2e_functor.Str_inst.<specialization_of>.Make.run]  (specialised for [Str_inst])

   The context comes from the [Module] scope the frontend records around the
   application in [module Foo = Make (...)], folded into the specialised
   code's debuginfo when the simplifier copies it. *)
module type ORD = sig
  type t

  val cmp : t -> t -> int
end

module Make (O : ORD) = struct
  let[@inline never] run a b = O.cmp a b + O.cmp b a
end

module Int_inst = Make (struct
  type t = int

  let cmp = compare
end)

module Str_inst = Make (struct
  type t = string

  let cmp = compare
end)

let () =
  ignore (Int_inst.run 1 2);
  ignore (Str_inst.run "a" "b")
