(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-name-mangling-scheme structured -c";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 output = "e2e_anonymous.table";
 script = "sh ${test_source_directory}/e2e_table.sh \
           ${test_build_directory}/e2e_anonymous.o \
           '(caml|U[0-9]+)(Stdlib|Camlinternal)'";
 script;
 reference = "${test_source_directory}/e2e_anonymous.reference";
 check-program-output;
*)

(* Cases where the structured name-mangling scheme produces clean, precise
   demanglings for anonymous functions and modules. An anonymous entry is
   identified by its own [(file:line:col)], so the enclosing scopes up to
   the compilation unit are redundant and dropped, leaving a short name
   that still pins down the source location. See the STRUCTURED column of
   [e2e_anonymous.reference] for the demangled names. *)

(* {1 Nested anonymous functions each collapse to their own location}

   Each [fun] is [@inline never] so every closure survives as its own
   symbol. Rather than chaining every enclosing anonymous location (and the
   [nested_lambdas] prefix), each closure mangles to a bare
   [E2e_anonymous.fn(<file:line:col>)] that identifies it uniquely --
   including the deepest one, which keeps its own location. *)
let nested_lambdas a =
  fun[@inline never] b ->
    fun[@inline never] c ->
      fun[@inline never] d -> a + b + c + d

(* {1 Anonymous first-class modules are told apart by their location}

   The two distinct [(module struct ... end)] bodies are distinguished by
   their anonymous-module location ([mod(<loc>).v]); the enclosing [pick]
   is dropped, as that location already identifies the body uniquely. *)
module type V = sig
  val v : int -> int
end

let pick flag : (module V) =
  if flag
  then (module struct
    let[@inline never] v x = x + 1
  end)
  else
    (module struct
      let[@inline never] v x = x - 1
    end)

(* {1 Lazy thunks carry a [lazy] path item with their location}

   The body of a [lazy (...)] is lifted to its own closure. It records a
   [lazy(<file:line:col>)] path item, so it mangles to
   [E2e_anonymous.lazy(<loc>).fn] -- located, marked as a lazy thunk, and
   with the enclosing [deferred] dropped since the location already
   identifies it. *)
let deferred () =
  let t = lazy (Sys.opaque_identity 41 + 1) in
  Lazy.force t

let () =
  ignore (nested_lambdas 1 2 3 4);
  ignore (pick true);
  ignore (deferred ())
