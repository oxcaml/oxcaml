(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-name-mangling-scheme structured -c";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 output = "e2e_suboptimal.table";
 script = "sh ${test_source_directory}/e2e_table.sh \
           ${test_build_directory}/e2e_suboptimal.o \
           '(caml|U[0-9]+)(Stdlib|Camlinternal)'";
 script;
 reference = "${test_source_directory}/e2e_suboptimal.reference";
 check-program-output;
*)

(* Cases where the structured name-mangling scheme currently produces
   confusing, lossy, or missing demanglings. This test is a deliberate
   record of that suboptimal output, not an endorsement of it: the
   reference is expected to improve as the scheme is refined. See the
   STRUCTURED column of [e2e_suboptimal.reference] for the demangled
   names. *)

(* {1 Nested anonymous functions collapse to a single location}

   Each [fun] is [@inline never] so every closure survives as its own
   symbol. A single [fn(file:line:col)] identifies a closure uniquely, so
   the enclosing scopes (the outer anonymous functions and [nested_lambdas]
   itself) are dropped: the deepest closure now mangles to a bare
   [E2e_suboptimal.fn(..:35:6)] rather than chaining every enclosing
   location. *)
let nested_lambdas a =
  fun[@inline never] b ->
    fun[@inline never] c ->
      fun[@inline never] d -> a + b + c + d

(* {1 Anonymous first-class modules are told apart by their location}

   The two distinct [(module struct ... end)] bodies are distinguished by
   their anonymous-module location ([mod(..77:15).v] vs [mod(..81:12).v]);
   the enclosing [pick] is dropped, as the location already identifies it. *)
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

(* {1 Lazy thunks have no mangling path item}

   The body of a [lazy (...)] is lifted to its own closure but the
   scheme records no [lazy] path item for it, so it mangles to a bare
   top-level [fn] with no location: indistinguishable from any other
   anonymous function and giving no hint that it is a lazy thunk or
   where it came from. *)
let deferred () =
  let t = lazy (Sys.opaque_identity 41 + 1) in
  Lazy.force t

(* {1 Classes and objects get poor symbols across the board}

   OxCaml emits no DWARF for the OOP fragment of OCaml and the linker
   names are correspondingly weak. Recording them here documents what
   future object support (Shape.t object variants, then DWARF
   [DW_TAG_class_type] / [DW_TAG_subprogram] / [DW_TAG_inheritance])
   would aim to fix.

   - Method bodies lose their name: every method below mangles to an
     anonymous [fn] -- mostly a bare top-level [E2e_suboptimal.fn],
     rarely as [<class>.fn] -- so methods are indistinguishable from
     each other, from the unrelated top-level [scale], and from
     ordinary anonymous functions.
   - Instance variables ([val nick], [val count]) and their accessors
     produce no symbol of their own.
   - A [virtual] class yields no constructor symbol, and [inherit]
     leaves no trace in any name.
   - An immediate object ([object ... end] with no class, in
     [make_logger]) loses even the name of the binding it is attached
     to. *)
let scale k = k + 1

class virtual animal = object
  method virtual sound : string
  method describe = "an animal"
end

class dog (name : string) = object
  inherit animal
  val mutable nick = name
  method sound = "woof"
  method scale k = scale k
  method rename n = nick <- n
end

let make_logger () = object
  val mutable count = 0
  method log = count <- count + 1
  method total = count
end

let () =
  ignore (nested_lambdas 1 2 3 4);
  ignore (pick true);
  ignore (deferred ());
  ignore (scale 1);
  let d = new dog "rex" in
  ignore d#sound;
  ignore d#describe;
  ignore (d#scale 2);
  d#rename "fido";
  let l = make_logger () in
  l#log;
  ignore l#total
