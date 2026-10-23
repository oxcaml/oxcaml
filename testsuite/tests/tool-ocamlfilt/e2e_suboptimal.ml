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

(* {1 Functor instances are indistinguishable}

   The two applications below produce two separate compiled copies of
   [run], but both mangle identically to [Make.run]: the application
   site ([Int_inst] / [Str_inst]) never appears in the name, so the
   copies cannot be told apart. (The functor's own body is [Make].) *)
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

(* {1 Classes and objects}

   OxCaml emits no DWARF for the OOP fragment of OCaml, but the linker
   names now carry the class: methods are [<class>.method_<name>] and the
   closures the class compiler generates are marked,
   [<class>.{class_init}] and [<class>.{obj_init}]. What remains lossy:
   - Instance variables ([val nick], [val count]) and their accessors
     produce no symbol of their own.
   - A [virtual] class yields no constructor symbol, and [inherit]
     leaves no trace in any name.
   - An immediate object ([object ... end] with no class, in
     [make_logger]) is a class named [object]:
     [make_logger.object.method_log]. *)
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
  ignore (Int_inst.run 1 2);
  ignore (Str_inst.run "a" "b");
  ignore (scale 1);
  let d = new dog "rex" in
  ignore d#sound;
  ignore d#describe;
  ignore (d#scale 2);
  d#rename "fido";
  let l = make_logger () in
  l#log;
  ignore l#total
