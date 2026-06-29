(* TEST
 native-compiler;
 {
   compiler_directory_suffix = ".plain";
   setup-ocamlopt.byte-build-env;
   flags = "-name-mangling-scheme structured -extension comprehensions -c";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   output = "e2e_oxcaml.plain.table";
   script = "sh ${test_source_directory}/e2e_table.sh \
             ${test_build_directory}/e2e_oxcaml.o \
             'camlStdlib|camlCamlinternal'";
   script;
   reference = "${test_source_directory}/e2e_oxcaml.plain.reference";
   check-program-output;
 }{
   (* Rebuild with -for-pack. Exercises the path that embeds the pack
      prefix (joined by [__]) inside the U payload of the structured
      mangling. *)
   compiler_directory_suffix = ".packed";
   setup-ocamlopt.byte-build-env;
   flags = "-name-mangling-scheme structured -extension comprehensions \
            -for-pack Ox_pack -c";
   ocamlopt_byte_exit_status = "0";
   ocamlopt.byte;
   check-ocamlopt.byte-output;
   output = "e2e_oxcaml.packed.table";
   script = "sh ${test_source_directory}/e2e_table.sh \
             ${test_build_directory}/e2e_oxcaml.o \
             'camlStdlib|camlCamlinternal'";
   script;
   reference = "${test_source_directory}/e2e_oxcaml.packed.reference";
   check-program-output;
 }
*)

(* Comprehensive exercise of OxCaml-specific constructs. The goal is
   that every distinct OxCaml feature contributes at least one symbol
   that gets through the structured demangler successfully. Features
   here mirror the umbrella areas advertised on oxcaml.org: stack
   allocation/locality, modes (locality, portability, uniqueness),
   unboxed types, kinds, polymorphic parameters, labeled tuples,
   immutable arrays, [let mutable], [include functor], list/array
   comprehensions, and functors that interact with mode annotations. *)

(* {1 Stack allocation and local-mode annotations} *)

let[@inline never] consume (_ : _ @ local) = ()

let stack_alloc_pair x =
  let local_ pair = (x, x + 1) in
  consume pair;
  ()

let use_local (local_ s) = String.length s

type box = { mutable v : int }

let mutate_local () =
  let local_ b = { v = 0 } in
  b.v <- 42;
  b.v

(* {1 Non-locality mode annotations (portable, unique)} *)

let portable_id (x @ portable) = x

let consume_unique (x @ unique) = ignore x

(* {1 Unboxed scalars (layouts beyond [value])} *)

let pi_u : float# = #3.14
let twice_u (x : float#) : float# = x

let zero64 : int64# = #0L
let pass_int64u (x : int64#) : int64# = x

(* {1 Polymorphic parameter (rank-2 type)} *)

let apply_poly (f : 'a. 'a -> 'a) = f 1, f "x"

(* {1 Labeled tuples} *)

let lt_point = ~x:1, ~y:2
let lt_sum (~x, ~y) = x + y

type lt_named = x:int * y:int

let make_lt x y : lt_named = ~x, ~y

(* {1 Immutable arrays} *)

let iarr : int iarray = [: 10; 20; 30 :]

let iarr_first = function
  | [: x; _; _ :] -> x
  | _ -> 0

(* {1 [let mutable] inside a function body} *)

let triangle n =
  let mutable total = 0 in
  for i = 1 to n do total <- total + i done;
  total

(* {1 Kind-annotated type alias} *)

type id : immediate = int
let an_id : id = 99
let id_succ (x : id) : id = x + 1

(* {1 Include functor (functor application via [include functor])} *)

module type Adder_sig = sig
  val base : int
end

module Bump (X : Adder_sig) = struct
  let bumped = X.base + 1
  let bumped_twice = X.base + 2
end

module Seven = struct
  let base = 7
  include functor Bump
end

(* {1 Functors interacting with mode-annotated arguments} *)

module type Adder = sig
  val add : int -> int -> int
end

module Make_adder (M : sig val base : int end) = struct
  let add x y = M.base + x + y

  let scale_local (local_ x) (local_ y) = add x y
end

module Five = struct let base = 5 end
module Five_adder = Make_adder (Five)

module Pipeline (M : Adder) = struct
  let single_local (local_ x) = M.add x 1

  let nested xs =
    List.iter
      (fun x -> List.iter (fun y -> ignore (M.add x y)) xs)
      xs
end

module P = Pipeline (Five_adder)

(* {1 Comprehensions} *)

let evens = [ x for x = 1 to 10 when x mod 2 = 0 ]
let pairs = [ (i, j) for i = 1 to 3 for j in evens ]

(* {1 Force evaluation of every binding} *)

let () =
  stack_alloc_pair 1;
  ignore (use_local "hi");
  ignore (mutate_local ());
  ignore (portable_id 42);
  consume_unique 1;
  let _ : float# = twice_u pi_u in
  let _ : int64# = pass_int64u zero64 in
  ignore (apply_poly (fun x -> x));
  ignore (lt_sum lt_point);
  ignore (make_lt 1 2);
  ignore (iarr_first iarr);
  ignore (triangle 10);
  ignore (id_succ an_id);
  ignore (Seven.bumped + Seven.bumped_twice);
  ignore (Five_adder.add 1 2);
  let module Five_adder' = Make_adder (Five) in
  ignore (Five_adder'.scale_local 1 2);
  ignore (P.single_local 5);
  P.nested [ 1; 2; 3 ];
  ignore (List.length evens);
  ignore (List.length pairs)
