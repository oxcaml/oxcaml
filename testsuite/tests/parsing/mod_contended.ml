(* TEST
 flags = "-stop-after parsing -dparsetree";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(*****************)
(* as a modality *)

module type S = sig
  (* value descriptions *)
  val v : int mod contended aliased
  val v : int @@ once portable mod contended aliased
  val v : int -> int mod contended aliased
  val v : (int -> int) mod contended aliased
  val v : int -> int @@ once portable mod contended aliased
  val v : (int -> int) @@ once portable mod contended aliased

  (* primitive declarations *)
  external ex : int mod contended aliased = "foo1"
  external ex : int @@ once portable mod contended aliased = "foo2"
  external ex : int -> int mod contended aliased = "foo3"
  external ex : int -> int @@ once portable mod contended aliased = "foo4"

  (* TODO (ZJE): maybe just add in tests with types? *)

  type t =
    | T1 of int mod contended aliased
    | T2 of int @@ once portable mod contended aliased
    | T3 of (int -> int) mod contended aliased
    (* [T3 of int -> int mod contended aliased] won't parse, for the same reason why
       [T3 of int -> int @ once portable] doesn't *)
    | T4 of (int -> int) @@ once portable mod contended aliased
    | T5 of int @@ once portable mod contended aliased * int mod contended aliased
  (* these examples are non-exhaustive; see the OxCaml docs -> modes -> syntax ->
     "constructor field" examples *)

  type 'a r = {
    f1 : 'a mod contended aliased
  ; f2 : 'a @@ once portable mod contended aliased
  ; f3 : 'a -> 'a mod contended aliased
  ; f4 : ('a -> 'a) mod contended aliased
  (* ; f5 : 'a mod contended aliased -> 'a *)
  (* ; f6 : 'a @ once portable mod contended aliased -> 'a *)
  ; f7 : 'a -> 'a @@ once portable mod contended aliased
  (* ; f8 : ('a -> 'a mod contended aliased) @@ once portable *)
  }

  (* the other places where modalities appear all have to deal with modules, which is
     not currently intended to be supported and does not parse currently *)
end

(*************)
(* as a mode *)

(* Q (ZJE): does it matter these examples are in a structure (aka [structure_item]s) *)
module M = struct
  (* value bindings *)
  (* no support for using legacy syntax for modes with the new mod modalities syntax *)

  (* TODO (ZJE): maybe add more of these tests? Definitely rearrange some, at least *)
  let (v mod contended aliased) = 42
  let (v @ once portable mod contended aliased) = 42

  let v mod contended aliased = 42
  let (v mod contended) mod aliased = 42

  let v : int mod contended aliased = 42
  let v : (int -> int) mod contended aliased = fun _ -> 42

  let v @ once portable mod contended aliased = 42
  let v : int @ once portable mod contended aliased = 42
  let v : (int -> int) @ once portable mod contended aliased = fun _ -> 42

  (* these modalities are attached to the [let] itself, not to the pattern.
     such modalities on arbitrary patterns will not be supported, at least initially *)
  let (x, y) mod contended aliased = 4, 2
  let (x, y) : (int * int) mod contended aliased = 4, 2
  let (x, y) : (int * int) @ once portable mod contended aliased = 4, 2

  let foo : type a. a @ once portable mod contended aliased = fun x -> x
  let foo : type a. (a -> a) @ once portable mod contended aliased = fun x -> x

  (* source_jane_street has more examples with (kind-constrained) polymorphism
     and also locally abstract type stuff too, which is omitted here currently *)

  (* the following syntax is plausible, but currently serves no purpose, since functions
     cross all monadic axes currently anyways. this may just be a coincedence, so these
     examples are included to illustrate possibilities: *)

  let (f mod contended aliased) a b = 42
  let (f @ once portable mod contended aliased) a b = 42

  (* expressions *)
  let foo = (42 : int mod contended aliased) + (42 : int @ once portable mod contended)
  let foo = (42 : _ mod contended aliased) + (42 : _ @ once portable mod contended)
end

(* let expressions (not just as structure items) *)
let f () =
  let v mod contended aliased = 42 in
  let v : int mod contended aliased = 42 in
  let v : (int -> int) mod contended aliased = fun _ -> 42 in

  let v @ once portable mod contended aliased = 42
  and v : int @ once portable mod contended aliased = 42
  and v : (int -> int) @ once portable mod contended aliased = fun _ -> 42 in

  ()

(* ALSO WORTH TESTING:
    - make sure that comments work; specifically, that doc comments are attached to the
      right thing after parsing
    - make sure that attributes work too
   Of course, more tests will reveal themselves during implementation *)

(* TODO more tests: examples with with_kinds, examples with kind_of_ *)
