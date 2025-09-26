(* TEST
 flags = "-stop-after parsing -dparsetree";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

module type S = sig
  (* value descriptions *)
  val v : int mod contended aliased
  val v : int @@ once portable mod contended aliased
  (* currently, these parse in the same way, which is not desired *)
  val v : int -> int mod contended aliased
  val v : (int -> int) mod contended aliased
  val v : int -> int @@ once portable mod contended aliased

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
  ; f5 : 'a -> 'a @@ once portable mod contended aliased
  }

  (* currently, mod modes are properly parsing on arrow types:
     the following examples will not parse, but they should eventually

  val v : int -> int mod contended aliased @@ once portable

  type t = int mod contended aliased -> int

  type 'a r = {
    f1 : 'a mod contended aliased -> 'a
  ; f2 : 'a @ once portable mod contended aliased -> 'a
  ; f3 : 'a -> 'a mod contended aliased @@ once portable
  }
  *)

  (* the other places where modalities appear all have to deal with modules, which is
     not currently intended to be supported and does not parse currently *)
end

module M = struct
  (* value bindings *)
  (* CR zeisbach: we intend no support for using legacy syntax for modes with the new mod
     modalities syntax, but maybe some of it will accidentally work. Test this out *)

  let (v mod contended aliased) = 42
  let (v @ once portable mod contended aliased) = 42

  let v mod contended aliased = 42
  let (v mod contended) mod aliased = 42

  let v : int mod contended aliased = 42
  let v : (int -> int) mod contended aliased = fun _ -> 42

  let v @ once portable mod contended aliased = 42
  let v : int @ once portable mod contended aliased = 42
  let v : (int -> int) @ once portable mod contended aliased = fun _ -> 42

  let f (x mod contended aliased) = 42
  let f (x @ once portable mod contended aliased) = 42

  (* these modalities are attached to the [let] itself, not to the pattern. *)
  let (x, y) mod contended aliased = 4, 2
  let (x, y) : (int * int) mod contended aliased = 4, 2
  let (x, y) : (int * int) @ once portable mod contended aliased = 4, 2

  let foo : type a. a @ once portable mod contended aliased = fun x -> x
  let foo : type a. (a -> a) @ once portable mod contended aliased = fun x -> x

  (* the following syntax is plausible, but currently serves no purpose, since functions
     cross all monadic axes currently anyways. this may just be a coincedence, so these
     examples are included to illustrate possibilities: *)

  let (f mod contended aliased) a b = 42
  let (f @ once portable mod contended aliased) a b = 42

  (* expressions *)
  let foo = (42 : int mod contended aliased) + (42 : int @ once portable mod contended)
  let foo = (42 : _ mod contended aliased) + (42 : _ @ once portable mod contended)

  (* like above, there are examples with mod modes on arrow types which do not work.
     here are a few examples:

  let v : int -> int mod contended aliased = fun _ -> 42
  let v : int -> int @ once portable mod contended aliased = fun _ -> 42
  let v : int mod contended aliased -> int = fun _ -> 42
  *)
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

(* TODO (ZJE): ALSO WORTH TESTING:
    - make sure that comments work; specifically, that doc comments are attached to the
      right thing after parsing
    - make sure that attributes work too
    - might want to add to the source_jane_street.ml test file, which also has more *)

(* in addition to the mod modes on arrow types, there are some more odd consequences
   of this choice of syntax, which are noted here for examination:

  mod modalities interaction with with-bounds: parses as mod jkind_annotation

  type 'a t : immutable_data with 'a mod contended
  type 'a t : immutable_data with 'a -> 'a mod contended

  kind_of_ similarly has weird precedence-related cases.

  type 'a t : kind_of_ 'a mod global
  (* these two cases parse identically *)
  type 'a t : kind_of_ 'a -> 'a mod global
  type 'a t : kind_of_ ('a -> 'a) mod global
*)
