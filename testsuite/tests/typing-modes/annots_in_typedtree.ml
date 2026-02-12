(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

(* This file tests that mode/modality annotations are properly propogated from
   the parsetree into the typedtree. It does so by typing a bit of code that
   contains an `@ portable`/`@@ portable` annotation, then iterating over the
   typedtree and asserting that `@ portable`/`@@ portable` appears somewhere in
   it. *)

(********** Setup **********)

let run s =
  (* Parse and typecheck the string as an impl. *)
  let pi = Parse.implementation (Lexing.from_string s) in
  let pm = Ast_helper.Mod.structure pi in
  let tm, _ = Typemod.type_module (Lazy.force Env.initial) pm in
  (* Check if the typedtree has a "@ portable" annotation anywhere in it. *)
  let has_portable_annotation = ref false in
  let modes iterator (modes : _ Typedtree.modes) =
    List.iter
      (fun (mode : Mode.Alloc.atom Location.loc) ->
        match mode.txt with
        | Atom (Comonadic Portability, Portable) ->
          has_portable_annotation := true
        | _ -> ())
      modes.mode_desc;
    Tast_iterator.default_iterator.modes iterator modes
  in
  let modalities iterator (modalities : Typedtree.modalities) =
    List.iter
      (fun (mode : Mode.Modality.atom Location.loc) ->
        match mode.txt with
        | Atom (Comonadic Portability, Meet_with Portable) ->
          has_portable_annotation := true
        | _ -> ())
      modalities.moda_desc;
    Tast_iterator.default_iterator.modalities iterator modalities
  in
  let iterator = { Tast_iterator.default_iterator with modes; modalities } in
  iterator.module_expr iterator tm;
  if !has_portable_annotation
  then Format.printf "Has annotation@."
  else failwith "Error: annotation missing"
;;

[%%expect{|
val run : string -> unit = <fun>
|}];;

(* Validate testing strategy but checking that we get an error message if no
   modes/modalities appear *)
run {| type t |};;

[%%expect{|
Exception: Failure "Error: annotation missing".
|}];;

(********** Tests **********)

run {| let f : 'a. ('a @ portable -> 'a) = fun x -> x |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let f : 'a. ('a -> 'a @ portable) = assert false |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let f : 'a. ('a -> 'a) @ portable = fun x -> x |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| type t = int @ portable -> int |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| type t = int -> (int -> int) @ portable |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let x : int @ portable = 0 |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| module type S = sig @@ portable end |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| module type S = sig include sig end @@ portable end |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let foo = (10 : int @ portable) |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| type t = { foo : int -> int @@ portable } |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| type t = Foo of (int -> int) @@ portable |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| type t = Foo of { x : int -> int @@ portable } |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| external id : 'a -> 'a @@ portable = "%identity" |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| module type S = sig val foo : int -> int @@ portable end |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let f a b : _ @ portable = () |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let f (a @ portable) b = () |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let f a (b @ portable) = () |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let foo x y @ portable = ()|};;
[%%expect{|
Has annotation
- : unit = ()
|}];;

run {| let foo = fun x y @ portable -> () |};;
[%%expect{|
Has annotation
- : unit = ()
|}];;
