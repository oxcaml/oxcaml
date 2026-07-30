(* TEST
 reference =
   "${test_source_directory}/unboxed_record_any_field_inference.reference";
 flambda2;
 {
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -Oclassic";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

(* Regression test: a function parameter whose unboxed-record type has an
   [('a : any)] field, where ['a] is only discovered to be a nested unboxed
   product by inference from the function body (via a [_] type annotation).
   The sorts stored in the pattern used to be defaulted prematurely, making
   the parameter's unarized layout disagree with the field projections and
   crashing the middle end. *)

type ('a : any) with_id =
  #{ id : int
   ; value : 'a
   }

type t =
  #{ foo : int
   ; bar : string
   }

(* Wildcard annotation: the payload's sort is discovered from the body. *)
let f (#{ id; value = t } : _ with_id) = id, t.#foo, t.#bar

(* Control: concrete annotation. *)
let f' (#{ id; value = t } : t with_id) = id, t.#foo, t.#bar

(* Two [any] fields, both inferred. *)
type ('a : any, 'b : any) pair2 =
  #{ fst : 'a
   ; snd : 'b
   }

let g (#{ fst; snd } : (_, _) pair2) = fst.#bar, snd + 1

(* Curried: the payload is only resolved in an inner body. *)
let h (#{ id = _; value = v } : _ with_id) () = v.#foo

(* Inlining at closure conversion (classic mode) used to crash when the
   callee's unarized params disagreed with the apply's unarized args. *)
let[@inline] proj (#{ id; value = t } : _ with_id) =
  ignore (id : int);
  t.#foo

let apply_proj x = (proj [@inlined]) x

let () =
  let arg = #{ id = 3; value = #{ foo = 42; bar = "hello" } } in
  let id, foo, bar = f arg in
  Printf.printf "f: %d %d %s\n" id foo bar;
  let id, foo, bar = f' arg in
  Printf.printf "f': %d %d %s\n" id foo bar;
  let bar, snd = g #{ fst = #{ foo = 1; bar = "x" }; snd = 10 } in
  Printf.printf "g: %s %d\n" bar snd;
  Printf.printf "h: %d\n" (h arg ());
  Printf.printf "apply_proj: %d\n"
    (apply_proj #{ id = 7; value = #{ foo = 9; bar = "y" } })
