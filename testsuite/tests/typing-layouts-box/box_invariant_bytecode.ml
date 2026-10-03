(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* Internal ticket 7068: Bytecode currently breaks the boxing invariant. Both
   [t#] and [s#] have layout [value & value], but the boxed versions [t] and [s]
   are laid out differently. [box] is layout-directed, so one of these
   necessarily gets miscompiled. We will have to change the bytecode
   representation to adjust this. *)

type t = { i : #(int * int) }
type s = { i : int; j : int }

(* CR box: this test should pass. *)
let () =
  let t : t = { i = #(1, 2) } in
  let s : s = { i = 1; j = 2 } in
  assert (Obj.size (Obj.repr t) == Obj.size (Obj.repr s))
[%%expect{|
type t = { i : #(int * int); }
type s = { i : int; j : int; }
Exception: Assert_failure ("", 8, 2).
|}]
