(* TEST
 flambda2;
 native;
*)

(* Runtime representation of mixed float/float# records.

   Without [@@flatten_floats], the [float] field is stored as a pointer to a
   separately-allocated double block; with [@@flatten_floats], the [float] is
   stored inline in the record's flat suffix. Bytecode does not implement
   flattening, so this test is native-only. *)

type t_non_flat = { f : float; u : float# }
type t_flat     = { f : float; u : float# } [@@flatten_floats]

let make_non_flat x y : t_non_flat = { f = x; u = y }
let make_flat     x y : t_flat     = { f = x; u = y }

let () =
  let n = make_non_flat 1. #2. in
  let m = make_flat 1. #2. in
  let n_words = Obj.reachable_words (Obj.repr n) in
  let m_words = Obj.reachable_words (Obj.repr m) in
  Printf.printf "non-flat reaches %d words, flat reaches %d words\n"
    n_words m_words;
  (* The non-flat record's [f] is a separately-allocated boxed float, so
     [n] reaches strictly more words than [m]. *)
  assert (n_words > m_words);
  (* The first field of the non-flat record is a Double-tagged block. *)
  assert (Obj.tag (Obj.field (Obj.repr n) 0) = Obj.double_tag)
