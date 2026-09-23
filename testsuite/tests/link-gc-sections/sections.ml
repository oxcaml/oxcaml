(* TEST
 flags = "-S -function-sections";
 native-compiler;
 function_sections;
 link_order_frametables;
 native;
*)

(* Check the shape of the assembly emitted for link-order frametables: one
   caml_frametable piece per function (all of them link-order, "ao"), the
   unit's begin/end markers, per-function trap notes, per-symbol data
   sections, and no per-unit jump table section. *)

type t =
  | A | B | C | D | E | F | G | H | I | J | K | L

(* A switch on many constructors, to force a jump table. *)
let[@inline never] describe = function
  | A -> "a" ^ "!"
  | B -> "b" ^ "!"
  | C -> "c" ^ "!"
  | D -> "d" ^ "!"
  | E -> "e" ^ "!"
  | F -> "f" ^ "!"
  | G -> "g" ^ "!"
  | H -> "h" ^ "!"
  | I -> "i" ^ "!"
  | J -> "j" ^ "!"
  | K -> "k" ^ "!"
  | L -> "l" ^ "!"

exception Boom of int

let[@inline never] may_raise n =
  if n > 2 then raise (Boom n) else n * 2

(* A try-with, to force a trap note. *)
let[@inline never] guarded n =
  try may_raise n with Boom m -> m + 100

let () =
  let all = [A; B; C; D; E; F; G; H; I; J; K; L] in
  List.iter (fun x -> print_string (describe x)) all;
  print_newline ();
  Printf.printf "%d %d\n" (guarded 1) (guarded 5)
