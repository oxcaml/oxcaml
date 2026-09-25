(* TEST
 flags = "-function-sections -internal-assembler";
 function_sections;
 arch_amd64;
 native;
*)

(* With -function-sections, jump tables are emitted in their function's own
   section, so the internal assembler must resolve their entries itself. *)

(* The arms must not all be constants, or the switch becomes a table lookup
   rather than a jump table. *)

let g1 s = s ^ "*"
let g2 s = "*" ^ s
let g3 s = "*" ^ s ^ "*"

let f = function
  | 1 -> g1 "a"
  | 2 -> g2 "b"
  | 3 -> g3 "c"
  | 4 -> g1 "d"
  | 5 -> g2 "e"
  | 6 -> g3 "f"
  | _ -> "x"
[@@inline never]

let g x =
  match x with
  | 10 -> x + 1
  | 11 -> x * 3
  | 12 -> x - 5
  | 13 -> x lsl 2
  | 14 -> x lxor 7
  | 15 -> -x
  | 16 -> x / 3
  | _ -> 0
[@@inline never]

let () =
  assert (List.map f [0; 1; 2; 3; 4; 5; 6; 7]
          = ["x"; "a*"; "*b"; "*c*"; "d*"; "*e"; "*f*"; "x"]);
  assert (List.map g [9; 10; 11; 12; 13; 14; 15; 16; 17]
          = [0; 11; 33; 7; 52; 9; -15; 5; 0])
