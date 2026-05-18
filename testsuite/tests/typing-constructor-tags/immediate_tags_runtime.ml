(* TEST_BELOW
*)

type t =
  | A [@immediate 2]
  | B
  | C [@immediate 1]

type mixed =
  | I0 [@immediate 1]
  | Block of int
  | I1

let int_of_t (x : t) : int = Obj.magic x

let int_of_mixed (x : mixed) : int = Obj.magic x

let print_t x =
  Printf.printf "%s:%d\n"
    (match x with A -> "A" | B -> "B" | C -> "C")
    (int_of_t x)

let print_mixed x =
  match x with
  | I0 -> Printf.printf "I0:%d\n" (int_of_mixed x)
  | I1 -> Printf.printf "I1:%d\n" (int_of_mixed x)
  | Block n -> Printf.printf "Block %d\n" n

let classify_t_default x =
  match x with
  | A -> "A"
  | _ -> "default"

let classify_mixed_default x =
  match x with
  | I0 -> "I0"
  | _ -> "mixed default"

let () =
  List.iter print_t [ A; B; C ];
  List.iter print_mixed [ I0; I1 ];
  print_mixed (Block 42);
  List.iter
    (fun x -> Printf.printf "%s\n" (classify_t_default x))
    [ A; B; C ];
  List.iter
    (fun x -> Printf.printf "%s\n" (classify_mixed_default x))
    [ I0; I1; Block 42 ]

(* TEST
 reference = "${test_source_directory}/immediate_tags_runtime.reference";
 {
   bytecode;
 }{
   native;
 }
*)
