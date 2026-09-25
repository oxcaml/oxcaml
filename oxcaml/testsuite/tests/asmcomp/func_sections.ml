(* TEST
 flags = "-S -function-sections";
 function_sections;
 {
   arch_arm64;
   reference = "${test_source_directory}/func_sections.arm64.reference";
   native;
 }{
   arch_amd64;
   reference = "${test_source_directory}/func_sections.amd64.reference";
   native;
 }
*)

(* amd64 emits jump tables as data in text; arm64 emits branch tables inline in
   the function body. This only affects compilation of f5 below. *)

(* Test for anonymous functions which result in a mangled symbol *)
let f4 list =
  List.map (fun s -> String.length s) list

let test1 () =
  f4 ["a";"asfda";"afda"]

(* Test for jump tables*)

let g1 s = s^"*"
let g2 s = "*"^s
let g3 s = "*"^s^"*"

let f5 = function
  | 1 -> g1 "a"
  | 2 -> g2 "b"
  | 3 -> g3 "c"
  | 4 -> g1 "d"
  | 5 -> g2 "e"
  | 6 -> g3 "f"
  | _ -> "x"

let test2 () =
  let list = List.map f5 [1; 2; 3; 4; 5; 6; 7; 15; 26] in
  assert (list = ["a*"; "*b"; "*c*"; "d*"; "*e"; "*f*"; "x"; "x"; "x"])

let iter = 1_000

let f0 x = x - 7;
[@@inline never]

let f1 x = x + iter
[@@inline never]

let f2 x = f1(x)
[@@inline never]

let f3 x = f2(x)*f0(x)
[@@inline never]

let test3 () =
  f3 iter


let () =
  ignore (test1 ());
  ignore (test2 ());
  ignore (test3 ());
  ()
