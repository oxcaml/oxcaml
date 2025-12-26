(* Test complex control flow patterns *)

(* Nested conditionals *)
let classify n =
  if n < 0
  then "negative"
  else if n = 0
  then "zero"
  else if n < 10
  then "small"
  else if n < 100
  then "medium"
  else "large"

(* Pattern matching on variants *)
type color =
  | Red
  | Green
  | Blue
  | RGB of int * int * int

let color_to_int = function
  | Red -> 0xFF0000
  | Green -> 0x00FF00
  | Blue -> 0x0000FF
  | RGB (r, g, b) -> (r lsl 16) lor (g lsl 8) lor b

(* Pattern matching on options *)
let get_or_default default = function None -> default | Some x -> x

(* Pattern matching on lists *)
let rec sum_list = function [] -> 0 | x :: xs -> x + sum_list xs

let rec length_list = function [] -> 0 | _ :: xs -> 1 + length_list xs

(* Mutual recursion *)
let rec is_even n = if n = 0 then true else is_odd (n - 1)

and is_odd n = if n = 0 then false else is_even (n - 1)

(* While loop equivalent using recursion *)
let rec count_down n acc = if n <= 0 then acc else count_down (n - 1) (n :: acc)

(* Entry point *)
let () =
  let _ = classify 42 in
  let _ = classify (-5) in
  let _ = color_to_int Red in
  let _ = color_to_int (RGB (128, 64, 32)) in
  let _ = get_or_default 0 None in
  let _ = get_or_default 0 (Some 42) in
  let _ = sum_list [1; 2; 3; 4; 5] in
  let _ = length_list [1; 2; 3] in
  let _ = is_even 10 in
  let _ = is_odd 7 in
  let _ = count_down 5 [] in
  ()
