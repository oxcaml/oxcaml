(* TEST
 reference = "${test_source_directory}/custom_optimized.reference";
 flambda2;
 {
   native;
 } {
   flags = "-O3";
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   bytecode;
 }
*)

type ('a : value) t : value_or_null =
  | Nope
  | Yep of 'a
[@@or_null]

type ('a : value) flipped : value_or_null =
  | Yep_first of 'a
  | Nope_last
[@@or_null]

let print_int_t prefix = function
  | Nope -> Printf.printf "%s: Nope\n" prefix
  | Yep x -> Printf.printf "%s: Yep %d\n" prefix x

let print_string_flipped prefix = function
  | Nope_last -> Printf.printf "%s: Nope_last\n" prefix
  | Yep_first x -> Printf.printf "%s: Yep_first %s\n" prefix x

let[@inline] map_t f = function
  | Nope -> Nope
  | Yep x -> Yep (f x)

let[@inline never] bind_t f = function
  | Nope -> Nope
  | Yep x -> f x

let[@inline] map_flipped f = function
  | Nope_last -> Nope_last
  | Yep_first x -> Yep_first (f x)

let[@inline never] add_pair a b =
  match a, b with
  | Yep x, Yep y -> Yep (x + y)
  | _ -> Nope

let test_inlining () =
  print_int_t "Inline, Yep 4 + 5"
    (add_pair (Yep 4) (Yep 5));
  print_int_t "Inline, Nope + 5"
    (add_pair Nope (Yep 5));
  print_int_t "Map, increment"
    (map_t (fun x -> x + 1) (Yep 7));
  print_int_t "Map, increment Nope"
    (map_t (fun x -> x + 1) Nope)

let () = test_inlining ()

let test_higher_order () =
  let safe_half x =
    if x land 1 = 0 then Yep (x / 2) else Nope
  in
  print_int_t "Bind, Yep 8" (bind_t safe_half (Yep 8));
  print_int_t "Bind, Yep 3" (bind_t safe_half (Yep 3));
  print_int_t "Bind, Nope" (bind_t safe_half Nope);
  print_string_flipped "Flipped map"
    (map_flipped String.uppercase_ascii (Yep_first "custom"));
  print_string_flipped "Flipped map Nope"
    (map_flipped String.uppercase_ascii Nope_last)

let () = test_higher_order ()

let test_closures () =
  let[@inline never] make_adder delta =
    fun x ->
      match x, delta with
      | Yep n, Yep d -> Yep (n + d)
      | _ -> Nope
  in
  let add_two = make_adder (Yep 2) in
  let add_none = make_adder Nope in
  print_int_t "Closure, add two" (add_two (Yep 10));
  print_int_t "Closure, add two to Nope" (add_two Nope);
  print_int_t "Closure, add none" (add_none (Yep 10))

let () = test_closures ()

let test_refs_and_opaque () =
  let r = Sys.opaque_identity (ref Nope_last) in
  print_string_flipped "Ref, initial" !r;
  r := Yep_first "kept";
  print_string_flipped "Ref, updated" !r

let () = test_refs_and_opaque ()
