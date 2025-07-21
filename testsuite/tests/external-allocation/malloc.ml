(* TEST
 modules = "stubs.c";
 native;
*)

external is_young : ('a : word) -> bool = "is_young" "is_young"

external print_external_block_entries : ('a : word) -> int64# -> string -> unit = "print_block" "print_block"

let is_a_malloc name ~num_fields f =
  let prebefore = Gc.allocated_bytes () in
  let before = Gc.allocated_bytes () in
  let v = Sys.opaque_identity f () in
  let after = Gc.allocated_bytes () in
  let delta =
    int_of_float ((after -. before) -. (before -. prebefore))
      / (Sys.word_size/8)
  in
  let msg =
    match delta with
    | 0 -> "No GC-visible Allocation"
    | n -> "GC-visible Allocation ocurred"
  in
  let location =
    if is_young v then "in small heap"
    else "outside small heap"
  in
  Format.printf "%s: %s, result value is %s\n" name msg location;
  Stdlib.flush Stdlib.stdout;
  print_external_block_entries v num_fields name;
  Format.printf "\n";
  ()

(* Tuple allocations with different patterns *)
let () = is_a_malloc "tuple" ~num_fields:#2L (fun () -> malloc_ (1,3))

(* Record allocations *)
type t = {x : int; y : int}
let () = is_a_malloc "record" ~num_fields:#2L (fun () -> malloc_ {x = 3; y = 4})

type t' = {x : int; y : int64#}
let () = is_a_malloc "record_mixed" ~num_fields:#2L (fun () -> malloc_ {x = 1; y = #42L})

type t'' = {x : int64#; y : int}
let () = is_a_malloc "record_mixed2" ~num_fields:#2L (fun () -> malloc_ {x = #42L; y = 1})

type mixed_first = {unboxed_field : int64#; boxed_field : char; another_boxed : int}
let () = is_a_malloc "mixed_int64" ~num_fields:#3L (fun () -> malloc_ {unboxed_field = #100L; boxed_field = 't'; another_boxed = 42})

type mixed_middle = {start : int; middle_unboxed : float#; end_field : int64#}
let () = is_a_malloc "mixed_float" ~num_fields:#3L (fun () -> malloc_ {start = 10; middle_unboxed = #3.14; end_field = #4L})

type mixed_last = {first_boxed : bool; second_boxed : int; last_unboxed : int32#}
let () = is_a_malloc "mixed_int32" ~num_fields:#3L (fun () -> malloc_ {first_boxed = true; second_boxed = 99; last_unboxed = #777l})

type bool_record = {flag : bool; id : int}
let () = is_a_malloc "bool_record_mixed" ~num_fields:#2L (fun () -> malloc_ {flag = true; id = 0})

(* Variant allocations *)
type 'a variant = Bar of 'a
let () = is_a_malloc "variant_allocation" ~num_fields:#1L (fun () -> malloc_ (Bar 10))
let () = is_a_malloc "variant_string_arg" ~num_fields:#1L (fun () -> malloc_ (Bar "test"))

type int_variant = B of int | C of int * char | D of #(int * char * int)
let () = is_a_malloc "int_variant_single" ~num_fields:#1L (fun () -> malloc_ (B 5))
let () = is_a_malloc "int_variant_tuple" ~num_fields:#2L (fun () -> malloc_ (C (10, 'd')))
let () = is_a_malloc "int_var_unboxed" ~num_fields:#3L (fun () -> malloc_ (D #(1, 'd', 5)))

let () = is_a_malloc "option_allocation" ~num_fields:#1L (fun () -> malloc_ (Some 15))

(* List allocations *)
let () = is_a_malloc "single_element_list" ~num_fields:#2L (fun () -> malloc_ [5])

(* GADT allocations *)
type gadt = Pack : 'a -> gadt
let () = is_a_malloc "gadt_allocation" ~num_fields:#1L (fun () -> malloc_ (Pack 100))

type 'a typed_gadt = IntPack : int -> int typed_gadt | CharPack : char -> char typed_gadt
let () = is_a_malloc "typed_gadt_int" ~num_fields:#1L (fun () -> malloc_ (IntPack 99))
let () = is_a_malloc "typed_gadt_char" ~num_fields:#1L (fun () -> malloc_ (CharPack 'c'))

(* Polymorphic variants *)
let () = is_a_malloc "poly_variant" ~num_fields:#1L (fun () -> malloc_ (`Apple 7))
let () = is_a_malloc "poly_apple_const" ~num_fields:#1L (fun () -> malloc_ (`Apple 42))
