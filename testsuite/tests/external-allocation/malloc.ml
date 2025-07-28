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
    if is_young v then "in minor heap"
    else "outside minor heap"
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

type t0 = {x : int64#; y: char; z: int}
let () = is_a_malloc "t0" ~num_fields:#3L (fun () -> malloc_ {x = #100L; y = 't'; z = 42})

type t1 = {x : int; y : float#; z : int64#}
let () = is_a_malloc "t1" ~num_fields:#3L (fun () -> malloc_ {x = 10; y = #3.14; z = #4L})

type t2 = {x : bool; y : int; z : int32#}
let () = is_a_malloc "t2 " ~num_fields:#3L (fun () -> malloc_ {x = true; y = 99; z = #777l})

type r0 = {flag : bool; id : int}
let () = is_a_malloc "r0" ~num_fields:#2L (fun () -> malloc_ {flag = true; id = 0})

type r1 = Foo of {flag : bool; id : int}
let () = is_a_malloc "r1" ~num_fields:#2L (fun () -> malloc_ (Foo {flag = true; id = 0}))

(* Variant allocations *)
type 'a variant = Bar of 'a
let () = is_a_malloc "Bar" ~num_fields:#1L (fun () -> malloc_ (Bar 10))

type int_variant = B of int | C of int * char | D of #(int * char * int)
let () = is_a_malloc "B5" ~num_fields:#1L (fun () -> malloc_ (B 5))
let () = is_a_malloc "C10d" ~num_fields:#2L (fun () -> malloc_ (C (10, 'd')))
let () = is_a_malloc "D1d5" ~num_fields:#3L (fun () -> malloc_ (D #(1, 'd', 5)))

let () = is_a_malloc "Some15" ~num_fields:#1L (fun () -> malloc_ (Some 15))

(* List allocations *)
let () = is_a_malloc "list5" ~num_fields:#2L (fun () -> malloc_ [5])

(* GADT allocations *)
type gadt = Pack : 'a -> gadt
let () = is_a_malloc "Pack100" ~num_fields:#1L (fun () -> malloc_ (Pack 100))

type 'a g =
  | IntPack : int -> int g
  | CharPack : char -> char g
  | UnboxPack : #(int * char) -> unit g
let () = is_a_malloc "gadt_int" ~num_fields:#1L (fun () -> malloc_ (IntPack 99))
let () = is_a_malloc "gadt_char" ~num_fields:#1L (fun () -> malloc_ (CharPack 'c'))
let () = is_a_malloc "gadt_char" ~num_fields:#1L (fun () -> malloc_ (UnboxPack #(1,'a')))

(* Polymorphic variants *)
let () = is_a_malloc "apple" ~num_fields:#2L (fun () -> malloc_ (`Apple 7))
let () = is_a_malloc "apple2" ~num_fields:#2L (fun () -> malloc_ (`Apple 42))
let () = is_a_malloc "pear" ~num_fields:#2L (fun () -> malloc_ (`Pear 42))
