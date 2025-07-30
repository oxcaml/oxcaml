(* TEST
 modules = "stubs.c";
 native;
*)

external is_young : ('a : word) -> bool = "is_young" "is_young"

external is_static_alloc : ('a : word) -> bool = "is_static_alloc" "is_static_alloc"

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
    | 0 -> "No GC-visible allocation ocurred"
    | n -> "GC-VISIBLE ALLOCATION OCCURRED"
  in
  let location =
    if is_young v then "IN MINOR HEAP"
    else "outside minor heap"
  in
  let allocation_time =
    if is_static_alloc v then "COMPILE TIME"
    else "runtime"
  in
  Format.printf "%s: %s, result value is %s, allocated at %s\n" name msg location allocation_time;
  Stdlib.flush Stdlib.stdout;
  print_external_block_entries v num_fields name;
  Format.printf "\n";
  ()


(*
We test each form of allocation two ways:
1. Where a constant value is directly [malloc_]'d to test that malloc prevents
   blocks from being lifted to statically-allocated structured constants
2. Where the malloc is seperatedly defined as a function, which is then called.
*)

(* Tuple allocations with different patterns *)
let () = is_a_malloc "tuple" ~num_fields:#2L (fun () -> malloc_ (1,3))

let tuple_func x y = malloc_ (x, y)
let () = is_a_malloc "tuple-func-creation" ~num_fields:#2L (fun () -> tuple_func 1 3)

(* Record allocations *)
type t = {x : int; y : int}
let () = is_a_malloc "record" ~num_fields:#2L (fun () -> malloc_ {x = 3; y = 4})

let f x y = malloc_ {x ; y}
let () = is_a_malloc "record-func-creation" ~num_fields:#2L (fun () -> f 3 4)

type t' = {x : int; y : int64#}
let () = is_a_malloc "record_mixed" ~num_fields:#2L (fun () -> malloc_ {x = 1; y = #42L})

let mixed_func x y = malloc_ {x; y}
let () = is_a_malloc "record_mixed-func-creation" ~num_fields:#2L (fun () -> mixed_func 1 #42L)

type t'' = {x : int64#; y : int}
let () = is_a_malloc "record_mixed2" ~num_fields:#2L (fun () -> malloc_ {x = #42L; y = 1})

let mixed2_func x y = malloc_ {x; y}
let () = is_a_malloc "record_mixed2-func-creation" ~num_fields:#2L (fun () -> mixed2_func #42L 1)

type t0 = {x : int64#; y: char; z: int}
let () = is_a_malloc "t0" ~num_fields:#3L (fun () -> malloc_ {x = #100L; y = 't'; z = 42})

let t0_func x y z = malloc_ {x; y; z}
let () = is_a_malloc "t0-func-creation" ~num_fields:#3L (fun () -> t0_func #100L 't' 42)

type t1 = {x : int; y : float#; z : int64#}
let () = is_a_malloc "t1" ~num_fields:#3L (fun () -> malloc_ {x = 10; y = #3.14; z = #4L})

let t1_func x y z = malloc_ {x; y; z}
let () = is_a_malloc "t1-func-creation" ~num_fields:#3L (fun () -> t1_func 10 #3.14 #4L)

type t2 = {x : bool; y : int; z : int32#}
let () = is_a_malloc "t2 " ~num_fields:#3L (fun () -> malloc_ {x = true; y = 99; z = #777l})

let t2_func x y z = malloc_ {x; y; z}
let () = is_a_malloc "t2-func-creation" ~num_fields:#3L (fun () -> t2_func true 99 #777l)

type r0 = {flag : bool; id : int}
let () = is_a_malloc "r0" ~num_fields:#2L (fun () -> malloc_ {flag = true; id = 0})

let r0_func flag id = malloc_ {flag; id}
let () = is_a_malloc "r0-func-creation" ~num_fields:#2L (fun () -> r0_func true 0)

type r1 = Foo of {flag : bool; id : int}
let () = is_a_malloc "r1" ~num_fields:#2L (fun () -> malloc_ (Foo {flag = true; id = 0}))

let r1_func flag id = malloc_ (Foo {flag; id})
let () = is_a_malloc "r1-func-creation" ~num_fields:#2L (fun () -> r1_func true 0)

(* Variant allocations *)
type 'a variant = Bar of 'a
let () = is_a_malloc "Bar" ~num_fields:#1L (fun () -> malloc_ (Bar 10))

let bar_func x = malloc_ (Bar x)
let () = is_a_malloc "Bar-func-creation" ~num_fields:#1L (fun () -> bar_func 10)

type int_variant = B of int | C of int * char | D of #(int * char * int)
let () = is_a_malloc "B5" ~num_fields:#1L (fun () -> malloc_ (B 5))
let () = is_a_malloc "C10d" ~num_fields:#2L (fun () -> malloc_ (C (10, 'd')))
let () = is_a_malloc "D1d5" ~num_fields:#3L (fun () -> malloc_ (D #(1, 'd', 5)))

let b_func x = malloc_ (B x)
let () = is_a_malloc "B5-func-creation" ~num_fields:#1L (fun () -> b_func 5)

let c_func x y = malloc_ (C (x, y))
let () = is_a_malloc "C10d-func-creation" ~num_fields:#2L (fun () -> c_func 10 'd')

let d_func x y z = malloc_ (D #(x, y, z))
let () = is_a_malloc "D1d5-func-creation" ~num_fields:#3L (fun () -> d_func 1 'd' 5)

type f =  D of #(int * char * int) * int
let () = is_a_malloc "f2" ~num_fields:#4L (fun () -> malloc_ (D (#(1,'d',5),6)))

let f_func a b c d = malloc_ (D (#(a,b,c),d))
let () = is_a_malloc "f2-func-creation" ~num_fields:#4L (fun () -> f_func 1 'd' 5 6)

(* List allocations *)
let () = is_a_malloc "list5" ~num_fields:#2L (fun () -> malloc_ [5])

let list_func x = malloc_ [x]
let () = is_a_malloc "list5-func-creation" ~num_fields:#2L (fun () -> list_func 5)

(* GADT allocations *)
type gadt = Pack : 'a -> gadt
let () = is_a_malloc "Pack100" ~num_fields:#1L (fun () -> malloc_ (Pack 100))

let pack_func x = malloc_ (Pack x)
let () = is_a_malloc "Pack100-func-creation" ~num_fields:#1L (fun () -> pack_func 100)

type 'a g =
  | IntPack : int -> int g
  | CharPack : char -> char g
  | UnboxPack : #(int * char) -> unit g
let () = is_a_malloc "gadt_int" ~num_fields:#1L (fun () -> malloc_ (IntPack 99))
let () = is_a_malloc "gadt_char" ~num_fields:#1L (fun () -> malloc_ (CharPack 'c'))
let () = is_a_malloc "gadt_char" ~num_fields:#1L (fun () -> malloc_ (UnboxPack #(1,'a')))

let intpack_func x = malloc_ (IntPack x)
let () = is_a_malloc "gadt_int-func-creation" ~num_fields:#1L (fun () -> intpack_func 99)

let charpack_func x = malloc_ (CharPack x)
let () = is_a_malloc "gadt_char-func-creation" ~num_fields:#1L (fun () -> charpack_func 'c')

let unboxpack_func x y = malloc_ (UnboxPack #(x, y))
let () = is_a_malloc "gadt_unbox-func-creation" ~num_fields:#1L (fun () -> unboxpack_func 1 'a')

(* Polymorphic variants *)
let () = is_a_malloc "apple" ~num_fields:#2L (fun () -> malloc_ (`Apple 7))
let () = is_a_malloc "apple2" ~num_fields:#2L (fun () -> malloc_ (`Apple 42))
let () = is_a_malloc "pear" ~num_fields:#2L (fun () -> malloc_ (`Pear 42))

let apple_func x = malloc_ (`Apple x)
let () = is_a_malloc "apple-func-creation" ~num_fields:#2L (fun () -> apple_func 7)

let apple2_func x = malloc_ (`Apple x)
let () = is_a_malloc "apple2-func-creation" ~num_fields:#2L (fun () -> apple2_func 42)

let pear_func x = malloc_ (`Pear x)
let () = is_a_malloc "pear-func-creation" ~num_fields:#2L (fun () -> pear_func 42)
