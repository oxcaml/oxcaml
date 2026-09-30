(* TEST
 include stdlib_stable;
 include stdlib_upstream_compatible;
 flambda2;
 stack-allocation;
 native;
*)

(* [%makearray_dynamic_uninit] creates arrays whose elements are not scanned by
   the GC by calling the runtime, except for small arrays of known length,
   which are allocated inline. For local arrays, the [@zero_alloc] annotations
   below check that no runtime function is called. Both must create the same
   arrays. *)

open Stdlib_stable
open Stdlib_upstream_compatible

external[@layout_poly] make_local :
  ('a : any mod separable). int -> 'a array @ local
  = "%makearray_dynamic_uninit"

external[@layout_poly] make_heap :
  ('a : any mod separable). int -> 'a array = "%makearray_dynamic_uninit"

external[@layout_poly] make_init_local :
  ('a : any mod separable). int -> 'a -> 'a array @ local
  = "%makearray_dynamic"

external[@layout_poly] length :
  ('a : any mod separable). ('a array[@local_opt]) -> int = "%array_length"

external[@layout_poly] get :
  ('a : any mod separable). ('a array[@local_opt]) -> int -> 'a
  = "%array_safe_get"

external[@layout_poly] set :
  ('a : any mod separable). ('a array[@local_opt]) -> int -> 'a -> unit
  = "%array_safe_set"

external repr : ('a[@local_opt]) -> (Obj.t[@local_opt]) = "%identity"
external obj : (Obj.t[@local_opt]) -> ('a[@local_opt]) = "%identity"
external is_stack : Obj.t @ local -> bool = "caml_obj_is_stack"
external tag : Obj.t @ local -> int = "caml_obj_tag" [@@noalloc]
external size : Obj.t @ local -> int = "%obj_size"
external local_stack_offset : unit -> int = "caml_local_stack_offset"

let local_stack_words () = local_stack_offset () / (Sys.word_size / 8)

type product = #(float# * int32_u * int8#)

let[@inline always] make_float n = exclave_ repr (make_local n : float# array)

let[@inline always] make_float32 n =
  exclave_ repr (make_local n : float32_u array)

let[@inline always] make_int n = exclave_ repr (make_local n : int# array)

let[@inline always] make_int8 n = exclave_ repr (make_local n : int8# array)

let[@inline always] make_int16 n = exclave_ repr (make_local n : int16# array)

let[@inline always] make_int32 n = exclave_ repr (make_local n : int32_u array)

let[@inline always] make_int64 n = exclave_ repr (make_local n : int64_u array)

let[@inline always] make_nativeint n =
  exclave_ repr (make_local n : nativeint_u array)

let[@inline always] make_product n =
  exclave_ repr (make_local n : product array)

let[@inline never] describe ~(make : unit -> Obj.t @ local)
    ~(length : Obj.t @ local -> int) =
  let before = local_stack_words () in
  let a = make () in
  let words = local_stack_words () - before in
  Gc.full_major ();
  Printf.sprintf "Array.length %d, tag %d, size %d, words %d, stack %b"
    (length a) (tag a) (size a) words (is_stack a)

let unknown_lengths = [ 0; 1; 2; 3; 4; 5; 7; 8; 9; 100 ]

let describe_with_unknown_length ~(make : int -> Obj.t @ local) ~length n =
  describe ~make:(fun () -> exclave_ make (Sys.opaque_identity n)) ~length

let[@inline never] check_known_lengths name ~make ~length
    ~(known_lengths : (int * (unit -> Obj.t @ local)) list) =
  List.iter
    (fun (n, make_with_known_length) ->
      let actual = describe ~make:make_with_known_length ~length in
      let expected = describe_with_unknown_length ~make ~length n in
      if not (String.equal actual expected)
      then
        Printf.printf "%s, known length %d: %s, but %s for unknown length\n"
          name n actual expected)
    known_lengths;
  Printf.printf "%s: checked known lengths %s\n" name
    (String.concat ", "
       (List.map (fun (n, _) -> Int.to_string n) known_lengths))

let[@inline never] test_layout name ~make ~length ~known_lengths =
  List.iter
    (fun n ->
      Printf.printf "%s, length %d: %s\n" name n
        (describe_with_unknown_length ~make ~length n))
    unknown_lengths;
  check_known_lengths name ~make ~length ~known_lengths

(* The largest known lengths are those of arrays of [Config.max_young_wosize]
   words, which are the largest arrays allocated inline. *)
let () =
  test_layout "float#" ~make:make_float
    ~length:(fun a -> length (obj a : float# array))
    ~known_lengths:
      [ 1, (fun[@zero_alloc] () -> exclave_ make_float 1);
        3, (fun[@zero_alloc] () -> exclave_ make_float 3);
        256, (fun[@zero_alloc] () -> exclave_ make_float 256) ];
  test_layout "float32_u" ~make:make_float32
    ~length:(fun a -> length (obj a : float32_u array))
    ~known_lengths:
      [ 1, (fun[@zero_alloc] () -> exclave_ make_float32 1);
        2, (fun[@zero_alloc] () -> exclave_ make_float32 2);
        3, (fun[@zero_alloc] () -> exclave_ make_float32 3);
        512, (fun[@zero_alloc] () -> exclave_ make_float32 512) ];
  test_layout "int#" ~make:make_int
    ~length:(fun a -> length (obj a : int# array))
    ~known_lengths:
      [ 1, (fun[@zero_alloc] () -> exclave_ make_int 1);
        3, (fun[@zero_alloc] () -> exclave_ make_int 3) ];
  test_layout "int8#" ~make:make_int8
    ~length:(fun a -> length (obj a : int8# array))
    ~known_lengths:
      [ 1, (fun[@zero_alloc] () -> exclave_ make_int8 1);
        2, (fun[@zero_alloc] () -> exclave_ make_int8 2);
        3, (fun[@zero_alloc] () -> exclave_ make_int8 3);
        4, (fun[@zero_alloc] () -> exclave_ make_int8 4);
        5, (fun[@zero_alloc] () -> exclave_ make_int8 5);
        6, (fun[@zero_alloc] () -> exclave_ make_int8 6);
        7, (fun[@zero_alloc] () -> exclave_ make_int8 7);
        8, (fun[@zero_alloc] () -> exclave_ make_int8 8);
        9, (fun[@zero_alloc] () -> exclave_ make_int8 9);
        2048, (fun[@zero_alloc] () -> exclave_ make_int8 2048) ];
  test_layout "int16#" ~make:make_int16
    ~length:(fun a -> length (obj a : int16# array))
    ~known_lengths:
      [ 1, (fun[@zero_alloc] () -> exclave_ make_int16 1);
        2, (fun[@zero_alloc] () -> exclave_ make_int16 2);
        3, (fun[@zero_alloc] () -> exclave_ make_int16 3);
        4, (fun[@zero_alloc] () -> exclave_ make_int16 4);
        5, (fun[@zero_alloc] () -> exclave_ make_int16 5) ];
  test_layout "int32_u" ~make:make_int32
    ~length:(fun a -> length (obj a : int32_u array))
    ~known_lengths:
      [ 1, (fun[@zero_alloc] () -> exclave_ make_int32 1);
        2, (fun[@zero_alloc] () -> exclave_ make_int32 2);
        3, (fun[@zero_alloc] () -> exclave_ make_int32 3) ];
  test_layout "int64_u" ~make:make_int64
    ~length:(fun a -> length (obj a : int64_u array))
    ~known_lengths:
      [ 1, (fun[@zero_alloc] () -> exclave_ make_int64 1);
        3, (fun[@zero_alloc] () -> exclave_ make_int64 3) ];
  test_layout "nativeint_u" ~make:make_nativeint
    ~length:(fun a -> length (obj a : nativeint_u array))
    ~known_lengths:
      [ 1, (fun[@zero_alloc] () -> exclave_ make_nativeint 1);
        3, (fun[@zero_alloc] () -> exclave_ make_nativeint 3) ];
  test_layout "product" ~make:make_product
    ~length:(fun a -> length (obj a : product array))
    ~known_lengths:
      [ 1, (fun[@zero_alloc] () -> exclave_ make_product 1);
        3, (fun[@zero_alloc] () -> exclave_ make_product 3);
        85, (fun[@zero_alloc] () -> exclave_ make_product 85) ]

(* Small heap arrays of known length are allocated inline too, on the minor
   heap. *)
let[@inline always] make_heap_float n = repr (make_heap n : float# array)

let[@inline always] make_heap_int8 n = repr (make_heap n : int8# array)

let[@inline always] make_heap_product n = repr (make_heap n : product array)

let () =
  check_known_lengths "float# (heap)" ~make:make_heap_float
    ~length:(fun a -> length (obj a : float# array))
    ~known_lengths:
      [ 0, (fun () -> make_heap_float 0);
        3, (fun () -> make_heap_float 3);
        256, (fun () -> make_heap_float 256) ];
  check_known_lengths "int8# (heap)" ~make:make_heap_int8
    ~length:(fun a -> length (obj a : int8# array))
    ~known_lengths:
      [ 5, (fun () -> make_heap_int8 5);
        2048, (fun () -> make_heap_int8 2048) ];
  check_known_lengths "product (heap)" ~make:make_heap_product
    ~length:(fun a -> length (obj a : product array))
    ~known_lengths:
      [ 3, (fun () -> make_heap_product 3);
        85, (fun () -> make_heap_product 85) ]

(* Initialized arrays are built on top of uninitialized ones. *)
let[@inline never] check_initialized_arrays n =
  let floats : float# array = make_init_local n #1.5 in
  let products : #(int * float#) array = make_init_local n #(42, #1.5) in
  Gc.full_major ();
  for i = 0 to n - 1 do
    let #(x, y) = get products i in
    if Float_u.to_float (get floats i) <> 1.5 || x <> 42
       || Float_u.to_float y <> 1.5
    then Printf.printf "initialized arrays: bad element %d of %d\n" i n
  done

let () =
  check_initialized_arrays 5;
  check_initialized_arrays (Sys.opaque_identity 5);
  print_endline "initialized arrays: done"

let check_invalid_length name make ~description n =
  match make (Sys.opaque_identity n) with
  | (_ : Obj.t) -> Printf.printf "%s, %s: no exception\n" name description
  | exception Invalid_argument msg ->
    Printf.printf "%s, %s: Invalid_argument %S\n" name description msg

let () =
  let check name make ~max_length =
    List.iter
      (fun (description, n) -> check_invalid_length name make ~description n)
      [ "-1", -1;
        "min_int", min_int;
        "max_int", max_int;
        "max_length + 1", max_length + 1 ]
  in
  check "float#" make_float ~max_length:Sys.max_array_length;
  check "int8#" make_int8 ~max_length:(8 * Sys.max_array_length);
  check "int32_u" make_int32 ~max_length:(2 * Sys.max_array_length);
  check "product" make_product ~max_length:(Sys.max_array_length / 3)

(* The same exceptions are raised for invalid lengths that are known. *)
let () =
  let check name (make : unit -> Obj.t @ local) =
    match make () with
    | (_ : Obj.t) -> Printf.printf "%s: no exception\n" name
    | exception Invalid_argument msg ->
      Printf.printf "%s: Invalid_argument %S\n" name msg
  in
  check "float#, known length -1" (fun () -> exclave_ make_float (-1));
  check "int8#, known length -1" (fun () -> exclave_ make_int8 (-1));
  check "int8#, known length max_int" (fun () -> exclave_ make_int8 max_int);
  check "product, known length -1" (fun () -> exclave_ make_product (-1))

(* Keep enough arrays live at once that the local stack has to grow several
   times, including by more than the usual growth factor for a single array,
   with arrays allocated both inline and by the runtime. *)
let rec grow_local_stack depth =
  if depth = 0
  then begin
    let n = Sys.opaque_identity 3_000_000 in
    let a : int64_u array = obj (make_int64 n) in
    set a (n - 1) #42L;
    Gc.full_major ();
    if Int64_u.to_int (get a (n - 1)) <> 42
    then print_endline "grow_local_stack: bad element in large array"
  end
  else begin
    let n = depth * 1_000 in
    let a : product array = obj (make_product n) in
    let b : int64_u array = obj (make_int64 200) in
    for i = 0 to n - 1 do
      set a i
        #(Float_u.of_int i, Int32_u.of_int depth, Int8_u.of_int (i land 127))
    done;
    for i = 0 to 199 do
      set b i (Int64_u.of_int (depth + i))
    done;
    grow_local_stack (depth - 1);
    for i = 0 to n - 1 do
      let #(x, y, z) = get a i in
      if Float_u.to_int x <> i || Int32_u.to_int y <> depth
         || Int8_u.to_int z <> i land 127
      then
        Printf.printf "grow_local_stack: bad element %d at depth %d\n" i depth
    done;
    for i = 0 to 199 do
      if Int64_u.to_int (get b i) <> depth + i
      then
        Printf.printf "grow_local_stack: bad inline element %d at depth %d\n"
          i depth
    done
  end

let () =
  grow_local_stack 20;
  print_endline "grow_local_stack: done"
