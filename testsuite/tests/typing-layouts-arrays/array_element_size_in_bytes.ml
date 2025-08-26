(* TEST
 modules = "block_checks.ml";
 flambda2;
 stack-allocation;
 {
   bytecode;
 } {
   native;
 }
*)

(* Unboxed arrays now use normal blocks instead of custom blocks *)

(* We only compile for 64 bits. *)
let bytes_per_word = 8

(* Tag definitions from Cmm_helpers.Unboxed_array_tags *)
let unboxed_product_array_tag = 0
let unboxed_int64_array_tag = 1
let unboxed_int32_array_even_tag = 2
let unboxed_int32_array_odd_tag = 3
let unboxed_float32_array_even_tag = 4
let unboxed_float32_array_odd_tag = 5
let unboxed_vec128_array_tag = 6
let unboxed_vec256_array_tag = 7
let unboxed_vec512_array_tag = 8
let unboxed_nativeint_array_tag = 9



external[@layout_poly] size_in_bytes : ('a : any_non_null). 'a array -> int
  = "%array_element_size_in_bytes"

external[@layout_poly] makearray_dynamic :
  ('a : any_non_null). int -> 'a -> 'a array = "%makearray_dynamic"

let array_sizes_to_check = [0; 1; 2; 25]

(* values *)
let check_value ~init ~element_size =
  (* It is unfortunately necessary to duplicate this function many times because
     we don't have layout polymorphism. *)
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)))
  in
  List.iter check_one array_sizes_to_check

let int_array_element_size = size_in_bytes ([||] : int array)
let _ = check_value ~init:42 ~element_size:int_array_element_size

let string_array_element_size = size_in_bytes ([||] : string array)
let _ = check_value ~init:"abc" ~element_size:int_array_element_size

let float_array_element_size = size_in_bytes ([||] : float array)
let _ = check_value ~init:42.0 ~element_size:int_array_element_size

let float32_array_element_size = size_in_bytes ([||] : float32 array)
let _ = check_value ~init:42.0s ~element_size:int_array_element_size

let int32_array_element_size = size_in_bytes ([||] : int32 array)
let _ = check_value ~init:42l ~element_size:int_array_element_size

(* unboxed floats *)
let check_floatu ~init ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)));
    (* float# arrays use Double_array_tag (254) when non-empty, tag 0 when empty *)
    let tag = Obj.tag (Obj.repr x) in
    let expected_tag = if n = 0 then 0 else 254 in
    assert (tag = expected_tag)
  in
  List.iter check_one array_sizes_to_check

let floatu_array_element_size = size_in_bytes ([||] : float# array)

let _ = check_floatu ~init:#42.0 ~element_size:floatu_array_element_size

(* unboxed int64s *)
let check_int64u ~(init : int64#) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)));
    if n = 0 then
      Block_checks.check_empty_array_is_uniform ~array_type:"int64#" (Obj.repr x)
    else begin
      (* Non-empty arrays have specific tags and are mixed blocks *)
      let tag = Obj.tag (Obj.repr x) in
      let expected_tag = 
        match Sys.backend_type with
        | Native -> unboxed_int64_array_tag
        | Bytecode | Other _ -> 0
      in
      assert (tag = expected_tag);
      (* Check mixed block has zero scannable fields *)
      match Sys.backend_type with
      | Native -> Block_checks.check_mixed_block_scannable_size ~array_type:"int64#" (Obj.repr x) 0
      | Bytecode | Other _ -> ()
    end
  in
  List.iter check_one array_sizes_to_check

let int64u_array_element_size = size_in_bytes ([||] : int64# array)

let _ = check_int64u ~init:#42L ~element_size:int64u_array_element_size

(* unboxed nativeints *)
let check_nativeintu ~(init : nativeint#) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)));
    if n = 0 then
      Block_checks.check_empty_array_is_uniform ~array_type:"nativeint#" (Obj.repr x)
    else begin
      (* Non-empty arrays have specific tags and are mixed blocks *)
      let tag = Obj.tag (Obj.repr x) in
      let expected_tag = 
        match Sys.backend_type with
        | Native -> unboxed_nativeint_array_tag
        | Bytecode | Other _ -> 0
      in
      assert (tag = expected_tag);
      (* Check mixed block has zero scannable fields *)
      match Sys.backend_type with
      | Native -> Block_checks.check_mixed_block_scannable_size ~array_type:"nativeint#" (Obj.repr x) 0
      | Bytecode | Other _ -> ()
    end
  in
  List.iter check_one array_sizes_to_check

let nativeintu_array_element_size = size_in_bytes ([||] : nativeint# array)

let _ = check_nativeintu ~init:#42n ~element_size:nativeintu_array_element_size

(* unboxed float32s *)
let check_float32u ~(init : float32#) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    (* These arrays are packed in native code *)
    let padded_n =
      match Sys.backend_type with
      | Native -> if n mod 2 = 0 then n else n + 1
      | Bytecode -> n
      | Other _ -> failwith "Don't know what to do"
    in
    assert ((element_size * padded_n / bytes_per_word) = (Obj.size (Obj.repr x)));
    if n = 0 then
      Block_checks.check_empty_array_is_uniform ~array_type:"float32#" (Obj.repr x)
    else begin
      (* Check tag based on actual array length (n) not padded length *)
      let tag = Obj.tag (Obj.repr x) in
      match Sys.backend_type with
      | Native ->
        (* Tag is based on original element count, not padded count *)
        let expected_tag = 
          if n mod 2 = 0 then unboxed_float32_array_even_tag 
          else unboxed_float32_array_odd_tag in
        assert (tag = expected_tag);
        (* Check mixed block has zero scannable fields *)
        Block_checks.check_mixed_block_scannable_size ~array_type:"float32#" (Obj.repr x) 0
      | Bytecode | Other _ -> ()
    end
  in
  List.iter check_one array_sizes_to_check

let float32u_array_element_size = size_in_bytes ([||] : float32# array)

let _ = check_float32u ~init:#42.0s ~element_size:float32u_array_element_size

(* unboxed int32s *)
let check_int32u ~(init : int32#) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    (* These arrays are packed in native code *)
    let padded_n =
      match Sys.backend_type with
      | Native -> if n mod 2 = 0 then n else n + 1
      | Bytecode -> n
      | Other _ -> failwith "Don't know what to do"
    in
    assert ((element_size * padded_n / bytes_per_word) = (Obj.size (Obj.repr x)));
    if n = 0 then
      Block_checks.check_empty_array_is_uniform ~array_type:"int32#" (Obj.repr x)
    else begin
      (* Check tag based on actual array length (n) not padded length *)
      let tag = Obj.tag (Obj.repr x) in
      match Sys.backend_type with
      | Native ->
        (* Tag is based on original element count, not padded count *)
        let expected_tag = 
          if n mod 2 = 0 then unboxed_int32_array_even_tag 
          else unboxed_int32_array_odd_tag in
        assert (tag = expected_tag);
        (* Check mixed block has zero scannable fields *)
        Block_checks.check_mixed_block_scannable_size ~array_type:"int32#" (Obj.repr x) 0
      | Bytecode | Other _ -> ()
    end
  in
  List.iter check_one array_sizes_to_check

let int32u_array_element_size = size_in_bytes ([||] : int32# array)

let _ = check_int32u ~init:#42l ~element_size:int32u_array_element_size

(* simple scannable products *)
let check_scannable_product1 ~(init : #(int * string * int * float array))
      ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)));
    let tag = Obj.tag (Obj.repr x) in
    (* All unboxed product arrays use tag 0, which matches empty array tag *)
    assert (tag = unboxed_product_array_tag);
    (* Scannable product arrays are always uniform blocks, never mixed *)
    match Sys.backend_type with
    | Native ->
      let mixed_info = Obj.Uniform_or_mixed.of_block (Obj.repr x) in
      begin match Obj.Uniform_or_mixed.repr mixed_info with
      | Uniform -> ()  (* Scannable products are uniform blocks - expected *)
      | Mixed _ -> assert false  (* Should not be mixed *)
      end
    | Bytecode | Other _ -> ()
  in
  List.iter check_one array_sizes_to_check

let scannable_product1_array_element_size =
  size_in_bytes ([||] : #(int * string * int * float array) array)

let _ = check_scannable_product1 ~init:#(42, "hi", 0, [| 1.0; 2.0; 3.0 |])
          ~element_size:scannable_product1_array_element_size

(* complex scannable products *)
type t_scan = #{ x : int; y : #(float * string); z: int option }

let check_scannable_product2 ~(init : #(int * t_scan * string * t_scan))
      ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)));
    let tag = Obj.tag (Obj.repr x) in
    (* All unboxed product arrays use tag 0, which matches empty array tag *)
    assert (tag = unboxed_product_array_tag);
    (* Scannable product arrays are always uniform blocks, never mixed *)
    match Sys.backend_type with
    | Native ->
      let mixed_info = Obj.Uniform_or_mixed.of_block (Obj.repr x) in
      begin match Obj.Uniform_or_mixed.repr mixed_info with
      | Uniform -> ()  (* Scannable products are uniform blocks - expected *)
      | Mixed _ -> assert false  (* Should not be mixed *)
      end
    | Bytecode | Other _ -> ()
  in
  List.iter check_one array_sizes_to_check

let mk_el () =
  #(42,
    #{ x = 42; y = #(42.0, "hi"); z = Some 42 },
    "hi",
    #{ x = 42; y = #(42.0, "hi"); z = Some 42 })

let scannable_product2_array_element_size =
  size_in_bytes ([||] : #(int * t_scan * string * t_scan) array)

let _ = check_scannable_product2 ~init:(mk_el ())
          ~element_size:scannable_product2_array_element_size

(* simple ignorable products *)
let check_ignorable_product1 ~(init : #(int * float32# * int * int64#))
      ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)));
    let tag = Obj.tag (Obj.repr x) in
    (* All unboxed product arrays use tag 0, which matches empty array tag *)
    assert (tag = unboxed_product_array_tag);
    if n = 0 then
      Block_checks.check_empty_array_is_uniform ~array_type:"ignorable_product1" (Obj.repr x)
    else begin
      (* Non-empty ignorable products are mixed blocks with no scannable fields *)
      match Sys.backend_type with
      | Native ->
        (* This product has mixed fields but is allocated as fully non-scannable *)
        Block_checks.check_mixed_block_scannable_size ~array_type:"ignorable_product1" (Obj.repr x) 0
      | Bytecode | Other _ -> ()
    end
  in
  List.iter check_one array_sizes_to_check

let ignorable_product1_array_element_size =
  size_in_bytes ([||] : #(int * float32# * int * int64#) array)

let _ = check_ignorable_product1 ~init:#(42, #42.0s, 0, #42L)
          ~element_size:ignorable_product1_array_element_size

(* complex ignorable products *)
type t_ignore = #{ x : int; y : #(float# * int32#); z: int32# }

let check_ignorable_product2 ~(init : #(int * t_ignore * bool * t_ignore))
      ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    assert ((element_size * n / bytes_per_word) = (Obj.size (Obj.repr x)));
    let tag = Obj.tag (Obj.repr x) in
    (* All unboxed product arrays use tag 0, which matches empty array tag *)
    assert (tag = unboxed_product_array_tag);
    if n = 0 then
      Block_checks.check_empty_array_is_uniform ~array_type:"ignorable_product2" (Obj.repr x)
    else begin
      (* Non-empty ignorable products are mixed blocks with no scannable fields *)
      match Sys.backend_type with
      | Native ->
        (* Product has mixed fields but is allocated as fully non-scannable *)
        Block_checks.check_mixed_block_scannable_size ~array_type:"ignorable_product2" (Obj.repr x) 0
      | Bytecode | Other _ -> ()
    end
  in
  List.iter check_one array_sizes_to_check

let mk_el () =
  #(42,
    #{ x = 42; y = #(#41.0, #40l); z = #43l },
    true,
    #{ x = 42; y = #(#41.0, #40l); z = #43l })

let ignorable_product2_array_element_size =
  size_in_bytes ([||] : #(int * t_ignore * bool * t_ignore) array)

let _ = check_ignorable_product2 ~init:(mk_el ())
          ~element_size:ignorable_product2_array_element_size

(* check lack of float32# packing in unboxed product arrays *)
let check_float32u_pair ~(init : #(float32# * float32#)) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    (* 2 because there are two components in the unboxed product *)
    match Sys.backend_type with
    | Native -> assert (n * 2 = (Obj.size (Obj.repr x)))
    | Bytecode | Other _ -> assert (n = Obj.size (Obj.repr x));
    let tag = Obj.tag (Obj.repr x) in
    (* All unboxed product arrays use tag 0, which matches empty array tag *)
    assert (tag = unboxed_product_array_tag);
    if n = 0 then
      Block_checks.check_empty_array_is_uniform ~array_type:"float32#_pair" (Obj.repr x)
    else
      (* Non-empty arrays with no scannable fields are mixed blocks *)
      Block_checks.check_mixed_block_scannable_size ~array_type:"float32#_pair" (Obj.repr x) 0
  in
  List.iter check_one array_sizes_to_check

let float32u_pair_array_element_size =
  size_in_bytes ([||] : #(int * t_ignore * bool * t_ignore) array)

let _ = check_float32u_pair ~init:#(#1.0s, #42.1s)
          ~element_size:float32u_pair_array_element_size

(* check lack of int32# packing in unboxed product arrays *)
let check_int32u_pair ~(init : #(int32# * int32#)) ~element_size =
  let check_one n =
    let x = makearray_dynamic n init in
    (* 2 because there are two components in the unboxed product *)
    match Sys.backend_type with
    | Native -> assert (n * 2 = (Obj.size (Obj.repr x)))
    | Bytecode | Other _ -> assert (n = Obj.size (Obj.repr x));
    let tag = Obj.tag (Obj.repr x) in
    (* All unboxed product arrays use tag 0, which matches empty array tag *)
    assert (tag = unboxed_product_array_tag);
    (* This product has no scannable fields (only check non-empty arrays) *)
    if n > 0 then Block_checks.check_mixed_block_scannable_size ~array_type:"int32#_pair" (Obj.repr x) 0
  in
  List.iter check_one array_sizes_to_check

let int32u_pair_array_element_size =
  size_in_bytes ([||] : #(int * t_ignore * bool * t_ignore) array)

let _ = check_int32u_pair ~init:#(#1l, #42l)
          ~element_size:int32u_pair_array_element_size
