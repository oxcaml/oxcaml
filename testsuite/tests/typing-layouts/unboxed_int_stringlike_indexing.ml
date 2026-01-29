(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 include stdlib_stable;
 {
   native;
 }{
   flags = "-O3";
   native;
 }{
   bytecode;
 }{
   flags = "-extension layouts_alpha";
   native;
 }{
   flags = "-extension layouts_alpha -O3";
   native;
 }{
   flags = "-extension layouts_alpha";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }{
   flags = "-extension layouts_beta -O3";
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }
*)

let lengths = List.init 17 (fun x -> x) @ List.init 17 (fun x -> 300 + x)

type exn += Test_failed

let create_s length =
  String.init length (fun i -> i * 7 mod 256 |> char_of_int)
;;

let create_b length = create_s length |> Bytes.of_string

open struct
  open Bigarray

  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  let bigstring_of_string s =
    let a = Array1.create char c_layout (String.length s) in
    for i = 0 to String.length s - 1 do
      a.{i} <- s.[i]
    done;
    a

  let create_bs length = create_s length |> bigstring_of_string
end

module Tester (Primitives : sig
    type boxed_index
    type boxed_data
    type container

    val create : int -> container
    val generate_data : int -> boxed_data
    val to_index : int -> boxed_index
    val data_equal : boxed_data -> boxed_data -> bool

    type 'a getter := container -> 'a -> boxed_data
    type 'a setter := container -> 'a -> boxed_data -> unit

    val get_reference : int getter
    val get_safe : boxed_index getter
    val get_unsafe : boxed_index getter
    val set_reference : int setter
    val set_safe : boxed_index setter
    val set_unsafe : boxed_index setter
    val extra_bounds_checks : boxed_index list
    val max_index : int
  end) : sig end = struct
  open Primitives

  let make_tester_functions length =
    let for_reference = create length
    and for_safe = create length
    and for_unsafe = create length in
    let check_get_bounds i =
      try
        let _ = get_safe for_safe i in
        assert false
      with
      | Invalid_argument _ -> ()
    in
    let check_set_bounds i x =
      try
        let _ = set_safe for_safe i x in
        assert false
      with
      | Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = to_index i in
      try
        let res = get_reference for_reference i in
        try
          assert (data_equal res (get_safe for_safe test_i));
          assert (data_equal res (get_unsafe for_unsafe test_i))
        with
        | _ -> raise Test_failed
      with
      | Test_failed -> assert false
      | Invalid_argument _ -> check_get_bounds test_i
      | _ ->
        (try
           let _ = get_safe for_safe test_i in
           assert false
         with
         | Invalid_argument _ -> assert false
         | _ -> ())
    in
    let check_set i x =
      let test_i = to_index i in
      try
        set_reference for_reference i x;
        try
          set_safe for_safe test_i x;
          assert (data_equal x (get_reference for_safe i));
          set_unsafe for_unsafe test_i x;
          assert (data_equal x (get_reference for_unsafe i));
          (* Check that we didn't ruin adjacent indices *)
          check_get (i - 1);
          check_get (i + 1)
        with
        | _ -> raise Test_failed
      with
      | Test_failed -> assert false
      | Invalid_argument _ -> check_set_bounds test_i x
      | _ ->
        (try
           set_safe for_safe test_i x;
           assert false
         with
         | Invalid_argument _ -> assert false
         | _ -> ())
    in
    check_get_bounds, check_get, check_set_bounds, check_set
  ;;

  let test length =
    Random.init 1234;
    let check_get_bounds, check_get, check_set_bounds, check_set =
      make_tester_functions length
    in
    for i = -1 to min max_index (length + 1) do
      check_get i;
      check_set i (generate_data i)
    done;
    List.iter
      (fun bound ->
        check_get_bounds bound;
        check_set_bounds bound (generate_data 1))
      extra_bounds_checks
  ;;

  let () = List.iter test lengths
end

module Tester_no_set (Primitives : sig
    type boxed_index
    type boxed_data
    type container

    val create : int -> container
    val generate_data : int -> boxed_data
    val to_index : int -> boxed_index
    val data_equal : boxed_data -> boxed_data -> bool

    type 'a getter := container -> 'a -> boxed_data

    val get_reference : int getter
    val get_safe : boxed_index getter
    val get_unsafe : boxed_index getter
    val extra_bounds_checks : boxed_index list
    val max_index : int
  end) : sig end = struct
  open Primitives

  let make_tester_functions length =
    let for_reference = create length
    and for_safe = create length
    and for_unsafe = create length in
    let check_get_bounds i =
      try
        let _ = get_safe for_safe i in
        assert false
      with
      | Invalid_argument _ -> ()
    in
    let check_get i =
      let test_i = to_index i in
      try
        let res = get_reference for_reference i in
        try
          assert (data_equal res (get_safe for_safe test_i));
          assert (data_equal res (get_unsafe for_unsafe test_i))
        with
        | _ -> raise Test_failed
      with
      | Test_failed -> assert false
      | Invalid_argument _ -> check_get_bounds test_i
      | _ ->
        (try
           let _ = get_safe for_safe test_i in
           assert false
         with
         | Invalid_argument _ -> assert false
         | _ -> ())
    in
    check_get_bounds, check_get
  ;;

  let test length =
    Random.init 1234;
    let check_get_bounds, check_get = make_tester_functions length in
    for i = -1 to min max_index (length + 1) do
      check_get i;
    done;
    List.iter (fun bound -> check_get_bounds bound) extra_bounds_checks
  ;;

  let () = List.iter test lengths
end

module Nativeint_u = Stdlib_upstream_compatible.Nativeint_u
module Int64_u = Stdlib_upstream_compatible.Int64_u
module Int32_u = Stdlib_upstream_compatible.Int32_u
module Int16 = Stdlib_stable.Int16
module Int16_u = Stdlib_stable.Int16_u
module Int8 = Stdlib_stable.Int8
module Int8_u = Stdlib_stable.Int8_u
module Float32 = Stdlib_stable.Float32
module Float32_u = Stdlib_stable.Float32_u

external int64_of_int64 : int64 -> int64 = "%identity"
external int64_to_int64 : int64 -> int64 = "%identity"
external int32_of_int64 : int64 -> int32 = "%int32_of_int64"
external int32_to_int64 : int32 -> int64 = "%int64_of_int32"
external int16_of_int64 : int64 -> int16 = "%int16_of_int64"
external int16_to_int64 : int16 -> int64 = "%int64_of_int16"
external int8_of_int64 : int64 -> int8 = "%int8_of_int64"
external int8_to_int64 : int8 -> int64 = "%int64_of_int8"
external int_of_int64 : int64 -> int = "%int_of_int64"
external int_to_int64 : int -> int64 = "%int64_of_int"
external int_of_int : int -> int = "%identity"
external int_to_int : int -> int = "%identity"


open struct

  type boxed_index = nativeint
  type boxed_data = int8

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int8.zero
      | 1 -> Int8.min_int
      | 2 -> Int8.max_int
      | _ -> Random.int64_in_range
                 ~min:(int8_to_int64 Int8.min_int)
                 ~max:(int8_to_int64 Int8.max_int)
               |> int8_of_int64


  let to_index = Nativeint.of_int
  let data_equal = Int8.equal
  let unbox_index = Nativeint_u.of_nativeint
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int8
        = "%caml_string_geti8"

      external get_safe
        :  string
        -> nativeint#
        -> int8
        = "%caml_string_geti8_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int8
        = "%caml_string_geti8u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int8
        = "%caml_bytes_geti8"

      external get_safe
        :  bytes
        -> nativeint#
        -> int8
        = "%caml_bytes_geti8_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int8
        = "%caml_bytes_geti8u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int8
        -> unit
        = "%caml_bytes_set8"

      external set_safe
        :  bytes
        -> nativeint#
        -> int8
        -> unit
        = "%caml_bytes_set8_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int8
        -> unit
        = "%caml_bytes_set8u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int8
        = "%caml_bigstring_geti8"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int8
        = "%caml_bigstring_geti8_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int8
        = "%caml_bigstring_geti8u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int8
        -> unit
        = "%caml_bigstring_set8"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int8
        -> unit
        = "%caml_bigstring_set8_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int8
        -> unit
        = "%caml_bigstring_set8u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = int

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int.zero
      | 1 -> (Int.(shift_left one) (16 - 1))
      | 2 -> 
        (let shift = 16 - 1 in
        Int.(lognot (shift_left (shift_right (lognot zero) shift) shift)))
      | _ -> Random.int_in_range
                 ~min:Int.zero
                 ~max:(Int.(shift_left one) (16 - 1))
               |> int_of_int


  let to_index = Nativeint.of_int
  let data_equal = Int.equal
  let unbox_index = Nativeint_u.of_nativeint
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int
        = "%caml_string_get16"

      external get_safe
        :  string
        -> nativeint#
        -> int
        = "%caml_string_get16_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int
        = "%caml_string_get16u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int
        = "%caml_bytes_get16"

      external get_safe
        :  bytes
        -> nativeint#
        -> int
        = "%caml_bytes_get16_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int
        = "%caml_bytes_get16u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> nativeint#
        -> int
        -> unit
        = "%caml_bytes_set16_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int
        -> unit
        = "%caml_bytes_set16u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int
        = "%caml_bigstring_get16"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int
        = "%caml_bigstring_get16_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int
        = "%caml_bigstring_get16u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int
        -> unit
        = "%caml_bigstring_set16_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int
        -> unit
        = "%caml_bigstring_set16u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = int16

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int16.zero
      | 1 -> Int16.min_int
      | 2 -> Int16.max_int
      | _ -> Random.int64_in_range
                 ~min:(int16_to_int64 Int16.min_int)
                 ~max:(int16_to_int64 Int16.max_int)
               |> int16_of_int64


  let to_index = Nativeint.of_int
  let data_equal = Int16.equal
  let unbox_index = Nativeint_u.of_nativeint
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int16
        = "%caml_string_geti16"

      external get_safe
        :  string
        -> nativeint#
        -> int16
        = "%caml_string_geti16_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int16
        = "%caml_string_geti16u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int16
        = "%caml_bytes_geti16"

      external get_safe
        :  bytes
        -> nativeint#
        -> int16
        = "%caml_bytes_geti16_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int16
        = "%caml_bytes_geti16u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int16
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> nativeint#
        -> int16
        -> unit
        = "%caml_bytes_set16_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int16
        -> unit
        = "%caml_bytes_set16u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int16
        = "%caml_bigstring_geti16"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int16
        = "%caml_bigstring_geti16_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int16
        = "%caml_bigstring_geti16u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int16
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int16
        -> unit
        = "%caml_bigstring_set16_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int16
        -> unit
        = "%caml_bigstring_set16u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int64_in_range
                 ~min:(int32_to_int64 Int32.min_int)
                 ~max:(int32_to_int64 Int32.max_int)
               |> int32_of_int64


  let to_index = Nativeint.of_int
  let data_equal = Int32.equal
  let unbox_index = Nativeint_u.of_nativeint
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> nativeint#
        -> int32
        = "%caml_string_get32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int32
        = "%caml_string_get32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> nativeint#
        -> int32
        = "%caml_bytes_get32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int32
        = "%caml_bytes_get32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> nativeint#
        -> int32
        -> unit
        = "%caml_bytes_set32_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int32
        -> unit
        = "%caml_bytes_set32u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int32
        = "%caml_bigstring_get32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int32
        = "%caml_bigstring_get32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int32
        -> unit
        = "%caml_bigstring_set32_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int32
        -> unit
        = "%caml_bigstring_set32u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range
                 ~min:(int64_to_int64 Int64.min_int)
                 ~max:(int64_to_int64 Int64.max_int)
               |> int64_of_int64


  let to_index = Nativeint.of_int
  let data_equal = Int64.equal
  let unbox_index = Nativeint_u.of_nativeint
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> nativeint#
        -> int64
        = "%caml_string_get64_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int64
        = "%caml_string_get64u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> nativeint#
        -> int64
        = "%caml_bytes_get64_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int64
        = "%caml_bytes_get64u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> nativeint#
        -> int64
        -> unit
        = "%caml_bytes_set64_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int64
        -> unit
        = "%caml_bytes_set64u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int64
        = "%caml_bigstring_get64_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int64
        = "%caml_bigstring_get64u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int64
        -> unit
        = "%caml_bigstring_set64_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int64
        -> unit
        = "%caml_bigstring_set64u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int64_in_range
                 ~min:(int32_to_int64 Int32.min_int)
                 ~max:(int32_to_int64 Int32.max_int)
               |> int32_of_int64


  let to_index = Nativeint.of_int
  let data_equal = Int32.equal
  let unbox_index = Nativeint_u.of_nativeint
  let unbox_data = Int32_u.of_int32
  let box_data = Int32_u.to_int32
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> nativeint#
        -> int32#
        = "%caml_string_get32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int32#
        = "%caml_string_get32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> nativeint#
        -> int32#
        = "%caml_bytes_get32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int32#
        = "%caml_bytes_get32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> nativeint#
        -> int32#
        -> unit
        = "%caml_bytes_set32#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int32#
        -> unit
        = "%caml_bytes_set32u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int32#
        = "%caml_bigstring_get32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int32#
        = "%caml_bigstring_get32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int32#
        -> unit
        = "%caml_bigstring_set32#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int32#
        -> unit
        = "%caml_bigstring_set32u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range
                 ~min:(int64_to_int64 Int64.min_int)
                 ~max:(int64_to_int64 Int64.max_int)
               |> int64_of_int64


  let to_index = Nativeint.of_int
  let data_equal = Int64.equal
  let unbox_index = Nativeint_u.of_nativeint
  let unbox_data = Int64_u.of_int64
  let box_data = Int64_u.to_int64
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> nativeint#
        -> int64#
        = "%caml_string_get64#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> int64#
        = "%caml_string_get64u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> nativeint#
        -> int64#
        = "%caml_bytes_get64#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> nativeint#
        -> int64#
        -> unit
        = "%caml_bytes_set64#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> int64#
        -> unit
        = "%caml_bytes_set64u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> nativeint#
        -> int64#
        = "%caml_bigstring_get64#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> int64#
        = "%caml_bigstring_get64u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> nativeint#
        -> int64#
        -> unit
        = "%caml_bigstring_set64#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> int64#
        -> unit
        = "%caml_bigstring_set64u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Float32.of_float f

  let to_index = Nativeint.of_int
  let data_equal = Float32.equal
  let unbox_index = Nativeint_u.of_nativeint
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> nativeint#
        -> float32
        = "%caml_string_getf32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> float32
        = "%caml_string_getf32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> nativeint#
        -> float32
        = "%caml_bytes_getf32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> float32
        = "%caml_bytes_getf32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> nativeint#
        -> float32
        -> unit
        = "%caml_bytes_setf32_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> float32
        -> unit
        = "%caml_bytes_setf32u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> nativeint#
        -> float32
        = "%caml_bigstring_getf32_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> float32
        = "%caml_bigstring_getf32u_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> nativeint#
        -> float32
        -> unit
        = "%caml_bigstring_setf32_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> float32
        -> unit
        = "%caml_bigstring_setf32u_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = nativeint
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Float32.of_float f

  let to_index = Nativeint.of_int
  let data_equal = Float32.equal
  let unbox_index = Nativeint_u.of_nativeint
  let unbox_data = Float32_u.of_float32
  let box_data = Float32_u.to_float32
  let extra_bounds_checks = Nativeint.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> nativeint#
        -> float32#
        = "%caml_string_getf32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> nativeint#
        -> float32#
        = "%caml_string_getf32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> nativeint#
        -> float32#
        = "%caml_bytes_getf32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> nativeint#
        -> float32#
        = "%caml_bytes_getf32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> nativeint#
        -> float32#
        -> unit
        = "%caml_bytes_setf32#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> nativeint#
        -> float32#
        -> unit
        = "%caml_bytes_setf32u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> nativeint#
        -> float32#
        = "%caml_bigstring_getf32#_indexed_by_nativeint#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> nativeint#
        -> float32#
        = "%caml_bigstring_getf32u#_indexed_by_nativeint#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> nativeint#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32#_indexed_by_nativeint#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> nativeint#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32u#_indexed_by_nativeint#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int8
  type boxed_data = int8

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int8.zero
      | 1 -> Int8.min_int
      | 2 -> Int8.max_int
      | _ -> Random.int64_in_range
                 ~min:(int8_to_int64 Int8.min_int)
                 ~max:(int8_to_int64 Int8.max_int)
               |> int8_of_int64


  let to_index = Int8.of_int
  let data_equal = Int8.equal
  let unbox_index = Int8_u.of_int8
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int8.[ min_int; add min_int one; sub zero one ]
  let max_index = Int8.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int8
        = "%caml_string_geti8"

      external get_safe
        :  string
        -> int8#
        -> int8
        = "%caml_string_geti8_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int8#
        -> int8
        = "%caml_string_geti8u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int8
        = "%caml_bytes_geti8"

      external get_safe
        :  bytes
        -> int8#
        -> int8
        = "%caml_bytes_geti8_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int8#
        -> int8
        = "%caml_bytes_geti8u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int8
        -> unit
        = "%caml_bytes_set8"

      external set_safe
        :  bytes
        -> int8#
        -> int8
        -> unit
        = "%caml_bytes_set8_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int8#
        -> int8
        -> unit
        = "%caml_bytes_set8u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int8
        = "%caml_bigstring_geti8"

      external get_safe
        :  bigstring
        -> int8#
        -> int8
        = "%caml_bigstring_geti8_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int8#
        -> int8
        = "%caml_bigstring_geti8u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int8
        -> unit
        = "%caml_bigstring_set8"

      external set_safe
        :  bigstring
        -> int8#
        -> int8
        -> unit
        = "%caml_bigstring_set8_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int8#
        -> int8
        -> unit
        = "%caml_bigstring_set8u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int8
  type boxed_data = int

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int.zero
      | 1 -> (Int.(shift_left one) (16 - 1))
      | 2 -> 
        (let shift = 16 - 1 in
        Int.(lognot (shift_left (shift_right (lognot zero) shift) shift)))
      | _ -> Random.int_in_range
                 ~min:Int.zero
                 ~max:(Int.(shift_left one) (16 - 1))
               |> int_of_int


  let to_index = Int8.of_int
  let data_equal = Int.equal
  let unbox_index = Int8_u.of_int8
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int8.[ min_int; add min_int one; sub zero one ]
  let max_index = Int8.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int
        = "%caml_string_get16"

      external get_safe
        :  string
        -> int8#
        -> int
        = "%caml_string_get16_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int8#
        -> int
        = "%caml_string_get16u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int
        = "%caml_bytes_get16"

      external get_safe
        :  bytes
        -> int8#
        -> int
        = "%caml_bytes_get16_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int8#
        -> int
        = "%caml_bytes_get16u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> int8#
        -> int
        -> unit
        = "%caml_bytes_set16_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int8#
        -> int
        -> unit
        = "%caml_bytes_set16u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int
        = "%caml_bigstring_get16"

      external get_safe
        :  bigstring
        -> int8#
        -> int
        = "%caml_bigstring_get16_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int8#
        -> int
        = "%caml_bigstring_get16u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> int8#
        -> int
        -> unit
        = "%caml_bigstring_set16_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int8#
        -> int
        -> unit
        = "%caml_bigstring_set16u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int8
  type boxed_data = int16

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int16.zero
      | 1 -> Int16.min_int
      | 2 -> Int16.max_int
      | _ -> Random.int64_in_range
                 ~min:(int16_to_int64 Int16.min_int)
                 ~max:(int16_to_int64 Int16.max_int)
               |> int16_of_int64


  let to_index = Int8.of_int
  let data_equal = Int16.equal
  let unbox_index = Int8_u.of_int8
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int8.[ min_int; add min_int one; sub zero one ]
  let max_index = Int8.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int16
        = "%caml_string_geti16"

      external get_safe
        :  string
        -> int8#
        -> int16
        = "%caml_string_geti16_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int8#
        -> int16
        = "%caml_string_geti16u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int16
        = "%caml_bytes_geti16"

      external get_safe
        :  bytes
        -> int8#
        -> int16
        = "%caml_bytes_geti16_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int8#
        -> int16
        = "%caml_bytes_geti16u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int16
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> int8#
        -> int16
        -> unit
        = "%caml_bytes_set16_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int8#
        -> int16
        -> unit
        = "%caml_bytes_set16u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int16
        = "%caml_bigstring_geti16"

      external get_safe
        :  bigstring
        -> int8#
        -> int16
        = "%caml_bigstring_geti16_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int8#
        -> int16
        = "%caml_bigstring_geti16u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int16
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> int8#
        -> int16
        -> unit
        = "%caml_bigstring_set16_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int8#
        -> int16
        -> unit
        = "%caml_bigstring_set16u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int8
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int64_in_range
                 ~min:(int32_to_int64 Int32.min_int)
                 ~max:(int32_to_int64 Int32.max_int)
               |> int32_of_int64


  let to_index = Int8.of_int
  let data_equal = Int32.equal
  let unbox_index = Int8_u.of_int8
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int8.[ min_int; add min_int one; sub zero one ]
  let max_index = Int8.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int8#
        -> int32
        = "%caml_string_get32_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int8#
        -> int32
        = "%caml_string_get32u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int8#
        -> int32
        = "%caml_bytes_get32_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int8#
        -> int32
        = "%caml_bytes_get32u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int8#
        -> int32
        -> unit
        = "%caml_bytes_set32_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int8#
        -> int32
        -> unit
        = "%caml_bytes_set32u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int8#
        -> int32
        = "%caml_bigstring_get32_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int8#
        -> int32
        = "%caml_bigstring_get32u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int8#
        -> int32
        -> unit
        = "%caml_bigstring_set32_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int8#
        -> int32
        -> unit
        = "%caml_bigstring_set32u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int8
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range
                 ~min:(int64_to_int64 Int64.min_int)
                 ~max:(int64_to_int64 Int64.max_int)
               |> int64_of_int64


  let to_index = Int8.of_int
  let data_equal = Int64.equal
  let unbox_index = Int8_u.of_int8
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int8.[ min_int; add min_int one; sub zero one ]
  let max_index = Int8.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int8#
        -> int64
        = "%caml_string_get64_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int8#
        -> int64
        = "%caml_string_get64u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int8#
        -> int64
        = "%caml_bytes_get64_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int8#
        -> int64
        = "%caml_bytes_get64u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int8#
        -> int64
        -> unit
        = "%caml_bytes_set64_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int8#
        -> int64
        -> unit
        = "%caml_bytes_set64u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int8#
        -> int64
        = "%caml_bigstring_get64_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int8#
        -> int64
        = "%caml_bigstring_get64u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int8#
        -> int64
        -> unit
        = "%caml_bigstring_set64_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int8#
        -> int64
        -> unit
        = "%caml_bigstring_set64u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int8
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int64_in_range
                 ~min:(int32_to_int64 Int32.min_int)
                 ~max:(int32_to_int64 Int32.max_int)
               |> int32_of_int64


  let to_index = Int8.of_int
  let data_equal = Int32.equal
  let unbox_index = Int8_u.of_int8
  let unbox_data = Int32_u.of_int32
  let box_data = Int32_u.to_int32
  let extra_bounds_checks = Int8.[ min_int; add min_int one; sub zero one ]
  let max_index = Int8.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int8#
        -> int32#
        = "%caml_string_get32#_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int8#
        -> int32#
        = "%caml_string_get32u#_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int8#
        -> int32#
        = "%caml_bytes_get32#_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int8#
        -> int32#
        = "%caml_bytes_get32u#_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int8#
        -> int32#
        -> unit
        = "%caml_bytes_set32#_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int8#
        -> int32#
        -> unit
        = "%caml_bytes_set32u#_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int8#
        -> int32#
        = "%caml_bigstring_get32#_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int8#
        -> int32#
        = "%caml_bigstring_get32u#_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int8#
        -> int32#
        -> unit
        = "%caml_bigstring_set32#_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int8#
        -> int32#
        -> unit
        = "%caml_bigstring_set32u#_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int8
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range
                 ~min:(int64_to_int64 Int64.min_int)
                 ~max:(int64_to_int64 Int64.max_int)
               |> int64_of_int64


  let to_index = Int8.of_int
  let data_equal = Int64.equal
  let unbox_index = Int8_u.of_int8
  let unbox_data = Int64_u.of_int64
  let box_data = Int64_u.to_int64
  let extra_bounds_checks = Int8.[ min_int; add min_int one; sub zero one ]
  let max_index = Int8.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int8#
        -> int64#
        = "%caml_string_get64#_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int8#
        -> int64#
        = "%caml_string_get64u#_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int8#
        -> int64#
        = "%caml_bytes_get64#_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int8#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int8#
        -> int64#
        -> unit
        = "%caml_bytes_set64#_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int8#
        -> int64#
        -> unit
        = "%caml_bytes_set64u#_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int8#
        -> int64#
        = "%caml_bigstring_get64#_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int8#
        -> int64#
        = "%caml_bigstring_get64u#_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int8#
        -> int64#
        -> unit
        = "%caml_bigstring_set64#_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int8#
        -> int64#
        -> unit
        = "%caml_bigstring_set64u#_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int8
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Float32.of_float f

  let to_index = Int8.of_int
  let data_equal = Float32.equal
  let unbox_index = Int8_u.of_int8
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int8.[ min_int; add min_int one; sub zero one ]
  let max_index = Int8.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int8#
        -> float32
        = "%caml_string_getf32_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int8#
        -> float32
        = "%caml_string_getf32u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int8#
        -> float32
        = "%caml_bytes_getf32_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int8#
        -> float32
        = "%caml_bytes_getf32u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int8#
        -> float32
        -> unit
        = "%caml_bytes_setf32_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int8#
        -> float32
        -> unit
        = "%caml_bytes_setf32u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int8#
        -> float32
        = "%caml_bigstring_getf32_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int8#
        -> float32
        = "%caml_bigstring_getf32u_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int8#
        -> float32
        -> unit
        = "%caml_bigstring_setf32_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int8#
        -> float32
        -> unit
        = "%caml_bigstring_setf32u_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int8
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Float32.of_float f

  let to_index = Int8.of_int
  let data_equal = Float32.equal
  let unbox_index = Int8_u.of_int8
  let unbox_data = Float32_u.of_float32
  let box_data = Float32_u.to_float32
  let extra_bounds_checks = Int8.[ min_int; add min_int one; sub zero one ]
  let max_index = Int8.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int8#
        -> float32#
        = "%caml_string_getf32#_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int8#
        -> float32#
        = "%caml_string_getf32u#_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int8#
        -> float32#
        = "%caml_bytes_getf32#_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int8#
        -> float32#
        = "%caml_bytes_getf32u#_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int8#
        -> float32#
        -> unit
        = "%caml_bytes_setf32#_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int8#
        -> float32#
        -> unit
        = "%caml_bytes_setf32u#_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int8#
        -> float32#
        = "%caml_bigstring_getf32#_indexed_by_int8#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int8#
        -> float32#
        = "%caml_bigstring_getf32u#_indexed_by_int8#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int8#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32#_indexed_by_int8#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int8#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32u#_indexed_by_int8#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int16
  type boxed_data = int8

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int8.zero
      | 1 -> Int8.min_int
      | 2 -> Int8.max_int
      | _ -> Random.int64_in_range
                 ~min:(int8_to_int64 Int8.min_int)
                 ~max:(int8_to_int64 Int8.max_int)
               |> int8_of_int64


  let to_index = Int16.of_int
  let data_equal = Int8.equal
  let unbox_index = Int16_u.of_int16
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int16.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int16.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int8
        = "%caml_string_geti8"

      external get_safe
        :  string
        -> int16#
        -> int8
        = "%caml_string_geti8_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int16#
        -> int8
        = "%caml_string_geti8u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int8
        = "%caml_bytes_geti8"

      external get_safe
        :  bytes
        -> int16#
        -> int8
        = "%caml_bytes_geti8_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int16#
        -> int8
        = "%caml_bytes_geti8u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int8
        -> unit
        = "%caml_bytes_set8"

      external set_safe
        :  bytes
        -> int16#
        -> int8
        -> unit
        = "%caml_bytes_set8_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int16#
        -> int8
        -> unit
        = "%caml_bytes_set8u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int8
        = "%caml_bigstring_geti8"

      external get_safe
        :  bigstring
        -> int16#
        -> int8
        = "%caml_bigstring_geti8_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int16#
        -> int8
        = "%caml_bigstring_geti8u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int8
        -> unit
        = "%caml_bigstring_set8"

      external set_safe
        :  bigstring
        -> int16#
        -> int8
        -> unit
        = "%caml_bigstring_set8_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int16#
        -> int8
        -> unit
        = "%caml_bigstring_set8u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int16
  type boxed_data = int

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int.zero
      | 1 -> (Int.(shift_left one) (16 - 1))
      | 2 -> 
        (let shift = 16 - 1 in
        Int.(lognot (shift_left (shift_right (lognot zero) shift) shift)))
      | _ -> Random.int_in_range
                 ~min:Int.zero
                 ~max:(Int.(shift_left one) (16 - 1))
               |> int_of_int


  let to_index = Int16.of_int
  let data_equal = Int.equal
  let unbox_index = Int16_u.of_int16
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int16.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int16.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int
        = "%caml_string_get16"

      external get_safe
        :  string
        -> int16#
        -> int
        = "%caml_string_get16_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int16#
        -> int
        = "%caml_string_get16u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int
        = "%caml_bytes_get16"

      external get_safe
        :  bytes
        -> int16#
        -> int
        = "%caml_bytes_get16_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int16#
        -> int
        = "%caml_bytes_get16u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> int16#
        -> int
        -> unit
        = "%caml_bytes_set16_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int16#
        -> int
        -> unit
        = "%caml_bytes_set16u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int
        = "%caml_bigstring_get16"

      external get_safe
        :  bigstring
        -> int16#
        -> int
        = "%caml_bigstring_get16_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int16#
        -> int
        = "%caml_bigstring_get16u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> int16#
        -> int
        -> unit
        = "%caml_bigstring_set16_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int16#
        -> int
        -> unit
        = "%caml_bigstring_set16u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int16
  type boxed_data = int16

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int16.zero
      | 1 -> Int16.min_int
      | 2 -> Int16.max_int
      | _ -> Random.int64_in_range
                 ~min:(int16_to_int64 Int16.min_int)
                 ~max:(int16_to_int64 Int16.max_int)
               |> int16_of_int64


  let to_index = Int16.of_int
  let data_equal = Int16.equal
  let unbox_index = Int16_u.of_int16
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int16.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int16.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int16
        = "%caml_string_geti16"

      external get_safe
        :  string
        -> int16#
        -> int16
        = "%caml_string_geti16_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int16#
        -> int16
        = "%caml_string_geti16u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int16
        = "%caml_bytes_geti16"

      external get_safe
        :  bytes
        -> int16#
        -> int16
        = "%caml_bytes_geti16_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int16#
        -> int16
        = "%caml_bytes_geti16u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int16
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> int16#
        -> int16
        -> unit
        = "%caml_bytes_set16_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int16#
        -> int16
        -> unit
        = "%caml_bytes_set16u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int16
        = "%caml_bigstring_geti16"

      external get_safe
        :  bigstring
        -> int16#
        -> int16
        = "%caml_bigstring_geti16_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int16#
        -> int16
        = "%caml_bigstring_geti16u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int16
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> int16#
        -> int16
        -> unit
        = "%caml_bigstring_set16_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int16#
        -> int16
        -> unit
        = "%caml_bigstring_set16u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int16
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int64_in_range
                 ~min:(int32_to_int64 Int32.min_int)
                 ~max:(int32_to_int64 Int32.max_int)
               |> int32_of_int64


  let to_index = Int16.of_int
  let data_equal = Int32.equal
  let unbox_index = Int16_u.of_int16
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int16.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int16.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int16#
        -> int32
        = "%caml_string_get32_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int16#
        -> int32
        = "%caml_string_get32u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int16#
        -> int32
        = "%caml_bytes_get32_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int16#
        -> int32
        = "%caml_bytes_get32u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int16#
        -> int32
        -> unit
        = "%caml_bytes_set32_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int16#
        -> int32
        -> unit
        = "%caml_bytes_set32u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int16#
        -> int32
        = "%caml_bigstring_get32_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int16#
        -> int32
        = "%caml_bigstring_get32u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int16#
        -> int32
        -> unit
        = "%caml_bigstring_set32_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int16#
        -> int32
        -> unit
        = "%caml_bigstring_set32u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int16
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range
                 ~min:(int64_to_int64 Int64.min_int)
                 ~max:(int64_to_int64 Int64.max_int)
               |> int64_of_int64


  let to_index = Int16.of_int
  let data_equal = Int64.equal
  let unbox_index = Int16_u.of_int16
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int16.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int16.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int16#
        -> int64
        = "%caml_string_get64_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int16#
        -> int64
        = "%caml_string_get64u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int16#
        -> int64
        = "%caml_bytes_get64_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int16#
        -> int64
        = "%caml_bytes_get64u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int16#
        -> int64
        -> unit
        = "%caml_bytes_set64_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int16#
        -> int64
        -> unit
        = "%caml_bytes_set64u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int16#
        -> int64
        = "%caml_bigstring_get64_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int16#
        -> int64
        = "%caml_bigstring_get64u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int16#
        -> int64
        -> unit
        = "%caml_bigstring_set64_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int16#
        -> int64
        -> unit
        = "%caml_bigstring_set64u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int16
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int64_in_range
                 ~min:(int32_to_int64 Int32.min_int)
                 ~max:(int32_to_int64 Int32.max_int)
               |> int32_of_int64


  let to_index = Int16.of_int
  let data_equal = Int32.equal
  let unbox_index = Int16_u.of_int16
  let unbox_data = Int32_u.of_int32
  let box_data = Int32_u.to_int32
  let extra_bounds_checks = Int16.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int16.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int16#
        -> int32#
        = "%caml_string_get32#_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int16#
        -> int32#
        = "%caml_string_get32u#_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int16#
        -> int32#
        = "%caml_bytes_get32#_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int16#
        -> int32#
        = "%caml_bytes_get32u#_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int16#
        -> int32#
        -> unit
        = "%caml_bytes_set32#_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int16#
        -> int32#
        -> unit
        = "%caml_bytes_set32u#_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int16#
        -> int32#
        = "%caml_bigstring_get32#_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int16#
        -> int32#
        = "%caml_bigstring_get32u#_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int16#
        -> int32#
        -> unit
        = "%caml_bigstring_set32#_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int16#
        -> int32#
        -> unit
        = "%caml_bigstring_set32u#_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int16
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range
                 ~min:(int64_to_int64 Int64.min_int)
                 ~max:(int64_to_int64 Int64.max_int)
               |> int64_of_int64


  let to_index = Int16.of_int
  let data_equal = Int64.equal
  let unbox_index = Int16_u.of_int16
  let unbox_data = Int64_u.of_int64
  let box_data = Int64_u.to_int64
  let extra_bounds_checks = Int16.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int16.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int16#
        -> int64#
        = "%caml_string_get64#_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int16#
        -> int64#
        = "%caml_string_get64u#_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int16#
        -> int64#
        = "%caml_bytes_get64#_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int16#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int16#
        -> int64#
        -> unit
        = "%caml_bytes_set64#_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int16#
        -> int64#
        -> unit
        = "%caml_bytes_set64u#_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int16#
        -> int64#
        = "%caml_bigstring_get64#_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int16#
        -> int64#
        = "%caml_bigstring_get64u#_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int16#
        -> int64#
        -> unit
        = "%caml_bigstring_set64#_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int16#
        -> int64#
        -> unit
        = "%caml_bigstring_set64u#_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int16
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Float32.of_float f

  let to_index = Int16.of_int
  let data_equal = Float32.equal
  let unbox_index = Int16_u.of_int16
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int16.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int16.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int16#
        -> float32
        = "%caml_string_getf32_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int16#
        -> float32
        = "%caml_string_getf32u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int16#
        -> float32
        = "%caml_bytes_getf32_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int16#
        -> float32
        = "%caml_bytes_getf32u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int16#
        -> float32
        -> unit
        = "%caml_bytes_setf32_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int16#
        -> float32
        -> unit
        = "%caml_bytes_setf32u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int16#
        -> float32
        = "%caml_bigstring_getf32_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int16#
        -> float32
        = "%caml_bigstring_getf32u_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int16#
        -> float32
        -> unit
        = "%caml_bigstring_setf32_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int16#
        -> float32
        -> unit
        = "%caml_bigstring_setf32u_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int16
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Float32.of_float f

  let to_index = Int16.of_int
  let data_equal = Float32.equal
  let unbox_index = Int16_u.of_int16
  let unbox_data = Float32_u.of_float32
  let box_data = Float32_u.to_float32
  let extra_bounds_checks = Int16.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int16.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int16#
        -> float32#
        = "%caml_string_getf32#_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int16#
        -> float32#
        = "%caml_string_getf32u#_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int16#
        -> float32#
        = "%caml_bytes_getf32#_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int16#
        -> float32#
        = "%caml_bytes_getf32u#_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int16#
        -> float32#
        -> unit
        = "%caml_bytes_setf32#_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int16#
        -> float32#
        -> unit
        = "%caml_bytes_setf32u#_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int16#
        -> float32#
        = "%caml_bigstring_getf32#_indexed_by_int16#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int16#
        -> float32#
        = "%caml_bigstring_getf32u#_indexed_by_int16#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int16#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32#_indexed_by_int16#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int16#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32u#_indexed_by_int16#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int8

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int8.zero
      | 1 -> Int8.min_int
      | 2 -> Int8.max_int
      | _ -> Random.int64_in_range
                 ~min:(int8_to_int64 Int8.min_int)
                 ~max:(int8_to_int64 Int8.max_int)
               |> int8_of_int64


  let to_index = Int32.of_int
  let data_equal = Int8.equal
  let unbox_index = Int32_u.of_int32
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int32.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int8
        = "%caml_string_geti8"

      external get_safe
        :  string
        -> int32#
        -> int8
        = "%caml_string_geti8_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int8
        = "%caml_string_geti8u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int8
        = "%caml_bytes_geti8"

      external get_safe
        :  bytes
        -> int32#
        -> int8
        = "%caml_bytes_geti8_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int8
        = "%caml_bytes_geti8u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int8
        -> unit
        = "%caml_bytes_set8"

      external set_safe
        :  bytes
        -> int32#
        -> int8
        -> unit
        = "%caml_bytes_set8_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int8
        -> unit
        = "%caml_bytes_set8u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int8
        = "%caml_bigstring_geti8"

      external get_safe
        :  bigstring
        -> int32#
        -> int8
        = "%caml_bigstring_geti8_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int8
        = "%caml_bigstring_geti8u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int8
        -> unit
        = "%caml_bigstring_set8"

      external set_safe
        :  bigstring
        -> int32#
        -> int8
        -> unit
        = "%caml_bigstring_set8_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int8
        -> unit
        = "%caml_bigstring_set8u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int.zero
      | 1 -> (Int.(shift_left one) (16 - 1))
      | 2 -> 
        (let shift = 16 - 1 in
        Int.(lognot (shift_left (shift_right (lognot zero) shift) shift)))
      | _ -> Random.int_in_range
                 ~min:Int.zero
                 ~max:(Int.(shift_left one) (16 - 1))
               |> int_of_int


  let to_index = Int32.of_int
  let data_equal = Int.equal
  let unbox_index = Int32_u.of_int32
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int32.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int
        = "%caml_string_get16"

      external get_safe
        :  string
        -> int32#
        -> int
        = "%caml_string_get16_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int
        = "%caml_string_get16u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int
        = "%caml_bytes_get16"

      external get_safe
        :  bytes
        -> int32#
        -> int
        = "%caml_bytes_get16_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int
        = "%caml_bytes_get16u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> int32#
        -> int
        -> unit
        = "%caml_bytes_set16_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int
        -> unit
        = "%caml_bytes_set16u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int
        = "%caml_bigstring_get16"

      external get_safe
        :  bigstring
        -> int32#
        -> int
        = "%caml_bigstring_get16_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int
        = "%caml_bigstring_get16u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> int32#
        -> int
        -> unit
        = "%caml_bigstring_set16_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int
        -> unit
        = "%caml_bigstring_set16u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int16

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int16.zero
      | 1 -> Int16.min_int
      | 2 -> Int16.max_int
      | _ -> Random.int64_in_range
                 ~min:(int16_to_int64 Int16.min_int)
                 ~max:(int16_to_int64 Int16.max_int)
               |> int16_of_int64


  let to_index = Int32.of_int
  let data_equal = Int16.equal
  let unbox_index = Int32_u.of_int32
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int32.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int16
        = "%caml_string_geti16"

      external get_safe
        :  string
        -> int32#
        -> int16
        = "%caml_string_geti16_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int16
        = "%caml_string_geti16u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int16
        = "%caml_bytes_geti16"

      external get_safe
        :  bytes
        -> int32#
        -> int16
        = "%caml_bytes_geti16_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int16
        = "%caml_bytes_geti16u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int16
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> int32#
        -> int16
        -> unit
        = "%caml_bytes_set16_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int16
        -> unit
        = "%caml_bytes_set16u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int16
        = "%caml_bigstring_geti16"

      external get_safe
        :  bigstring
        -> int32#
        -> int16
        = "%caml_bigstring_geti16_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int16
        = "%caml_bigstring_geti16u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int16
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> int32#
        -> int16
        -> unit
        = "%caml_bigstring_set16_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int16
        -> unit
        = "%caml_bigstring_set16u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int64_in_range
                 ~min:(int32_to_int64 Int32.min_int)
                 ~max:(int32_to_int64 Int32.max_int)
               |> int32_of_int64


  let to_index = Int32.of_int
  let data_equal = Int32.equal
  let unbox_index = Int32_u.of_int32
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int32.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int32#
        -> int32
        = "%caml_string_get32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int32
        = "%caml_string_get32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int32#
        -> int32
        = "%caml_bytes_get32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int32
        = "%caml_bytes_get32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int32#
        -> int32
        -> unit
        = "%caml_bytes_set32_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int32
        -> unit
        = "%caml_bytes_set32u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int32#
        -> int32
        = "%caml_bigstring_get32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int32
        = "%caml_bigstring_get32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int32#
        -> int32
        -> unit
        = "%caml_bigstring_set32_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int32
        -> unit
        = "%caml_bigstring_set32u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range
                 ~min:(int64_to_int64 Int64.min_int)
                 ~max:(int64_to_int64 Int64.max_int)
               |> int64_of_int64


  let to_index = Int32.of_int
  let data_equal = Int64.equal
  let unbox_index = Int32_u.of_int32
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int32.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int32#
        -> int64
        = "%caml_string_get64_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int64
        = "%caml_string_get64u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int32#
        -> int64
        = "%caml_bytes_get64_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int64
        = "%caml_bytes_get64u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int32#
        -> int64
        -> unit
        = "%caml_bytes_set64_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int64
        -> unit
        = "%caml_bytes_set64u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int32#
        -> int64
        = "%caml_bigstring_get64_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int64
        = "%caml_bigstring_get64u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int32#
        -> int64
        -> unit
        = "%caml_bigstring_set64_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int64
        -> unit
        = "%caml_bigstring_set64u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int64_in_range
                 ~min:(int32_to_int64 Int32.min_int)
                 ~max:(int32_to_int64 Int32.max_int)
               |> int32_of_int64


  let to_index = Int32.of_int
  let data_equal = Int32.equal
  let unbox_index = Int32_u.of_int32
  let unbox_data = Int32_u.of_int32
  let box_data = Int32_u.to_int32
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int32.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int32#
        -> int32#
        = "%caml_string_get32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int32#
        = "%caml_string_get32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int32#
        -> int32#
        = "%caml_bytes_get32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int32#
        = "%caml_bytes_get32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int32#
        -> int32#
        -> unit
        = "%caml_bytes_set32#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int32#
        -> unit
        = "%caml_bytes_set32u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int32#
        -> int32#
        = "%caml_bigstring_get32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int32#
        = "%caml_bigstring_get32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int32#
        -> int32#
        -> unit
        = "%caml_bigstring_set32#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int32#
        -> unit
        = "%caml_bigstring_set32u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range
                 ~min:(int64_to_int64 Int64.min_int)
                 ~max:(int64_to_int64 Int64.max_int)
               |> int64_of_int64


  let to_index = Int32.of_int
  let data_equal = Int64.equal
  let unbox_index = Int32_u.of_int32
  let unbox_data = Int64_u.of_int64
  let box_data = Int64_u.to_int64
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int32.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int32#
        -> int64#
        = "%caml_string_get64#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> int64#
        = "%caml_string_get64u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int32#
        -> int64#
        = "%caml_bytes_get64#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int32#
        -> int64#
        -> unit
        = "%caml_bytes_set64#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> int64#
        -> unit
        = "%caml_bytes_set64u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int32#
        -> int64#
        = "%caml_bigstring_get64#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> int64#
        = "%caml_bigstring_get64u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int32#
        -> int64#
        -> unit
        = "%caml_bigstring_set64#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> int64#
        -> unit
        = "%caml_bigstring_set64u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Float32.of_float f

  let to_index = Int32.of_int
  let data_equal = Float32.equal
  let unbox_index = Int32_u.of_int32
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int32.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int32#
        -> float32
        = "%caml_string_getf32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> float32
        = "%caml_string_getf32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int32#
        -> float32
        = "%caml_bytes_getf32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> float32
        = "%caml_bytes_getf32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int32#
        -> float32
        -> unit
        = "%caml_bytes_setf32_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> float32
        -> unit
        = "%caml_bytes_setf32u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int32#
        -> float32
        = "%caml_bigstring_getf32_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> float32
        = "%caml_bigstring_getf32u_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int32#
        -> float32
        -> unit
        = "%caml_bigstring_setf32_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> float32
        -> unit
        = "%caml_bigstring_setf32u_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int32
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Float32.of_float f

  let to_index = Int32.of_int
  let data_equal = Float32.equal
  let unbox_index = Int32_u.of_int32
  let unbox_data = Float32_u.of_float32
  let box_data = Float32_u.to_float32
  let extra_bounds_checks = Int32.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int32.(to_int max_int) - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int32#
        -> float32#
        = "%caml_string_getf32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int32#
        -> float32#
        = "%caml_string_getf32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int32#
        -> float32#
        = "%caml_bytes_getf32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int32#
        -> float32#
        = "%caml_bytes_getf32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int32#
        -> float32#
        -> unit
        = "%caml_bytes_setf32#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int32#
        -> float32#
        -> unit
        = "%caml_bytes_setf32u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int32#
        -> float32#
        = "%caml_bigstring_getf32#_indexed_by_int32#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int32#
        -> float32#
        = "%caml_bigstring_getf32u#_indexed_by_int32#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int32#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32#_indexed_by_int32#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int32#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32u#_indexed_by_int32#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int8

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int8.zero
      | 1 -> Int8.min_int
      | 2 -> Int8.max_int
      | _ -> Random.int64_in_range
                 ~min:(int8_to_int64 Int8.min_int)
                 ~max:(int8_to_int64 Int8.max_int)
               |> int8_of_int64


  let to_index = Int64.of_int
  let data_equal = Int8.equal
  let unbox_index = Int64_u.of_int64
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int8
        = "%caml_string_geti8"

      external get_safe
        :  string
        -> int64#
        -> int8
        = "%caml_string_geti8_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int8
        = "%caml_string_geti8u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int8
        = "%caml_bytes_geti8"

      external get_safe
        :  bytes
        -> int64#
        -> int8
        = "%caml_bytes_geti8_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int8
        = "%caml_bytes_geti8u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int8
        -> unit
        = "%caml_bytes_set8"

      external set_safe
        :  bytes
        -> int64#
        -> int8
        -> unit
        = "%caml_bytes_set8_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int8
        -> unit
        = "%caml_bytes_set8u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int8
        = "%caml_bigstring_geti8"

      external get_safe
        :  bigstring
        -> int64#
        -> int8
        = "%caml_bigstring_geti8_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int8
        = "%caml_bigstring_geti8u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int8
        -> unit
        = "%caml_bigstring_set8"

      external set_safe
        :  bigstring
        -> int64#
        -> int8
        -> unit
        = "%caml_bigstring_set8_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int8
        -> unit
        = "%caml_bigstring_set8u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int.zero
      | 1 -> (Int.(shift_left one) (16 - 1))
      | 2 -> 
        (let shift = 16 - 1 in
        Int.(lognot (shift_left (shift_right (lognot zero) shift) shift)))
      | _ -> Random.int_in_range
                 ~min:Int.zero
                 ~max:(Int.(shift_left one) (16 - 1))
               |> int_of_int


  let to_index = Int64.of_int
  let data_equal = Int.equal
  let unbox_index = Int64_u.of_int64
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int
        = "%caml_string_get16"

      external get_safe
        :  string
        -> int64#
        -> int
        = "%caml_string_get16_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int
        = "%caml_string_get16u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int
        = "%caml_bytes_get16"

      external get_safe
        :  bytes
        -> int64#
        -> int
        = "%caml_bytes_get16_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int
        = "%caml_bytes_get16u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> int64#
        -> int
        -> unit
        = "%caml_bytes_set16_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int
        -> unit
        = "%caml_bytes_set16u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int
        = "%caml_bigstring_get16"

      external get_safe
        :  bigstring
        -> int64#
        -> int
        = "%caml_bigstring_get16_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int
        = "%caml_bigstring_get16u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> int64#
        -> int
        -> unit
        = "%caml_bigstring_set16_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int
        -> unit
        = "%caml_bigstring_set16u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int16

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int16.zero
      | 1 -> Int16.min_int
      | 2 -> Int16.max_int
      | _ -> Random.int64_in_range
                 ~min:(int16_to_int64 Int16.min_int)
                 ~max:(int16_to_int64 Int16.max_int)
               |> int16_of_int64


  let to_index = Int64.of_int
  let data_equal = Int16.equal
  let unbox_index = Int64_u.of_int64
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int16
        = "%caml_string_geti16"

      external get_safe
        :  string
        -> int64#
        -> int16
        = "%caml_string_geti16_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int16
        = "%caml_string_geti16u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int16
        = "%caml_bytes_geti16"

      external get_safe
        :  bytes
        -> int64#
        -> int16
        = "%caml_bytes_geti16_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int16
        = "%caml_bytes_geti16u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int16
        -> unit
        = "%caml_bytes_set16"

      external set_safe
        :  bytes
        -> int64#
        -> int16
        -> unit
        = "%caml_bytes_set16_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int16
        -> unit
        = "%caml_bytes_set16u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int16
        = "%caml_bigstring_geti16"

      external get_safe
        :  bigstring
        -> int64#
        -> int16
        = "%caml_bigstring_geti16_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int16
        = "%caml_bigstring_geti16u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int16
        -> unit
        = "%caml_bigstring_set16"

      external set_safe
        :  bigstring
        -> int64#
        -> int16
        -> unit
        = "%caml_bigstring_set16_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int16
        -> unit
        = "%caml_bigstring_set16u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int64_in_range
                 ~min:(int32_to_int64 Int32.min_int)
                 ~max:(int32_to_int64 Int32.max_int)
               |> int32_of_int64


  let to_index = Int64.of_int
  let data_equal = Int32.equal
  let unbox_index = Int64_u.of_int64
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int64#
        -> int32
        = "%caml_string_get32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int32
        = "%caml_string_get32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int64#
        -> int32
        = "%caml_bytes_get32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int32
        = "%caml_bytes_get32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int64#
        -> int32
        -> unit
        = "%caml_bytes_set32_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int32
        -> unit
        = "%caml_bytes_set32u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int64#
        -> int32
        = "%caml_bigstring_get32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int32
        = "%caml_bigstring_get32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int64#
        -> int32
        -> unit
        = "%caml_bigstring_set32_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int32
        -> unit
        = "%caml_bigstring_set32u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range
                 ~min:(int64_to_int64 Int64.min_int)
                 ~max:(int64_to_int64 Int64.max_int)
               |> int64_of_int64


  let to_index = Int64.of_int
  let data_equal = Int64.equal
  let unbox_index = Int64_u.of_int64
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int64#
        -> int64
        = "%caml_string_get64_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int64
        = "%caml_string_get64u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int64#
        -> int64
        = "%caml_bytes_get64_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int64
        = "%caml_bytes_get64u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int64#
        -> int64
        -> unit
        = "%caml_bytes_set64_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int64
        -> unit
        = "%caml_bytes_set64u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int64#
        -> int64
        = "%caml_bigstring_get64_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int64
        = "%caml_bigstring_get64u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int64#
        -> int64
        -> unit
        = "%caml_bigstring_set64_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int64
        -> unit
        = "%caml_bigstring_set64u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int32

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int32.zero
      | 1 -> Int32.min_int
      | 2 -> Int32.max_int
      | _ -> Random.int64_in_range
                 ~min:(int32_to_int64 Int32.min_int)
                 ~max:(int32_to_int64 Int32.max_int)
               |> int32_of_int64


  let to_index = Int64.of_int
  let data_equal = Int32.equal
  let unbox_index = Int64_u.of_int64
  let unbox_data = Int32_u.of_int32
  let box_data = Int32_u.to_int32
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int32
        = "%caml_string_get32"

      external get_safe
        :  string
        -> int64#
        -> int32#
        = "%caml_string_get32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int32#
        = "%caml_string_get32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int32
        = "%caml_bytes_get32"

      external get_safe
        :  bytes
        -> int64#
        -> int32#
        = "%caml_bytes_get32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int32#
        = "%caml_bytes_get32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int32
        -> unit
        = "%caml_bytes_set32"

      external set_safe
        :  bytes
        -> int64#
        -> int32#
        -> unit
        = "%caml_bytes_set32#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int32#
        -> unit
        = "%caml_bytes_set32u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int32
        = "%caml_bigstring_get32"

      external get_safe
        :  bigstring
        -> int64#
        -> int32#
        = "%caml_bigstring_get32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int32#
        = "%caml_bigstring_get32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int32
        -> unit
        = "%caml_bigstring_set32"

      external set_safe
        :  bigstring
        -> int64#
        -> int32#
        -> unit
        = "%caml_bigstring_set32#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int32#
        -> unit
        = "%caml_bigstring_set32u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = int64

  let generate_data = 
    fun i ->
      match i mod 4 with
      | 0 -> Int64.zero
      | 1 -> Int64.min_int
      | 2 -> Int64.max_int
      | _ -> Random.int64_in_range
                 ~min:(int64_to_int64 Int64.min_int)
                 ~max:(int64_to_int64 Int64.max_int)
               |> int64_of_int64


  let to_index = Int64.of_int
  let data_equal = Int64.equal
  let unbox_index = Int64_u.of_int64
  let unbox_data = Int64_u.of_int64
  let box_data = Int64_u.to_int64
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> int64
        = "%caml_string_get64"

      external get_safe
        :  string
        -> int64#
        -> int64#
        = "%caml_string_get64#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> int64#
        = "%caml_string_get64u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> int64
        = "%caml_bytes_get64"

      external get_safe
        :  bytes
        -> int64#
        -> int64#
        = "%caml_bytes_get64#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> int64#
        = "%caml_bytes_get64u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> int64
        -> unit
        = "%caml_bytes_set64"

      external set_safe
        :  bytes
        -> int64#
        -> int64#
        -> unit
        = "%caml_bytes_set64#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> int64#
        -> unit
        = "%caml_bytes_set64u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> int64
        = "%caml_bigstring_get64"

      external get_safe
        :  bigstring
        -> int64#
        -> int64#
        = "%caml_bigstring_get64#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> int64#
        = "%caml_bigstring_get64u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> int64
        -> unit
        = "%caml_bigstring_set64"

      external set_safe
        :  bigstring
        -> int64#
        -> int64#
        -> unit
        = "%caml_bigstring_set64#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> int64#
        -> unit
        = "%caml_bigstring_set64u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Float32.of_float f

  let to_index = Int64.of_int
  let data_equal = Float32.equal
  let unbox_index = Int64_u.of_int64
  let unbox_data = fun x -> x
  let box_data = fun x -> x
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int64#
        -> float32
        = "%caml_string_getf32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> float32
        = "%caml_string_getf32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int64#
        -> float32
        = "%caml_bytes_getf32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> float32
        = "%caml_bytes_getf32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int64#
        -> float32
        -> unit
        = "%caml_bytes_setf32_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> float32
        -> unit
        = "%caml_bytes_setf32u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int64#
        -> float32
        = "%caml_bigstring_getf32_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> float32
        = "%caml_bigstring_getf32u_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int64#
        -> float32
        -> unit
        = "%caml_bigstring_setf32_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> float32
        -> unit
        = "%caml_bigstring_setf32u_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end

open struct

  type boxed_index = int64
  type boxed_data = float32

  let generate_data = 
  fun _ ->
    let f =
      let f = Random.float Float.max_float in
      if Random.bool () then Float.neg f else f
    in
    Float32.of_float f

  let to_index = Int64.of_int
  let data_equal = Float32.equal
  let unbox_index = Int64_u.of_int64
  let unbox_data = Float32_u.of_float32
  let box_data = Float32_u.to_float32
  let extra_bounds_checks = Int64.[ min_int; max_int; add min_int one; sub zero one ]
  let max_index = Int.max_int - 1


  module _ = Tester_no_set (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = string

      let create = create_s
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : string
        -> int
        -> float32
        = "%caml_string_getf32"

      external get_safe
        :  string
        -> int64#
        -> float32#
        = "%caml_string_getf32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  string
        -> int64#
        -> float32#
        = "%caml_string_getf32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bytes

      let create = create_b
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bytes
        -> int
        -> float32
        = "%caml_bytes_getf32"

      external get_safe
        :  bytes
        -> int64#
        -> float32#
        = "%caml_bytes_getf32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bytes
        -> int64#
        -> float32#
        = "%caml_bytes_getf32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bytes
        -> int
        -> float32
        -> unit
        = "%caml_bytes_setf32"

      external set_safe
        :  bytes
        -> int64#
        -> float32#
        -> unit
        = "%caml_bytes_setf32#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bytes
        -> int64#
        -> float32#
        -> unit
        = "%caml_bytes_setf32u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)

  module _ = Tester (struct
      type nonrec boxed_index = boxed_index
      type nonrec boxed_data = boxed_data
      type container = bigstring

      let create = create_bs
      let generate_data = generate_data
      let to_index = to_index
      let data_equal = data_equal
      let extra_bounds_checks = extra_bounds_checks
      let max_index = max_index

      external get_reference
        : bigstring
        -> int
        -> float32
        = "%caml_bigstring_getf32"

      external get_safe
        :  bigstring
        -> int64#
        -> float32#
        = "%caml_bigstring_getf32#_indexed_by_int64#"

      let get_safe b i = box_data (get_safe b (unbox_index i))

      external get_unsafe
        :  bigstring
        -> int64#
        -> float32#
        = "%caml_bigstring_getf32u#_indexed_by_int64#"

      let get_unsafe b i = box_data (get_unsafe b (unbox_index i))

      external set_reference
        : bigstring
        -> int
        -> float32
        -> unit
        = "%caml_bigstring_setf32"

      external set_safe
        :  bigstring
        -> int64#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32#_indexed_by_int64#"

      let set_safe b i d = set_safe b (unbox_index i) (unbox_data d)

      external set_unsafe
        :  bigstring
        -> int64#
        -> float32#
        -> unit
        = "%caml_bigstring_setf32u#_indexed_by_int64#"

      let set_unsafe b i d = set_unsafe b (unbox_index i) (unbox_data d)
    end)


end
