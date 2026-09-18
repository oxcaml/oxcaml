(* TEST
 include stdlib_stable;
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

module Or_null = Stdlib_stable.Or_null

module Null_array = struct
  external[@layout_poly] length : ('a : any mod separable).
    'a array -> int = "%array_length"

  external[@layout_poly] get : ('a : any mod separable).
    'a array -> int -> 'a = "%array_safe_get"

  external[@layout_poly] set : ('a : any mod separable).
    'a array -> int -> 'a -> unit = "%array_safe_set"

  external[@layout_poly] unsafe_get : ('a : any mod separable).
    'a array -> int -> 'a = "%array_unsafe_get"

  external[@layout_poly] unsafe_set : ('a : any mod separable).
    'a array -> int -> 'a -> unit =
    "%array_unsafe_set"

  external[@layout_poly] unsafe_blit : ('a : any mod separable).
    'a array -> int -> 'a array -> int -> int -> unit =
    "%arrayblit"
end

let test_array_creation () =
  let arr1 = [| Null; This 1; Null; This 3; This 4 |] in
  let arr2 = [| This "hello"; Null; This "world" |] in

  (match arr1 with
  | [| Null; This 1; Null; This 3; This 4 |] -> ()
  | _ -> assert false);

  (match arr2 with
  | [| This "hello"; Null; This "world" |] -> ()
  | _ -> assert false);

  print_endline "test_array_creation: passed"

let test_array_patterns () =
  let check_first_two arr =
    match arr with
    | [| x; y; _ |] -> (x, y)
    | _ -> (Null, Null)
  in

  let arr1 = [| This 1; Null; This 3 |] in
  let (x, y) = check_first_two arr1 in
  assert (x = This 1);
  assert (y = Null);

  let arr2 = [| Null; This 2; This 3 |] in
  let (x, y) = check_first_two arr2 in
  assert (x = Null);
  assert (y = This 2);

  let is_empty arr =
    match arr with
    | [||] -> true
    | _ -> false
  in

  assert (is_empty [||]);
  assert (not (is_empty [| Null |]));
  assert (not (is_empty [| This 1 |]));

  print_endline "test_array_patterns: passed"

type myrecord = { x: int; y: string }

let test_different_types () =
  let rec1 = { x = 1; y = "one" } in
  let rec2 = { x = 2; y = "two" } in

  let rec_arr = [| This rec1; Null; This rec2 |] in

  (match rec_arr with
  | [| This r1; Null; This r2 |] ->
    assert (r1.x = 1);
    assert (r2.y = "two")
  | _ -> assert false);

  print_endline "test_different_types: passed"

let test_array_comparison () =
  let arr1 = [| Null; This 1; This 2 |] in
  let arr2 = [| Null; This 1; This 2 |] in
  let arr3 = [| This 1; Null; This 2 |] in

  assert (arr1 = arr2);
  assert (arr1 <> arr3);

  let empty1 : int or_null array = [||] in
  let empty2 : int or_null array = [||] in
  assert (empty1 = empty2);

  print_endline "test_array_comparison: passed"

let test_null_array_accessors () =
  let arr1 = [| Null; This 1; This 2; Null; This 4 |] in
  assert (Null_array.length arr1 = 5);

  let empty : int or_null array = [||] in
  assert (Null_array.length empty = 0);

  let arr2 = [| This 10; Null; This 30 |] in
  assert (Null_array.get arr2 0 = This 10);
  assert (Null_array.get arr2 1 = Null);
  assert (Null_array.get arr2 2 = This 30);

  Null_array.set arr2 1 (This 20);
  assert (Null_array.get arr2 1 = This 20);

  Null_array.set arr2 0 Null;
  assert (Null_array.get arr2 0 = Null);

  let arr3 = [| Null; This "hello"; This "world" |] in
  assert (Null_array.unsafe_get arr3 1 = This "hello");

  Null_array.unsafe_set arr3 2 (This "OCaml");
  assert (Null_array.unsafe_get arr3 2 = This "OCaml");

  let src = [| This 1; Null; This 3; This 4 |] in
  let dst = [| Null; Null; Null; Null; Null |] in

  Null_array.unsafe_blit src 1 dst 2 2;
  assert (Null_array.get dst 0 = Null);
  assert (Null_array.get dst 1 = Null);
  assert (Null_array.get dst 2 = Null);
  assert (Null_array.get dst 3 = This 3);
  assert (Null_array.get dst 4 = Null);

  let rec_arr = [| This { x = 5; y = "five" }; Null |] in
  (match Null_array.get rec_arr 0 with
  | This r -> assert (r.x = 5 && r.y = "five")
  | Null -> assert false);

  print_endline "test_null_array_accessors: passed"

let test_nested_patterns () =
  let nested = ([| Null; This 1 |], [| This "a"; Null |]) in

  (match nested with
  | ([| Null; This x |], [| This s; Null |]) ->
    assert (x = 1);
    assert (s = "a")
  | _ -> assert false);

  print_endline "test_nested_patterns: passed"

let test_or_null_vs_option_arrays () =
  let or_null_arr = [| Null; This 42; Null; This 100 |] in
  let option_arr = [| None; Some 42; None; Some 100 |] in

  assert (Null_array.length or_null_arr = Array.length option_arr);

  for i = 0 to Null_array.length or_null_arr - 1 do
    assert (Or_null.to_option (Null_array.get or_null_arr i) = option_arr.(i));
    assert (Or_null.of_option option_arr.(i) = Null_array.get or_null_arr i)
  done;

  let map_or_null f arr =
    let len = Null_array.length arr in
    let result = [| Null; Null; Null; Null |] in
    for i = 0 to len - 1 do
      Null_array.set result i (match Null_array.get arr i with
        | Null -> Null
        | This v -> This (f v))
    done;
    result
  in

  let map_option f arr =
    Array.map (function
      | None -> None
      | Some v -> Some (f v)
    ) arr
  in

  let double x = x * 2 in
  let mapped_or_null = map_or_null double or_null_arr in
  let mapped_option = map_option double option_arr in

  for i = 0 to Null_array.length mapped_or_null - 1 do
    assert (Or_null.to_option (Null_array.get mapped_or_null i) = mapped_option.(i))
  done;

  let sum_or_null arr =
    let sum = ref 0 in
    for i = 0 to Null_array.length arr - 1 do
      match Null_array.get arr i with
      | Null -> ()
      | This v -> sum := !sum + v
    done;
    !sum
  in

  let sum_option arr =
    Array.fold_left (fun acc x ->
      match x with
      | None -> acc
      | Some v -> acc + v
    ) 0 arr
  in

  assert (sum_or_null or_null_arr = sum_option option_arr);
  assert (sum_or_null or_null_arr = 142);

  print_endline "test_or_null_vs_option_arrays: passed"

let test_or_null_immutable_arrays () =
  let iarr1 = [:Null; This 1; This 2; Null:] in
  let iarr2 = [:This "hello"; Null; This "world":] in

  (match iarr1 with
  | [:Null; This 1; This 2; Null:] -> ()
  | _ -> assert false);

  (match iarr2 with
  | [:This "hello"; Null; This "world":] -> ()
  | _ -> assert false);

  let empty : int or_null iarray = [::] in
  (match empty with
  | [::] -> ()
  | _ -> assert false);

  let nested = ([:Null; This 10:], [:This "a"; Null; This "b":]) in
  (match nested with
  | ([:Null; This x:], [:This s1; Null; This s2:]) ->
    assert (x = 10);
    assert (s1 = "a");
    assert (s2 = "b")
  | _ -> assert false);

  let rec_iarr = [:This { x = 5; y = "five" }; Null; This { x = 6; y = "six" }:] in
  (match rec_iarr with
  | [:This r1; Null; This r2:] ->
    assert (r1.x = 5 && r1.y = "five");
    assert (r2.x = 6 && r2.y = "six")
  | _ -> assert false);

  let iarr3 = [:Null; This 1; This 2:] in
  let iarr4 = [:Null; This 1; This 2:] in
  let iarr5 = [:This 1; Null; This 2:] in
  assert (iarr3 = iarr4);
  assert (iarr3 <> iarr5);

  let check_array arr =
    match arr with
    | [:Null; This x; This y:] when x < y -> "ascending"
    | [:Null; This x; This y:] when x > y -> "descending"
    | [:Null; This x; This y:] when x = y -> "equal"
    | _ -> "other"
  in

  assert (check_array [:Null; This 1; This 2:] = "ascending");
  assert (check_array [:Null; This 2; This 1:] = "descending");
  assert (check_array [:Null; This 2; This 2:] = "equal");

  print_endline "test_or_null_immutable_arrays: passed"

(* Unboxed tuples or 2+ element records are always separable. *)

type or_null_tuple = #(float or_null * string or_null)
type or_null_triple = #(int or_null * float or_null * myrecord or_null)
type or_null_record = #{ x : float or_null; y : string or_null }
type nested_or_null = #(float or_null * #(string or_null * int or_null))

let test_unboxed_products_or_null () =
  let arr1 : or_null_tuple array =
    [| #(Null, This "hello"); #(This 3.14, Null); #(This 2.718, This "world") |] in
  assert (Null_array.length arr1 = 3);

  (match Null_array.get arr1 0 with
  | #(Null, This "hello") -> ()
  | _ -> assert false);

  (match Null_array.get arr1 1 with
  | #(This 3.14, Null) -> ()
  | _ -> assert false);

  (match Null_array.get arr1 2 with
  | #(This 2.718, This "world") -> ()
  | _ -> assert false);

  let arr2 : or_null_record array =
    [| #{ x = This 10.5; y = This "a" }; #{ x = Null; y = This "b" } |] in

  Null_array.set arr2 0 #{ x = Null; y = Null };

  (match Null_array.get arr2 0 with
  | #{ x = Null; y = Null } -> ()
  | _ -> assert false);

  (match arr2 with
  | [| #{ x = Null; _ }; #{ x = Null; _ } |] -> ()
  | _ -> assert false);

  let rec1 = { x = 1; y = "one" } in
  let rec2 = { x = 2; y = "two" } in
  let arr3 : or_null_triple array =
    [| #(This 1, Null, This rec1); #(Null, This 99.9, This rec2) |] in

  (match arr3 with
  | [| #(This 1, Null, This _); #(Null, This 99.9, This _) |] -> ()
  | _ -> assert false);

  let arr2 : nested_or_null array =
    [| #(This 1.5, #(Null, This 10));
       #(Null, #(This "hello", Null));
       #(This 3.7, #(This "world", This 30)) |] in

  (match arr2 with
  | [| #(This 1.5, #(Null, This 10));
       #(Null, #(This "hello", Null));
       #(This 3.7, #(This "world", This 30)) |] -> ()
  | _ -> assert false);

  let src = arr2 in
  let dst : nested_or_null array =
    [| #(Null, #(Null, Null));
       #(Null, #(Null, Null));
       #(Null, #(Null, Null)) |] in

  Null_array.unsafe_blit src 0 dst 1 2;

  (match Null_array.get dst 1 with
  | #(This 1.5, #(Null, This 10)) -> ()
  | _ -> assert false);

  print_endline "test_unboxed_products_or_null: passed"

let () =
  test_array_creation ();
  test_array_patterns ();
  test_different_types ();
  test_array_comparison ();
  test_null_array_accessors ();
  test_nested_patterns ();
  test_or_null_vs_option_arrays ();
  test_or_null_immutable_arrays ();
  test_unboxed_products_or_null ();
  print_endline "All tests passed!"
