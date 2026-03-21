(* TEST
*)

type with_boxed =
  | Nullish_boxed [@repr null]
  | Int_boxed of int [@repr immediate]
  | Boxed of string

let classify_with_boxed = function
  | Nullish_boxed -> 0
  | Int_boxed n -> n
  | Boxed _ -> -1
;;

let () =
  assert (classify_with_boxed Nullish_boxed = 0);
  assert (classify_with_boxed (Int_boxed 7) = 7);
  assert (classify_with_boxed (Boxed "boxed") = -1);
  match Boxed "boxed" with
  | Nullish_boxed -> assert false
  | Int_boxed _ -> assert false
  | Boxed "boxed" -> ()
  | Boxed _ -> assert false
;;

let make_boxed s = Boxed s

let () =
  assert (classify_with_boxed (make_boxed "boxed-again") = -1);
  match make_boxed "boxed-again" with
  | Nullish_boxed -> assert false
  | Int_boxed _ -> assert false
  | Boxed "boxed-again" -> ()
  | Boxed _ -> assert false
;;

type with_tuple =
  | Nullish_tuple [@repr null]
  | Int_tuple of int [@repr immediate]
  | Pair of int * int

let classify_with_tuple = function
  | Nullish_tuple -> 0
  | Int_tuple n -> n
  | Pair (x, y) -> x + y
;;

let make_pair x y = Pair (x, y)

let () =
  assert (classify_with_tuple Nullish_tuple = 0);
  assert (classify_with_tuple (Int_tuple 9) = 9);
  assert (classify_with_tuple (make_pair 3 4) = 7);
  match make_pair 3 4 with
  | Nullish_tuple -> assert false
  | Int_tuple _ -> assert false
  | Pair (3, 4) -> ()
  | Pair _ -> assert false
;;

type null_with_constants =
  | Null_const [@repr null]
  | A
  | B

let classify_null_with_constants = function
  | Null_const -> 0
  | A -> 1
  | B -> 2
;;

let () =
  assert (classify_null_with_constants Null_const = 0);
  assert (classify_null_with_constants A = 1);
  assert (classify_null_with_constants B = 2);
  assert (compare Null_const Null_const = 0);
  assert (A <> B);
  assert (Marshal.from_bytes (Marshal.to_bytes B []) 0 = B);
;;

type multi_boxed =
  | Null_multi [@repr null]
  | Int_multi of int [@repr immediate]
  | String_multi of string
  | Pair_multi of int * int

let classify_multi_boxed = function
  | Null_multi -> 0
  | Int_multi n -> n
  | String_multi s -> String.length s
  | Pair_multi (x, y) -> x - y
;;

let make_string_multi s = String_multi s

let () =
  assert (classify_multi_boxed Null_multi = 0);
  assert (classify_multi_boxed (Int_multi 13) = 13);
  assert (classify_multi_boxed (make_string_multi "abcd") = 4);
  assert (classify_multi_boxed (Pair_multi (9, 4)) = 5);
  (match make_string_multi "xy" with
   | String_multi "xy" -> ()
   | _ -> assert false);
  match Pair_multi (3, 7) with
  | Pair_multi (3, 7) -> ()
  | _ -> assert false
;;

type ('a : value pointer) null_immediate_pointer =
  | NIP [@repr null]
  | IIP of int [@repr immediate]
  | PIP of 'a [@repr pointer]

type ptr : value pointer

external make_ptr : string -> ptr = "%opaque"

let make_pip p = PIP p

let classify_nip = function
  | NIP -> 0
  | IIP n -> n
  | PIP _ -> -1
;;

let () =
  let ptr = make_ptr "ptr" in
  assert (classify_nip NIP = 0);
  assert (classify_nip (IIP 11) = 11);
  assert (classify_nip (PIP ptr) = -1);
  assert (classify_nip (make_pip ptr) = -1);
  assert (compare (make_pip ptr) (make_pip ptr) = 0);
  assert
    (Marshal.from_bytes (Marshal.to_bytes (make_pip ptr) []) 0 = make_pip ptr);
  (match PIP ptr with
   | NIP -> assert false
   | IIP _ -> assert false
   | PIP p when p == ptr -> ()
   | PIP _ -> assert false);
  match make_pip ptr with
  | NIP -> assert false
  | IIP _ -> assert false
  | PIP p when p == ptr -> ()
  | PIP _ -> assert false
;;

type int_or_err =
  | Int_or_err of int [@repr immediate]
  | Error of string [@repr pointer]

let classify_int_or_err = function
  | Int_or_err n -> n
  | Error s -> String.length s
;;

let () =
  assert (classify_int_or_err (Int_or_err 17) = 17);
  assert (classify_int_or_err (Error "boom") = 4);
  match Error "boom" with
  | Int_or_err _ -> assert false
  | Error "boom" -> ()
  | Error _ -> assert false
;;

type ('a : value) repr_value =
  | Null_value [@repr null]
  | Value of 'a [@repr value]

let to_option_value = function
  | Null_value -> None
  | Value x -> Some x
;;

let () =
  assert (to_option_value Null_value = None);
  assert (to_option_value (Value "value") = Some "value");
  match Value "value" with
  | Null_value -> assert false
  | Value "value" -> ()
  | Value _ -> assert false
;;

type tree =
  | Empty [@repr null]
  | Leaf of int [@repr immediate]
  | Branch of tree * tree

let rec sum_tree = function
  | Empty -> 0
  | Leaf n -> n
  | Branch (l, r) -> sum_tree l + sum_tree r
;;

let rec depth_tree = function
  | Empty -> 0
  | Leaf _ -> 1
  | Branch (l, r) -> 1 + max (depth_tree l) (depth_tree r)
;;

let sample_tree = Branch (Leaf 1, Branch (Empty, Leaf 2))

let () =
  assert (sum_tree sample_tree = 3);
  assert (depth_tree sample_tree = 3);
  assert
    (Marshal.from_bytes (Marshal.to_bytes sample_tree []) 0 = sample_tree);
  match sample_tree with
  | Branch (Leaf 1, Branch (Empty, Leaf 2)) -> ()
  | _ -> assert false
;;
