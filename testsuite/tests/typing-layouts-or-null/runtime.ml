(* TEST
*)

let x = Null

let () =
  match x with
  | Null -> ()
  | This _ -> assert false
;;

let y = This 3

let () =
  match y with
  | This 3 -> ()
  | _ -> assert false
;;


external int_as_pointer : int -> int or_null = "%int_as_pointer"

let n = int_as_pointer 0

let () =
  match n with
  | Null -> ()
  | _ -> assert false
;;

external int_as_int : int -> int or_null = "%opaque"

let m = int_as_int 5

let () =
  match m with
  | This 5 -> ()
  | This _ -> assert false
  | Null -> assert false
;;

let x = (Null, This "bar")

let () =
  match x with
  | Null, This "foo" -> assert false
  | Null, This "bar" -> ()
  | _, This "bar" -> assert false
  | Null, _ -> assert false
  | _, _ -> assert false
;;

let y a = fun () -> This a

let d = y 5

let () =
  match d () with
  | This 5 -> ()
  | _ -> assert false
;;

let z = Marshal.to_bytes (This "foo") []

let () =
  match Marshal.from_bytes z 0 with
    | This "foo" -> ()
    | This _ -> assert false
    | Null -> assert false
;;

let w = Marshal.to_bytes Null []

let () =
  match Marshal.from_bytes w 0 with
    | Null -> ()
    | This _ -> assert false
;;

external evil : 'a or_null -> 'a = "%opaque"

let e = This (evil Null)

let () =
  match e with
  | Null -> ()
  | This _ -> assert false
;;

let e' = evil (This 4)

let () =
  match e' with
  | 4 -> ()
  | _ -> assert false
;;

let f a = fun () ->
  match a with
  | This x -> x ^ "bar"
  | Null -> "foo"
;;

let g = f (This "xxx")

let () =
  match g () with
  | "xxxbar" -> ()
  | _ -> assert false
;;

let h = f Null

let () =
  match h () with
  | "foo" -> ()
  | _ -> assert false
;;

let x = ref Null

let () =
  match !x with
  | Null -> ()
  | _ -> assert false
;;

let () = x := This "foo"

let () =
  match !x with
  | This "foo" -> ()
  | _ -> assert false
;;

let () = x := Null

let () =
  match !x with
  | Null -> ()
  | _ -> assert false
;;

let () =
  assert (Null = Null);
  assert (This 4 = This 4);
  assert (Null <> This 4);
  assert (This 8 <> Null);
  assert (This 4 <> This 5);
;;

let () =
  assert (compare Null Null = 0);
  assert (compare (This 4) (This 4) = 0);
  assert (compare Null (This 4) < 0);
  assert (compare (This 8) Null > 0);
  assert (compare (This 4) (This 5) < 0);
  assert (compare (This "abc") (This "xyz") <> 0);
  assert (compare (This "xyz") (This "xyz") = 0);
;;

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

type ('a : value pointer) null_immediate_pointer =
  | NIP [@repr null]
  | IIP of int [@repr immediate]
  | PIP of 'a [@repr pointer]

type ptr : value pointer

external make_ptr : string -> ptr = "%opaque"

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
  match PIP ptr with
  | NIP -> assert false
  | IIP _ -> assert false
  | PIP p when p == ptr -> ()
  | PIP _ -> assert false
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
