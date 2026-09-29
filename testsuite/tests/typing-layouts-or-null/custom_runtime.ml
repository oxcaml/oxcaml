(* TEST
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

type no_param =
  | No_param_null
  | No_param_payload of int
[@@or_null]

type float_payload =
  | Float_null
  | Float_payload of float
[@@or_null]

type void : void mod everything
external void : unit -> void = "%unbox_unit"

let[@inline never] use_void (v : void) =
  let _ : void = v in
  1

type void_null =
  | Null_void of void
  | This_void of int
[@@or_null]

type void_product_null =
  | This_void_product of int
  | Null_void_product of #(void * void)
[@@or_null]

type 'a unused_param =
  | Unused_null
  | Unused_payload of int
[@@or_null]

type ('a, 'b) multi_param =
  | Multi_null
  | Multi_payload of ('a list * 'b)
[@@or_null]

let () =
  match Nope with
  | Nope -> ()
  | Yep _ -> assert false
;;

let () =
  match Yep 3 with
  | Yep 3 -> ()
  | _ -> assert false
;;

let () =
  match Nope_last with
  | Nope_last -> ()
  | Yep_first _ -> assert false
;;

let () =
  match Yep_first "custom" with
  | Yep_first "custom" -> ()
  | _ -> assert false
;;

let () =
  match No_param_null with
  | No_param_null -> ()
  | No_param_payload _ -> assert false
;;

let () =
  match No_param_payload 11 with
  | No_param_payload 11 -> ()
  | _ -> assert false
;;

let () =
  match Float_null with
  | Float_null -> ()
  | Float_payload _ -> assert false
;;

let () =
  match Float_payload 3.5 with
  | Float_payload x when x = 3.5 -> ()
  | _ -> assert false
;;

let () =
  match Null_void (void ()) with
  | Null_void v when use_void v = 1 -> ()
  | _ -> assert false
;;

let () =
  let effects = ref 0 in
  let void_effect () =
    incr effects;
    void ()
  in
  match Null_void (void_effect ()) with
  | Null_void _ -> assert (!effects = 1)
  | _ -> assert false
;;

let () =
  match This_void 17 with
  | This_void 17 -> ()
  | _ -> assert false
;;

let () =
  match Null_void_product #(void (), void ()) with
  | Null_void_product #(v1, v2) when use_void v1 = 1 && use_void v2 = 1 -> ()
  | _ -> assert false
;;

let () =
  match This_void_product 19 with
  | This_void_product 19 -> ()
  | _ -> assert false
;;

let () =
  match (Unused_null : string unused_param) with
  | Unused_null -> ()
  | Unused_payload _ -> assert false
;;

let () =
  match (Unused_payload 13 : string unused_param) with
  | Unused_payload 13 -> ()
  | _ -> assert false
;;

let () =
  match Multi_null with
  | Multi_null -> ()
  | Multi_payload _ -> assert false
;;

let () =
  match Multi_payload ([ "a"; "b" ], 2) with
  | Multi_payload ([ "a"; "b" ], 2) -> ()
  | _ -> assert false
;;

let map_t f = function
  | Nope -> Nope
  | Yep x -> Yep (f x)

let map_flipped f = function
  | Nope_last -> Nope_last
  | Yep_first x -> Yep_first (f x)

let map_float_payload f = function
  | Float_null -> Float_null
  | Float_payload x -> Float_payload (f x)

let map_multi_param f g = function
  | Multi_null -> Multi_null
  | Multi_payload (xs, y) -> Multi_payload (List.map f xs, g y)

let () =
  match map_t (fun x -> x + 1) (Yep 4) with
  | Yep 5 -> ()
  | _ -> assert false
;;

let () =
  match map_t (fun x -> x + 1) Nope with
  | Nope -> ()
  | _ -> assert false
;;

let () =
  match map_flipped String.uppercase_ascii (Yep_first "ok") with
  | Yep_first "OK" -> ()
  | _ -> assert false
;;

let () =
  match map_float_payload (( +. ) 0.5) (Float_payload 1.25) with
  | Float_payload x when x = 1.75 -> ()
  | _ -> assert false
;;

let () =
  match map_float_payload (( +. ) 0.5) Float_null with
  | Float_null -> ()
  | _ -> assert false
;;

let () =
  match
    map_multi_param String.length succ
      (Multi_payload ([ "a"; "bc" ], 4))
  with
  | Multi_payload ([ 1; 2 ], 5) -> ()
  | _ -> assert false
;;

let () =
  match map_multi_param String.length succ Multi_null with
  | Multi_null -> ()
  | _ -> assert false
;;

let () =
  match (Nope, Yep "payload") with
  | Nope, Yep "payload" -> ()
  | _ -> assert false
;;

let make_closure x = fun () -> Yep x

let make_multi_closure xs y = fun () -> Multi_payload (xs, y)

let () =
  match make_closure 7 () with
  | Yep 7 -> ()
  | _ -> assert false
;;

let () =
  match make_multi_closure [ 1; 2 ] "closure" () with
  | Multi_payload ([ 1; 2 ], "closure") -> ()
  | _ -> assert false
;;

let () =
  let r = ref Nope in
  (match !r with
  | Nope -> ()
  | _ -> assert false);
  r := Yep "ref";
  match !r with
  | Yep "ref" -> ()
  | _ -> assert false
;;

let () =
  let r = ref No_param_null in
  (match !r with
  | No_param_null -> ()
  | _ -> assert false);
  r := No_param_payload 21;
  match !r with
  | No_param_payload 21 -> ()
  | _ -> assert false
;;

let () =
  let r = ref Multi_null in
  (match !r with
  | Multi_null -> ()
  | _ -> assert false);
  r := Multi_payload ([ "ref" ], 1);
  match !r with
  | Multi_payload ([ "ref" ], 1) -> ()
  | _ -> assert false
;;

let () =
  let bytes = Marshal.to_bytes (Yep_first 9) [] in
  match Marshal.from_bytes bytes 0 with
  | Yep_first 9 -> ()
  | _ -> assert false
;;

let () =
  let bytes = Marshal.to_bytes Nope [] in
  match Marshal.from_bytes bytes 0 with
  | Nope -> ()
  | _ -> assert false
;;

let () =
  let bytes = Marshal.to_bytes Nope_last [] in
  match Marshal.from_bytes bytes 0 with
  | Nope_last -> ()
  | _ -> assert false
;;

let () =
  let bytes = Marshal.to_bytes (No_param_payload 5) [] in
  match Marshal.from_bytes bytes 0 with
  | No_param_payload 5 -> ()
  | _ -> assert false
;;

let () =
  let bytes = Marshal.to_bytes Float_null [] in
  match Marshal.from_bytes bytes 0 with
  | Float_null -> ()
  | _ -> assert false
;;

let () =
  let bytes = Marshal.to_bytes (Float_payload 2.25) [] in
  match Marshal.from_bytes bytes 0 with
  | Float_payload x when x = 2.25 -> ()
  | _ -> assert false
;;

let () =
  let bytes = Marshal.to_bytes (Multi_payload ([ 1; 2 ], "marshal")) [] in
  match Marshal.from_bytes bytes 0 with
  | Multi_payload ([ 1; 2 ], "marshal") -> ()
  | _ -> assert false
;;

let () =
  assert (Nope = Nope);
  assert (Yep 4 = Yep 4);
  assert (Nope <> Yep 4);
  assert (compare Nope Nope = 0);
  assert (compare Nope (Yep 4) < 0);
  assert (compare (Yep 4) Nope > 0);
  assert (compare (Yep 4) (Yep 5) < 0);
  assert (Nope_last = Nope_last);
  assert (Yep_first "a" = Yep_first "a");
  assert (Nope_last <> Yep_first "a");
  assert (compare Nope_last Nope_last = 0);
  assert (compare Nope_last (Yep_first "a") < 0);
  assert (compare (Yep_first "a") Nope_last > 0);
  assert (No_param_null = No_param_null);
  assert (No_param_payload 4 = No_param_payload 4);
  assert (No_param_null <> No_param_payload 4);
  assert (compare No_param_null (No_param_payload 4) < 0);
  assert (Float_null = Float_null);
  assert (Float_payload 1.5 = Float_payload 1.5);
  assert (Float_null <> Float_payload 1.5);
  assert (compare Float_null (Float_payload 1.5) < 0);
  assert ((Unused_null : string unused_param) = Unused_null);
  assert ((Unused_payload 3 : string unused_param) = Unused_payload 3);
  assert ((Unused_null : string unused_param) <> Unused_payload 3);
  assert (compare (Unused_null : string unused_param) (Unused_payload 3) < 0);
  assert (Multi_null = Multi_null);
  assert (Multi_payload ([ 1 ], "a") = Multi_payload ([ 1 ], "a"));
  assert (Multi_null <> Multi_payload ([ 1 ], "a"));
  assert (compare Multi_null (Multi_payload ([ 1 ], "a")) < 0)
;;

(* GADT constructors keep the same null/payload representation. *)
type 'a gadt = N : 'a gadt | P : 'a -> 'a gadt [@@or_null]

let[@inline never] map_gadt : type a b. (a -> b) -> a gadt -> b gadt =
  fun f -> function N -> N | P x -> P (f x)

let () =
  assert (map_gadt succ (P 3) = P 4);
  assert (map_gadt succ N = N);
  assert (map_gadt String.length (P "abc") = P 3);
  assert (map_gadt (fun x -> x +. 1.) (P 2.5) = P 3.5);
  assert (map_gadt (fun x -> x +. 1.) N = N);
  let xs = [|P 1; N; P 3|] in
  xs.(1) <- P 2;
  xs.(0) <- N;
  assert (Array.fold_left
    (fun sum -> function N -> sum | P x -> sum + x) 0 xs = 5);
  let round_trip (x : int gadt) : int gadt =
    Marshal.from_bytes (Marshal.to_bytes x []) 0
  in
  assert (round_trip N = N);
  assert (round_trip (P 7) = P 7);
  assert (compare (N : int gadt) (P 0) < 0);
  assert (compare (P 0) (N : int gadt) > 0)
;;

type mixed =
  | Mixed_null
  | Mixed_this : 'a -> mixed
[@@or_null]

let[@inline never] mixed_is_null = function
  | Mixed_null -> true
  | Mixed_this _ -> false

type 'a mixed_null_gadt =
  | Mixed_gadt_null : int mixed_null_gadt
  | Mixed_plain_this of 'a
[@@or_null]

let[@inline never] mixed_to_option : type a. a mixed_null_gadt -> a option =
  function
  | Mixed_gadt_null -> None
  | Mixed_plain_this x -> Some x

let[@inline never] mixed_string_payload (x : string mixed_null_gadt) =
  match x with Mixed_plain_this s -> s

let () =
  assert (mixed_is_null Mixed_null);
  assert (not (mixed_is_null (Mixed_this 42)));
  assert (not (mixed_is_null (Mixed_this "hello")));
  assert (mixed_to_option Mixed_gadt_null = None);
  assert (mixed_to_option (Mixed_plain_this 42) = Some 42);
  assert (mixed_to_option (Mixed_plain_this "hello") = Some "hello");
  assert (mixed_string_payload (Mixed_plain_this "hello") = "hello")
;;

type _ indexed_gadt =
  | IN : int indexed_gadt
  | IP : float -> float indexed_gadt
[@@or_null]

let[@inline never] indexed_float (x : float indexed_gadt) =
  match x with IP f -> f
let[@inline never] indexed_null (x : int indexed_gadt) =
  match x with IN -> ()

let () =
  assert (indexed_float (IP 3.5) = 3.5);
  indexed_null IN
;;

type 'a existential_gadt =
  | EN : 'a existential_gadt
  | EP : ('b * ('b -> 'a)) -> 'a existential_gadt
[@@or_null]

let[@inline never] run_existential : type a. a existential_gadt -> a option =
  function EN -> None | EP (x, f) -> Some (f x)

let () =
  assert (run_existential EN = None);
  assert (run_existential (EP ("abcd", String.length)) = Some 4);
  assert (run_existential (EP (2., fun x -> x +. 1.)) = Some 3.)
;;

type _ compound_gadt =
  | CN : 'a compound_gadt
  | CP : 'a -> 'a list compound_gadt
[@@or_null]

let () =
  let x : int list compound_gadt = CP 3 in
  match x with CP n -> assert (n = 3) | CN -> assert false
;;

type _ void_gadt =
  | VN : void -> int void_gadt
  | VP : int -> int void_gadt
[@@or_null]

type _ void_gadt_flipped =
  | FP : int -> int void_gadt_flipped
  | FN : #(void * void) -> int void_gadt_flipped
[@@or_null]

let[@inline never] void_gadt_value = function
  | VN v -> use_void v - 1
  | VP n -> n

let () =
  let effects = ref 0 in
  let v = VN (incr effects; void ()) in
  assert (!effects = 1);
  assert (void_gadt_value v = 0);
  assert (void_gadt_value (VP 5) = 5);
  assert (Array.fold_left
    (fun sum x -> sum + void_gadt_value x) 0 [|v; VP 7|] = 7);
  (match FN #(void (), void ()) with
   | FN #(x, y) -> assert (use_void x + use_void y = 2)
   | FP _ -> assert false);
  (match FP 9 with FP n -> assert (n = 9) | FN _ -> assert false)
;;
