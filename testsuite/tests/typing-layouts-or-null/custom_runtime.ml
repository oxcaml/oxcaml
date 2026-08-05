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
