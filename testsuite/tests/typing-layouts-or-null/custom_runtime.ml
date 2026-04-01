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

let map_t f = function
  | Nope -> Nope
  | Yep x -> Yep (f x)

let map_flipped f = function
  | Nope_last -> Nope_last
  | Yep_first x -> Yep_first (f x)

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
  match (Nope, Yep "payload") with
  | Nope, Yep "payload" -> ()
  | _ -> assert false
;;

let make_closure x = fun () -> Yep x

let () =
  match make_closure 7 () with
  | Yep 7 -> ()
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
  let bytes = Marshal.to_bytes (Yep_first 9) [] in
  match Marshal.from_bytes bytes 0 with
  | Yep_first 9 -> ()
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
  (* CR or-null: if source constructor order should matter for custom
     [@@or_null], consider enforcing that at type declaration time. *)
  assert (compare Nope_last (Yep_first "a") < 0);
  assert (compare (Yep_first "a") Nope_last > 0)
;;
