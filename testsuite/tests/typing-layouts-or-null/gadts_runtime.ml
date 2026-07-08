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

(* Runtime behaviour of custom [@@or_null] GADTs. The type-level acceptance
   and rejection of these declarations is pinned by gadts.ml; here we
   construct, match, [map], array-fold and marshal them to check the NULL /
   payload discrimination survives to run time under native, -O3, -Oclassic
   and bytecode. *)

(* Polymorphic GADT: the nullary constructor is NULL, the unary one is the
   unboxed payload. *)
type 'a gadt =
  | A : 'a gadt
  | B : 'a -> 'a gadt
[@@or_null]

(* Concrete-index GADT: each constructor fixes the index type, so matching
   also refines it. *)
type 'a concrete_gadt =
  | Null : int concrete_gadt
  | This : string -> bool concrete_gadt
[@@or_null]

(* Compound payload that mentions the constructor's index inside a tuple. *)
type 'a compound_gadt =
  | Comp_null : 'a compound_gadt
  | Comp : ('a * int) -> 'a compound_gadt
[@@or_null]

(* Existential payload: the [B] argument type is not the index. *)
type 'a existential_gadt =
  | Nothing : 'a existential_gadt
  | Something : 'b -> 'a existential_gadt
[@@or_null]

let map_gadt : type a b. (a -> b) -> a gadt -> b gadt =
 fun f -> function
  | A -> A
  | B x -> B (f x)

(* Polymorphic GADT construct / match at two indices. *)
let () =
  (match (A : int gadt) with
   | A -> ()
   | B _ -> assert false);
  (match B 3 with
   | B 3 -> ()
   | _ -> assert false);
  (match (A : string gadt) with
   | A -> ()
   | B _ -> assert false);
  (match B "custom" with
   | B "custom" -> ()
   | _ -> assert false)
;;

(* [map] preserves NULL and transforms the payload. *)
let () =
  (match map_gadt (fun x -> x + 1) (B 4) with
   | B 5 -> ()
   | _ -> assert false);
  (match map_gadt (fun x -> x + 1) A with
   | A -> ()
   | _ -> assert false);
  (match map_gadt String.length (B "abcd") with
   | B 4 -> ()
   | _ -> assert false)
;;

(* Concrete-index GADT: match refines the index at run time. *)
let concrete_len : type a. a concrete_gadt -> int = function
  | Null -> -1
  | This s -> String.length s

let () =
  assert (concrete_len (Null : int concrete_gadt) = -1);
  assert (concrete_len (This "hello") = 5);
  (match (This "hi" : bool concrete_gadt) with
   | This "hi" -> ()
   | This _ -> assert false)
;;

(* Compound payload mentioning the index. *)
let () =
  (match Comp ("x", 7) with
   | Comp ("x", 7) -> ()
   | _ -> assert false);
  (match (Comp_null : string compound_gadt) with
   | Comp_null -> ()
   | Comp _ -> assert false);
  let get_snd : type a. a compound_gadt -> int = function
    | Comp_null -> 0
    | Comp (_, n) -> n
  in
  assert (get_snd (Comp ("y", 9)) = 9);
  assert (get_snd (Comp_null : int compound_gadt) = 0)
;;

(* Existential payload. *)
let () =
  (match (Something 3.14 : int existential_gadt) with
   | Something _ -> ()
   | Nothing -> assert false);
  (match (Nothing : int existential_gadt) with
   | Nothing -> ()
   | Something _ -> assert false)
;;

(* [int gadt array]: the conservative decl jkind is refined to separable at
   the use site, so a value array with NULL slots builds and folds. *)
let sum_int_gadt (arr : int gadt array) =
  Array.fold_left (fun acc x -> match x with A -> acc | B n -> acc + n) 0 arr

let () =
  let arr = [| B 1; A; B 3; A; B 4 |] in
  assert (Array.length arr = 5);
  assert (sum_int_gadt arr = 8);
  arr.(1) <- B 10;
  assert (sum_int_gadt arr = 18);
  arr.(0) <- A;
  assert (sum_int_gadt arr = 17)
;;

(* Marshalling round-trips NULL and payload. *)
let () =
  let bytes = Marshal.to_bytes (B 9) [] in
  (match Marshal.from_bytes bytes 0 with
   | B 9 -> ()
   | _ -> assert false);
  let bytes = Marshal.to_bytes (A : int gadt) [] in
  (match Marshal.from_bytes bytes 0 with
   | A -> ()
   | _ -> assert false);
  let bytes = Marshal.to_bytes (This "round") [] in
  (match (Marshal.from_bytes bytes 0 : bool concrete_gadt) with
   | This "round" -> ()
   | _ -> assert false)
;;

(* Structural comparison discriminates NULL from payload. *)
let () =
  assert ((A : int gadt) = A);
  assert (B 4 = B 4);
  assert ((A : int gadt) <> B 4);
  assert (compare (A : int gadt) (B 4) < 0);
  assert (compare (B 4) (A : int gadt) > 0);
  assert (compare (B 4) (B 5) < 0)
;;

let () = print_endline "All tests passed!"
