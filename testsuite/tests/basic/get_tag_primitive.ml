(* TEST *)

(* Tests for the new %get_tag primitive *)

external get_tag : 'a -> int = "%get_tag"

(* Test exception values - should have Obj.object_tag (248) for exception declarations,
   but exception constructors with arguments create regular blocks *)
exception Test_exn
exception Test_exn_with_arg of int * string

let test_exceptions () =
  Printf.printf "Testing exceptions:\n";
  let e1 = Test_exn in
  let e2 = Test_exn_with_arg (42, "hello") in
  Printf.printf "Test_exn tag: %d (expected %d)\n" (get_tag e1) Obj.object_tag;
  Printf.printf "Test_exn_with_arg tag: %d (exception value with arguments)\n" (get_tag e2);
  ()

(* Test tuples - should have tag 0 *)
let test_tuples () =
  Printf.printf "\nTesting tuples:\n";
  let t2 = (1, 2) in
  let t3 = (1, 2, 3) in
  let t4 = (1, 2, 3, 4) in
  Printf.printf "Tuple (1,2) tag: %d (expected 0)\n" (get_tag t2);
  Printf.printf "Tuple (1,2,3) tag: %d (expected 0)\n" (get_tag t3);
  Printf.printf "Tuple (1,2,3,4) tag: %d (expected 0)\n" (get_tag t4);
  ()

(* Test records - should have tag 0 *)
type test_record = { a : int; b : string; c : float }

let test_records () =
  Printf.printf "\nTesting records:\n";
  let r = { a = 42; b = "test"; c = 3.14 } in
  Printf.printf "Record tag: %d (expected 0)\n" (get_tag r);
  ()

(* Test arrays - should have tag 0 *)
let test_arrays () =
  Printf.printf "\nTesting arrays:\n";
  let arr1 = [| 1; 2; 3 |] in
  let arr2 = [| "a"; "b"; "c" |] in
  Printf.printf "Int array tag: %d (expected 0)\n" (get_tag arr1);
  Printf.printf "String array tag: %d (expected 0)\n" (get_tag arr2);
  ()

(* Test variant types - tags correspond to constructor order, but constant constructors 
   are represented as immediates and can't be tagged. Only constructors with arguments 
   get allocated blocks with tags. *)
type test_variant = 
  | A 
  | B of int 
  | C of int * string 
  | D

let test_variants () =
  Printf.printf "\nTesting variants:\n";
  (* A and D are constant constructors (immediates), can't use get_tag on them *)
  let v2 = B 42 in
  let v3 = C (1, "test") in
  Printf.printf "Variant B tag: %d (expected 0 for first constructor with args)\n" (get_tag v2);
  Printf.printf "Variant C tag: %d (expected 1 for second constructor with args)\n" (get_tag v3);
  ()

(* Test lists - should have tag 0 for cons cells *)
let test_lists () =
  Printf.printf "\nTesting lists:\n";
  let l1 = [1; 2; 3] in
  let l2 = ["a"; "b"] in
  Printf.printf "Non-empty int list tag: %d (expected 0)\n" (get_tag l1);
  Printf.printf "Non-empty string list tag: %d (expected 0)\n" (get_tag l2);
  ()

(* Test float boxed values - should have tag Obj.double_tag (253) *)
let test_floats () =
  Printf.printf "\nTesting floats:\n";
  let f1 = 3.14 in
  let f2 = 0.0 in
  let f3 = Float.infinity in
  Printf.printf "Float 3.14 tag: %d (expected %d)\n" (get_tag f1) Obj.double_tag;
  Printf.printf "Float 0.0 tag: %d (expected %d)\n" (get_tag f2) Obj.double_tag;
  Printf.printf "Float infinity tag: %d (expected %d)\n" (get_tag f3) Obj.double_tag;
  ()

(* Test strings - should have tag Obj.string_tag (252) *)
let test_strings () =
  Printf.printf "\nTesting strings:\n";
  let s1 = "hello" in
  let s2 = "" in
  let s3 = "a very long string that exceeds the immediate representation" in
  Printf.printf "String \"hello\" tag: %d (expected %d)\n" (get_tag s1) Obj.string_tag;
  Printf.printf "Empty string tag: %d (expected %d)\n" (get_tag s2) Obj.string_tag;
  Printf.printf "Long string tag: %d (expected %d)\n" (get_tag s3) Obj.string_tag;
  ()

(* Test lazy values - should have tag Obj.lazy_tag (246) *)
let test_lazy () =
  Printf.printf "\nTesting lazy values:\n";
  let lz1 = lazy (1 + 2) in
  let lz2 = lazy ("hello" ^ " world") in
  Printf.printf "Lazy int tag: %d (expected %d)\n" (get_tag lz1) Obj.lazy_tag;
  Printf.printf "Lazy string tag: %d (expected %d)\n" (get_tag lz2) Obj.lazy_tag;
  ()

(* Test closures - should have tag Obj.closure_tag (247) *)
let test_closures () =
  Printf.printf "\nTesting closures:\n";
  let f1 = fun x -> x + 1 in
  let f2 = let y = 42 in fun x -> x + y in
  Printf.printf "Simple closure tag: %d (expected %d)\n" (get_tag f1) Obj.closure_tag;
  Printf.printf "Closure with env tag: %d (expected %d)\n" (get_tag f2) Obj.closure_tag;
  ()

(* Test object values - should have tag Obj.object_tag (248) *)
let test_objects () =
  Printf.printf "\nTesting objects:\n";
  let obj1 = object method x = 42 end in
  let obj2 = object 
    method x = 1
    method y = "test" 
  end in
  Printf.printf "Simple object tag: %d (expected %d)\n" (get_tag obj1) Obj.object_tag;
  Printf.printf "Object with methods tag: %d (expected %d)\n" (get_tag obj2) Obj.object_tag;
  ()

(* Test custom blocks (like Int64, Int32) - should have tag Obj.custom_tag (255) *)
let test_custom_blocks () =
  Printf.printf "\nTesting custom blocks:\n";
  let i64 = Int64.of_int 42 in
  let i32 = Int32.of_int 42 in
  let nint = Nativeint.of_int 42 in
  Printf.printf "Int64 tag: %d (expected %d)\n" (get_tag i64) Obj.custom_tag;
  Printf.printf "Int32 tag: %d (expected %d)\n" (get_tag i32) Obj.custom_tag;
  Printf.printf "Nativeint tag: %d (expected %d)\n" (get_tag nint) Obj.custom_tag;
  ()

(* Test polymorphic variants - should have hash-based tags *)
let test_poly_variants () =
  Printf.printf "\nTesting polymorphic variants:\n";
  (* `Red is a constant constructor - represented as an immediate, can't use get_tag *)
  let pv2 = `Blue 42 in
  let pv3 = `Green ("test", 3.14) in
  Printf.printf "Poly variant `Blue tag: %d\n" (get_tag pv2);
  Printf.printf "Poly variant `Green tag: %d\n" (get_tag pv3);
  ()

(* Test references - should have tag 0 *)
let test_references () =
  Printf.printf "\nTesting references:\n";
  let r1 = ref 42 in
  let r2 = ref "hello" in
  Printf.printf "Int ref tag: %d (expected 0)\n" (get_tag r1);
  Printf.printf "String ref tag: %d (expected 0)\n" (get_tag r2);
  ()

(* Test option types - should have appropriate tags *)
let test_options () =
  Printf.printf "\nTesting options:\n";
  let opt1 = Some 42 in
  let opt2 = Some "hello" in
  (* None is an immediate value, can't use get_tag on it *)
  Printf.printf "Some 42 tag: %d (expected 0)\n" (get_tag opt1);
  Printf.printf "Some \"hello\" tag: %d (expected 0)\n" (get_tag opt2);
  ()

(* Test result types *)
let test_results () =
  Printf.printf "\nTesting result types:\n";
  let r1 = Ok 42 in
  let r2 = Error "failed" in
  Printf.printf "Ok 42 tag: %d (expected 0)\n" (get_tag r1);
  Printf.printf "Error \"failed\" tag: %d (expected 1)\n" (get_tag r2);
  ()

(* Test nested structures *)
let test_nested () =
  Printf.printf "\nTesting nested structures:\n";
  let nested1 = Some [1; 2; 3] in
  let nested2 = (Some 42, Error "test") in
  let nested3 = [| Some 1; Some 2; None |] in
  Printf.printf "Some [1;2;3] tag: %d (expected 0)\n" (get_tag nested1);
  Printf.printf "(Some 42, Error \"test\") tag: %d (expected 0)\n" (get_tag nested2);
  Printf.printf "[| Some 1; Some 2; None |] tag: %d (expected 0)\n" (get_tag nested3);
  ()

(* Test mutable records *)
type mutable_record = { mutable x : int; mutable y : string }

let test_mutable_records () =
  Printf.printf "\nTesting mutable records:\n";
  let mr = { x = 42; y = "test" } in
  Printf.printf "Mutable record tag: %d (expected 0)\n" (get_tag mr);
  mr.x <- 100;
  mr.y <- "updated";
  Printf.printf "Mutable record after update tag: %d (expected 0)\n" (get_tag mr);
  ()

(* Test float arrays - tag depends on whether flat float arrays are enabled *)
let test_float_arrays () =
  Printf.printf "\nTesting float arrays:\n";
  let farr1 = [| 1.0; 2.0; 3.0 |] in
  let farr2 = [| |] in  (* empty float array *)
  let expected_tag = 
    if Config.flat_float_array then
      Obj.double_array_tag  (* 254 when float array optimization is enabled *)
    else
      0  (* Regular array tag when optimization is disabled *)
  in
  Printf.printf "Float array tag: %d (expected %d)\n" (get_tag farr1) expected_tag;
  Printf.printf "Empty float array tag: %d (expected 0)\n" (get_tag farr2);
  ()

(* Test variants with multiple arguments *)
type multi_arg_variant = 
  | Single of int
  | Double of int * int
  | Triple of int * int * int
  | Quad of int * int * int * int

let test_multi_arg_variants () =
  Printf.printf "\nTesting variants with multiple arguments:\n";
  let v1 = Single 1 in
  let v2 = Double (1, 2) in
  let v3 = Triple (1, 2, 3) in
  let v4 = Quad (1, 2, 3, 4) in
  Printf.printf "Single 1 tag: %d (expected 0)\n" (get_tag v1);
  Printf.printf "Double (1,2) tag: %d (expected 1)\n" (get_tag v2);
  Printf.printf "Triple (1,2,3) tag: %d (expected 2)\n" (get_tag v3);
  Printf.printf "Quad (1,2,3,4) tag: %d (expected 3)\n" (get_tag v4);
  ()

(* Test GADTs *)
type _ gadt =
  | Int : int -> int gadt
  | String : string -> string gadt
  | Pair : 'a gadt * 'b gadt -> ('a * 'b) gadt

let test_gadts () =
  Printf.printf "\nTesting GADTs:\n";
  let g1 = Int 42 in
  let g2 = String "hello" in
  let g3 = Pair (Int 1, String "a") in
  Printf.printf "Int 42 tag: %d (expected 0)\n" (get_tag g1);
  Printf.printf "String \"hello\" tag: %d (expected 1)\n" (get_tag g2);
  Printf.printf "Pair (Int 1, String \"a\") tag: %d (expected 2)\n" (get_tag g3);
  ()

let () =
  Printf.printf "Testing the %%get_tag primitive\n";
  Printf.printf "=====================================\n";
  test_exceptions ();
  test_tuples ();
  test_records ();
  test_arrays ();
  test_variants ();
  test_lists ();
  test_floats ();
  test_strings ();
  test_lazy ();
  test_closures ();
  test_objects ();
  test_custom_blocks ();
  test_poly_variants ();
  test_references ();
  test_options ();
  test_results ();
  test_nested ();
  test_mutable_records ();
  test_float_arrays ();
  test_multi_arg_variants ();
  test_gadts ();
  Printf.printf "\nAll tests completed!\n";
  ()