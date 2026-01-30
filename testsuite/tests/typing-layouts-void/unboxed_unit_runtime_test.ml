(* TEST
 modules = "stubs.c";
 reference = "${test_source_directory}/unboxed_unit_runtime_test.reference";
 native;
 flambda2;
 {
   native;
 }{
   flags = "-Oclassic";
   native;
 }{
   flags = "-O3";
   native;
 }{
   bytecode;
 }
*)

let test_num = ref 0
let start_test name =
  incr test_num;
  Printf.printf "Test %d: %s\n" !test_num name

let unbox () = #()
let box #() = ()

(****************************)
(* Test 1: expressions *)

external void_to_void : unit# -> unit# = "void_to_void_bytecode" "void_to_void"
external void_to_seven : unit# -> int = "void_to_seven_bytecode" "void_to_seven"

let test1 () =
  start_test "expressions";
  assert (box (unbox (box #())) = ());
  assert (box (void_to_void #()) = ());
  assert (box (void_to_void (unbox (box #()))) = ());
  assert (void_to_seven (unbox (box #())) = 7);
  assert (void_to_seven (void_to_void #()) = 7);
  assert (void_to_seven (void_to_void (unbox (box #()))) = 7);
  (* the same test, eta-expanded *)
  let void_to_void = void_to_void in
  let void_to_seven = void_to_seven in
  assert (box (unbox (box #())) = ());
  assert (box (void_to_void #()) = ());
  assert (box (void_to_void (unbox (box #()))) = ());
  assert (void_to_seven (unbox (box #())) = 7);
  assert (void_to_seven (void_to_void #()) = 7);
  assert (void_to_seven (void_to_void (unbox (box #()))) = 7);
  ()

let _ = test1 ()

(****************************************)
(* Test 2: matching *)

let test2 () =
  start_test "matching";

  begin match #() with
  | #() -> ()
  end;

  begin match #(#(), #()) with
  | #(#(), #()) -> ()
  end;
  begin match #(#(), #()) with
  | #(#(), _) -> ()
  end;
  begin match #(#(), #()) with
  | #(_, #()) -> ()
  end;

  begin match #(#(), #(#(), #())) with
  | #(_, #(_, #())) -> ()
  end;
  begin match #(#(), #(#(), #())) with
  | #(_, #(#(), _)) -> ()
  end;
  begin match #(#(), #(#(), #())) with
  | #(#(), #(_, _)) -> ()
  end;

  begin match #(true, #()) with
  | #(false, #()) -> assert false
  | _ -> ()
  end;
  begin match #(#(), false) with
  | #(#(), true) -> assert false
  | _ -> ()
  end;

  begin match #(true, #()) with
  | #(false, #()) -> assert false
  | #(true, #()) -> ()
  end;
  begin match #(#(), false) with
  | #(#(), true) -> assert false
  | #(#(), false) -> ()
  end;

  begin match #(true, #()) with
  | #(true, #()) -> ()
  | #(false, #()) -> assert false
  end;
  begin match #(#(), false) with
  | #(#(), false) -> ()
  | #(#(), true) -> assert false
  end;

  begin match #(true, #()) with
  | #(x, #()) -> assert x
  end;
  begin match #(#(), false) with
  | #(#(), x) -> assert (not x)
  end;

  begin match #(true, #(), true) with
  | #(_, #(), false) | #(false, #(), _) -> assert false
  | _ -> ()
  end;
  begin match #(false, #(), false) with
  | #(true, #(), _) | #(_, #(), true) -> assert false
  | _ -> ()
  end;

  begin match #(true, #(), true) with
  | #(_, #(), false) | #(false, #(), _) -> assert false
  | #(true, #(), true) -> ()
  end;
  begin match #(false, #(), false) with
  | #(true, #(), _) | #(_, #(), true) -> assert false
  | #(false, #(), false) -> ()
  end;

  begin match #(true, #(), true) with
  | #(true, #(), true) -> ()
  | #(_, #(), false) | #(false, #(), _) -> assert false
  end;
  begin match #(false, #(), false) with
  | #(false, #(), false) -> ()
  | #(true, #(), _) | #(_, #(), true) -> assert false
  end;

  begin match #(true, #(), true) with
  | #(_, #(), false) -> assert false
  | #(false, #(), _) -> assert false
  | _ -> ()
  end;
  begin match #(false, #(), false) with
  | #(true, #(), _) -> assert false
  | #(_, #(), true) -> assert false
  | _ -> ()
  end;

  begin match #(true, #(), true) with
  | #(_, #(), false) -> assert false
  | #(false, #(), _) -> assert false
  | #(true, #(), true) -> ()
  end;
  begin match #(false, #(), false) with
  | #(true, #(), _) -> assert false
  | #(_, #(), true) -> assert false
  | #(false, #(), false) -> ()
  end;

  begin match #(true, #(), true) with
  | #(true, #(), true) -> ()
  | #(_, #(), false) -> assert false
  | #(false, #(), _) -> assert false
  end;
  begin match #(false, #(), false) with
  | #(false, #(), false) -> ()
  | #(true, #(), _) -> assert false
  | #(_, #(), true) -> assert false
  end;

  ()

let _ = test2 ()
