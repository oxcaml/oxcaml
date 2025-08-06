(* TEST
 reference = "${test_source_directory}/void_extended_tests.reference";
 include stdlib_stable;
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

(***********)
(* Prelude *)

type void : void mod everything
external void : unit -> void = "%unbox_unit"

let test_num = ref 0
let start_test name =
  incr test_num;
  Printf.printf "Test %d: %s\n" !test_num name

let[@inline never] use_void (v : void) =
  let _ : void = v in
  1

(******************************************************)
(* Test 1: Equality, comparison, hashing, marshalling *)

type void_holder = Vh of void

type variant_with_void =
  | Empty
  | Single of void
  | Pair of void * void
  | Mixed of int * void * string

type record_with_void = {
  rv : void;
  rx : int;
}

let test1 () =
  start_test "equality, comparison, hashing, and marshalling";

  (* Blocks containing voids *)
  let vh1 = Vh (void ()) in
  let vh2 = Vh (void ()) in

  (* Test equality *)
  Printf.printf "  Equality vh1 = vh2: %b\n" (vh1 = vh2);
  Printf.printf "  Physical equality vh1 == vh2: %b\n" (vh1 == vh2);

  (* Test comparison *)
  Printf.printf "  Comparison vh1 <= vh2: %b\n" (vh1 <= vh2);
  Printf.printf "  Comparison vh1 >= vh2: %b\n" (vh1 >= vh2);
  Printf.printf "  Compare vh1 vh2: %d\n" (compare vh1 vh2);

  (* Test hashing *)
  Printf.printf "  Hash vh1: %d\n" (Hashtbl.hash vh1);
  Printf.printf "  Hash vh2: %d\n" (Hashtbl.hash vh2);

  (* Test polymorphic variants with void *)
  let pv1 = Mixed (42, void (), "hello") in
  let pv2 = Mixed (42, void (), "hello") in
  (try
    Printf.printf "  Equality pv1 = pv2: %b\n" (pv1 = pv2)
  with Invalid_argument msg ->
    Printf.printf "  Equality pv1 = pv2: failed (%s)\n" msg);
  (try
    Printf.printf "  Compare pv1 pv2: %d\n" (compare pv1 pv2)
  with Invalid_argument msg ->
    Printf.printf "  Compare pv1 pv2: failed (%s)\n" msg);

  (* Test records with void *)
  let r1 = { rv = void (); rx = 100 } in
  let r2 = { rv = void (); rx = 100 } in
  (try
    Printf.printf "  Equality r1 = r2: %b\n" (r1 = r2)
  with Invalid_argument msg ->
    Printf.printf "  Equality r1 = r2: failed (%s)\n" msg);
  (try
    Printf.printf "  Compare r1 r2: %d\n" (compare r1 r2)
  with Invalid_argument msg ->
    Printf.printf "  Compare r1 r2: failed (%s)\n" msg);

  (* Test marshalling *)
  let test_marshalling x name =
    try
      let marshalled = Marshal.to_string x [] in
      let unmarshalled = Marshal.from_string marshalled 0 in
      Printf.printf "  Marshalling %s succeeded, equality: %b\n"
        name (x = unmarshalled)
    with e ->
      Printf.printf "  Marshalling %s failed: %s\n"
        name (Printexc.to_string e)
  in

  test_marshalling vh1 "void_holder";
  test_marshalling pv1 "variant_with_void";
  test_marshalling r1 "record_with_void";

  (* Test that operations on unboxed products with void are not supported *)
  let _ = #(42, void (), "test") in
  let _ = #(42, void (), "test") in
  Printf.printf "  Unboxed products with void: cannot test equality/comparison/hashing\n";
  Printf.printf "  (These operations are not supported on unboxed types containing void)\n"

let _ = test1 ()

(********************************************)
(* Test 2: Higher-order functions with void *)

let test2 () =
  start_test "higher-order functions with void arguments/returns";

  (* Function returning void *)
  let[@inline never] make_void () : void = void () in

  (* Function taking void as argument *)
  let[@inline never] consume_void (v : void) = use_void v in

  (* Higher-order function with void parameter *)
  let[@inline never] map_void f (v : void) = f v in

  (* Higher-order function returning function with void *)
  let[@inline never] curry_void f x =
    fun (v : void) -> f x v
  in

  (* Test composition with void *)
  let[@inline never] compose_void f g (v : void) = f (g v) in

  (* Apply tests *)
  let v = make_void () in
  assert (map_void consume_void v = 1);

  let add_with_void x (v : void) = x + use_void v in
  let curried = curry_void add_with_void 10 in
  assert (curried (void ()) = 11);

  (* Function returning void *)
  let[@inline never] identity_void (v : void) : void = v in
  let v2 = compose_void identity_void identity_void (void ()) in
  assert (use_void v2 = 1);

  (* List operations with void_holder instead of raw void *)
  let void_list = [Vh (void ()); Vh (void ()); Vh (void ())] in
  let counts = List.map (fun (Vh v) -> use_void v) void_list in
  assert (counts = [1; 1; 1]);

  (* Options with void_holder *)
  let[@inline never] process_void_holder_option opt =
    match opt with
    | Some (Vh v) -> use_void v
    | None -> 0
  in

  let vh_opt = Some (Vh (void ())) in
  assert (process_void_holder_option vh_opt = 1);
  assert (process_void_holder_option None = 0)

let _ = test2 ()

(************************************)
(* Test 3: Object methods with void *)

class void_class = object
  val mutable v : void_holder = Vh (void ())

  method get_void_holder = v

  method set_void (new_v : void) = v <- Vh new_v

  method use_void (x : void) = use_void x

  method combine_voids (v1 : void) (v2 : void) =
    use_void v1 + use_void v2
end

class ['a] parametric_void_class = object
  method wrap_void (a : 'a) (v : void) = (a, use_void v)

  method unwrap_void_unboxed : #('a * void) -> 'a * int =
    fun #(a, v) -> (a, use_void v)
end

let test3 () =
  start_test "object methods with void arguments/returns";

  let obj = new void_class in

  (* Test getting void from object *)
  let (Vh v) = obj#get_void_holder in
  assert (use_void v = 1);

  (* Test setting void in object *)
  obj#set_void (void ());

  (* Test method with void parameter *)
  assert (obj#use_void (void ()) = 1);

  (* Test method with multiple void parameters *)
  assert (obj#combine_voids (void ()) (void ()) = 2);

  (* Test parametric class with void *)
  let pobj = new parametric_void_class in
  let (s, count) = pobj#wrap_void "hello" (void ()) in
  assert (s = "hello" && count = 1);

  let (s2, count2) = pobj#unwrap_void_unboxed #("world", void ()) in
  assert (s2 = "world" && count2 = 1)

let _ = test3 ()

(*****************************************)
(* Test 4: Local continuations with void *)

let test4 () =
  start_test "local continuations with void arguments/returns";

  (* Local function with void argument *)
  let[@inline never] with_void_local () =
    let[@local] f (v : void) =
      100 + use_void v
    in
    f (void ())
  in
  assert (with_void_local () = 101);

  (* Local recursive function with void *)
  let[@inline never] loop_with_void_local n =
    let sum = ref 0 in
    let rec loop (v : void) i =
      if i >= n then !sum
      else begin
        sum := !sum + i + use_void v;
        loop (void ()) (i + 1)
      end
    in
    loop (void ()) 0
  in
  assert (loop_with_void_local 5 = 15); (* 0+1+2+3+4 + 5 void contributions *)

  (* Multiple void arguments in local function *)
  let[@inline never] multi_void_local () =
    let[@local] f (v1 : void) (v2 : void) x =
      x + use_void v1 + use_void v2
    in
    f (void ()) (void ()) 10
  in
  assert (multi_void_local () = 12)

let _ = test4 ()

(***********************************)
(* Test 5: Loopification with void *)

let test5 () =
  start_test "loopification with void arguments/returns";

  (* Loop with void parameter *)
  let[@inline never] sum_with_void n =
    let[@loop] rec loop (v : void) i acc =
      if i > n then acc
      else loop (void ()) (i + 1) (acc + i + use_void v)
    in
    loop (void ()) 1 0
  in
  assert (sum_with_void 5 = 20); (* 1+2+3+4+5 + 5 void contributions *)

  (* Loop with multiple void parameters *)
  let[@inline never] complex_loop n =
    let[@loop] rec loop (v1 : void) x (v2 : void) acc =
      if x > n then acc
      else
        let contribution = use_void v1 + use_void v2 in
        loop (void ()) (x + 1) (void ()) (acc + x + contribution)
    in
    loop (void ()) 1 (void ()) 0
  in
  assert (complex_loop 3 = 12) (* 1+2+3 + 3*2 void contributions *)

let _ = test5 ()

(*****************************************)
(* Test 6: Void in polymorphic functions *)

let test6 () =
  start_test "void in polymorphic functions";

  (* Void-specific identity *)
  let[@inline never] void_id (v : void) : void = v in
  let v = void_id (void ()) in
  assert (use_void v = 1);

  (* Polymorphic pair swap with unboxed tuple containing void *)
  let[@inline never] swap_unboxed #(x, y) = #(y, x) in
  let #(v, n) = swap_unboxed #(42, void ()) in
  assert (n = 42 && use_void v = 1);

  (* Polymorphic functions can work with void_holder *)
  let[@inline never] map_option : 'a 'b. ('a -> 'b) -> 'a option -> 'b option =
    fun f opt ->
      match opt with
      | None -> None
      | Some x -> Some (f x)
  in
  let vh_opt = map_option (fun () -> Vh (void ())) (Some ()) in
  match vh_opt with
  | None -> assert false
  | Some (Vh v) -> assert (use_void v = 1)

let _ = test6 ()

(**************************************)
(* Test 7: Stack allocation with void *)

let test8 () =
  start_test "stack-allocating blocks containing voids";

  (* Stack-allocated record with void *)
  let[@inline never] stack_record () =
    let r = stack_ { rv = void (); rx = 42 } in
    r.rx + use_void r.rv
  in
  assert (stack_record () = 43)

let _ = test8 ()

let () = print_endline "All extended tests completed."
