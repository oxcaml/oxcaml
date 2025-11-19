(* Test custom blocks: closures, lazy values, references, exceptions,
   first-class modules, weak references, hashtables *)

let[@inline never] [@local never] f_start () = ()

let _ = f_start ()

(* ============================================================================ *)
(* CLOSURES *)
(* ============================================================================ *)

(* Simple closure capturing one variable *)
let[@inline never] [@local never] f_simple_closure () =
  let x = 42 in
  let closure = fun () -> x in
  closure

let _ =
  let c = f_simple_closure () in
  c ()

(* Closure capturing multiple variables *)
let[@inline never] [@local never] f_multi_capture_closure () =
  let x = 10 in
  let y = 20 in
  let z = 30 in
  let closure = fun () -> x + y + z in
  closure

let _ =
  let c = f_multi_capture_closure () in
  c ()

(* Stateful closure using mutable reference *)
let[@inline never] [@local never] f_stateful_closure () =
  let counter = ref 0 in
  let increment = fun () ->
    counter := !counter + 1;
    !counter
  in
  increment

let _ =
  let inc = f_stateful_closure () in
  let _ = inc () in
  let _ = inc () in
  inc ()

(* Nested closures *)
let[@inline never] [@local never] f_nested_closure () =
  let x = 5 in
  let outer = fun y ->
    let inner = fun z -> x + y + z in
    inner
  in
  outer

let _ =
  let outer_fn = f_nested_closure () in
  let inner_fn = outer_fn 10 in
  inner_fn 15

(* ============================================================================ *)
(* LAZY VALUES *)
(* ============================================================================ *)

(* Simple lazy value *)
let[@inline never] [@local never] f_simple_lazy (x : int lazy_t) = x

let _ = f_simple_lazy (lazy 42)

let _ = f_simple_lazy (lazy (10 + 20))

(* Lazy computation *)
let[@inline never] [@local never] f_lazy_computation (x : int lazy_t) = x

let _ =
  let expensive_computation = lazy (
    let rec fib n =
      if n <= 1 then n
      else fib (n - 1) + fib (n - 2)
    in
    fib 10
  ) in
  f_lazy_computation expensive_computation

(* Test forcing a lazy value *)
let _ =
  let lz = lazy (100 * 2) in
  let _ = f_lazy_computation lz in
  let _ = Lazy.force lz in
  f_lazy_computation lz

(* Lazy closure combination *)
let[@inline never] [@local never] f_lazy_closure (x : (unit -> int) lazy_t) = x

let _ =
  let counter = ref 0 in
  let lazy_fn = lazy (fun () ->
    counter := !counter + 1;
    !counter
  ) in
  f_lazy_closure lazy_fn

(* ============================================================================ *)
(* REFERENCES *)
(* ============================================================================ *)

(* Integer reference *)
let[@inline never] [@local never] f_int_reference (x : int ref) = x

let _ = f_int_reference (ref 0)

let _ = f_int_reference (ref 123)

let _ = f_int_reference (ref (-456))

(* Tuple reference *)
let[@inline never] [@local never] f_tuple_reference (x : (int * string) ref) = x

let _ = f_tuple_reference (ref (1, "one"))

let _ = f_tuple_reference (ref (42, "answer"))

(* List reference *)
let[@inline never] [@local never] f_list_reference (x : int list ref) = x

let _ = f_list_reference (ref [])

let _ = f_list_reference (ref [1; 2; 3])

let _ = f_list_reference (ref [10; 20; 30; 40; 50])

(* ============================================================================ *)
(* MUTABLE RECORDS AND ARRAYS *)
(* ============================================================================ *)

type mutable_point = {
  mutable x: int;
  mutable y: int;
}

let[@inline never] [@local never] f_mutable_record (p : mutable_point) =
  let _ = p.x in
  let _ = p.y in
  p

let _ = f_mutable_record { x = 0; y = 0 }

let _ = f_mutable_record { x = 10; y = 20 }

let _ =
  let p = { x = 5; y = 5 } in
  p.x <- 15;
  p.y <- 25;
  f_mutable_record p

(* Mutable array *)
let[@inline never] [@local never] f_mutable_array (arr : int array) = arr

let _ = f_mutable_array [||]

let _ = f_mutable_array [| 1; 2; 3 |]

let _ =
  let arr = [| 10; 20; 30 |] in
  arr.(0) <- 100;
  arr.(2) <- 300;
  f_mutable_array arr

(* ============================================================================ *)
(* EXCEPTIONS *)
(* ============================================================================ *)

exception Custom_exception
exception Exception_with_data of int * string

let[@inline never] [@local never] f_exception (x : exn) = x

let _ = f_exception Custom_exception

let _ = f_exception (Exception_with_data (404, "not found"))

let _ = f_exception (Failure "test failure")

let _ = f_exception (Invalid_argument "invalid input")

(* ============================================================================ *)
(* OPTION WITH CLOSURE *)
(* ============================================================================ *)

let[@inline never] [@local never] f_option_closure (x : (int -> int) option) = x

let _ = f_option_closure None

let _ =
  let add_ten = fun x -> x + 10 in
  f_option_closure (Some add_ten)

let _ =
  let counter = ref 0 in
  let increment = fun x ->
    counter := !counter + x;
    !counter
  in
  f_option_closure (Some increment)

(* ============================================================================ *)
(* RESULT TYPE *)
(* ============================================================================ *)

type ('a, 'b) result = Ok of 'a | Error of 'b

let[@inline never] [@local never] f_result_type (x : (int, string) result) = x

let _ = f_result_type (Ok 42)

let _ = f_result_type (Error "something went wrong")

let _ = f_result_type (Ok 0)

let _ = f_result_type (Error "invalid operation")

(* ============================================================================ *)
(* FIRST-CLASS MODULES *)
(* ============================================================================ *)

module type COMPARABLE = sig
  type t
  val compare : t -> t -> int
end

module Int_comparable : COMPARABLE with type t = int = struct
  type t = int
  let compare = Int.compare
end

module String_comparable : COMPARABLE with type t = string = struct
  type t = string
  let compare = String.compare
end

let[@inline never] [@local never] f_first_class_module
  (cmp : (module COMPARABLE with type t = int)) = cmp

let _ =
  let module_val = (module Int_comparable : COMPARABLE with type t = int) in
  f_first_class_module module_val

let[@inline never] [@local never] f_first_class_module_string
  (cmp : (module COMPARABLE with type t = string)) = cmp

let _ =
  let module_val = (module String_comparable : COMPARABLE with type t = string) in
  f_first_class_module_string module_val

(* ============================================================================ *)
(* WEAK ARRAYS *)
(* ============================================================================ *)

let[@inline never] [@local never] f_weak_array (w : string Weak.t) = w

let _ = f_weak_array (Weak.create 0)

let _ = f_weak_array (Weak.create 5)

let _ =
  let w = Weak.create 3 in
  Weak.set w 0 (Some "first");
  Weak.set w 1 (Some "second");
  f_weak_array w

(* ============================================================================ *)
(* HASHTABLES *)
(* ============================================================================ *)

let[@inline never] [@local never] f_hashtable (h : (string, int) Hashtbl.t) = h

let _ = f_hashtable (Hashtbl.create 0)

let _ = f_hashtable (Hashtbl.create 10)

let _ =
  let h = Hashtbl.create 5 in
  Hashtbl.add h "one" 1;
  Hashtbl.add h "two" 2;
  Hashtbl.add h "three" 3;
  f_hashtable h

(* ============================================================================ *)
(* BUFFERS *)
(* ============================================================================ *)

let[@inline never] [@local never] f_buffer (b : Buffer.t) = b

let _ = f_buffer (Buffer.create 0)

let _ = f_buffer (Buffer.create 256)

let _ =
  let b = Buffer.create 10 in
  Buffer.add_string b "Hello";
  Buffer.add_char b ' ';
  Buffer.add_string b "World";
  f_buffer b

(* ============================================================================ *)
(* CLOSURE WITH OPTION AND LAZY COMBO *)
(* ============================================================================ *)

let[@inline never] [@local never] f_lazy_closure_combo
  (x : ((int -> int) option) lazy_t) = x

let _ = f_lazy_closure_combo (lazy None)

let _ =
  let counter = ref 0 in
  let lazy_opt_closure = lazy (
    Some (fun x ->
      counter := !counter + x;
      !counter
    )
  ) in
  f_lazy_closure_combo lazy_opt_closure
