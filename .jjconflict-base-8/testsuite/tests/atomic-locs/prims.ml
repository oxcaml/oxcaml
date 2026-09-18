(* TEST
   expect;
*)

type t = { mutable first_field : int; mutable field : int [@atomic] }
[%%expect{|
type t = { mutable first_field : int; mutable field : int [@atomic]; }
|}]

(* Test compare_exchange and compare_set separately *)

let x : t = { first_field = 1; field = 1 }
let print_value () = Format.printf "new value: %d\n%!" x.field
[%%expect{|
val x : t = {first_field = 1; field = 1}
val print_value : unit -> unit = <fun>
|}]

(* Failed compare_exchange *)
let result = Atomic.Loc.compare_exchange [%atomic.loc x.field] 2 2
let () = print_value ()
[%%expect{|
val result : int = 1
new value: 1
|}]

(* Succcessful compare_exchange *)
let result = Atomic.Loc.compare_exchange [%atomic.loc x.field] 1 2
let () = print_value ()
[%%expect{|
val result : int = 1
new value: 2
|}]

(* Failed compare_and_set *)
let result = Atomic.Loc.compare_and_set [%atomic.loc x.field] 1 3
let () = print_value ()
[%%expect{|
val result : bool = false
new value: 2
|}]

(* Succcessful compare_and_set *)
let result = Atomic.Loc.compare_and_set [%atomic.loc x.field] 2 3
let () = print_value ()
[%%expect{|
val result : bool = true
new value: 3
|}]

module Inline_record = struct
  type t =
    | A of { x : int; mutable field : int [@atomic] }
    | B of { x : int; y : int; mutable field : int [@atomic] }

  let get_field a = match a with
    | A r -> Atomic.Loc.get [%atomic.loc r.field]
    | B r -> Atomic.Loc.get [%atomic.loc r.field]

  let set_field a v = match a with
    | A r -> Atomic.Loc.set [%atomic.loc r.field] v
    | B r -> Atomic.Loc.set [%atomic.loc r.field] v
  ;;

  let cas_field a test v = match a with
    | A r -> Atomic.Loc.compare_and_set [%atomic.loc r.field] test v
    | B r -> Atomic.Loc.compare_and_set [%atomic.loc r.field] test v
  ;;

  let cmpxchg_field a test v = match a with
    | A r -> Atomic.Loc.compare_exchange [%atomic.loc r.field] test v
    | B r -> Atomic.Loc.compare_exchange [%atomic.loc r.field] test v
  ;;

  let a = A { x = 1; field = 1 }
  let () = set_field a 2
  let () = Format.printf "should be 2: %d\n%!" (get_field a)

  let b = B { x = 1; y = 2; field = 3 }
  let () = set_field b 7
  let () = Format.printf "should be 7: %d\n%!" (get_field b)

  (* Test compare_and_set operations *)
  let () = Format.printf "\nTesting compare_and_set:\n%!"

  (* Test A variant - successful CAS *)
  let a2 = A { x = 10; field = 5 }
  let cas_result = cas_field a2 5 10
  let () = Format.printf "A: CAS(5->10) result: %b, value: %d\n%!" cas_result (get_field a2)

  (* Test A variant - failed CAS *)
  let cas_result = cas_field a2 5 15
  let () = Format.printf "A: CAS(5->15) result: %b, value: %d\n%!" cas_result (get_field a2)

  (* Test B variant - successful CAS *)
  let b2 = B { x = 20; y = 30; field = 8 }
  let cas_result = cas_field b2 8 12
  let () = Format.printf "B: CAS(8->12) result: %b, value: %d\n%!" cas_result (get_field b2)

  (* Test B variant - failed CAS *)
  let cas_result = cas_field b2 8 16
  let () = Format.printf "B: CAS(8->16) result: %b, value: %d\n%!" cas_result (get_field b2)

  (* Test compare_exchange operations *)
  let () = Format.printf "\nTesting compare_exchange:\n%!"

  (* Test A variant - successful compare_exchange *)
  let a3 = A { x = 100; field = 20 }
  let cmpxchg_result = cmpxchg_field a3 20 25
  let () = Format.printf "A: compare_exchange(20->25) returned: %d, value: %d\n%!" cmpxchg_result (get_field a3)

  (* Test A variant - failed compare_exchange *)
  let cmpxchg_result = cmpxchg_field a3 20 30
  let () = Format.printf "A: compare_exchange(20->30) returned: %d, value: %d\n%!" cmpxchg_result (get_field a3)

  (* Test B variant - successful compare_exchange *)
  let b3 = B { x = 200; y = 300; field = 40 }
  let cmpxchg_result = cmpxchg_field b3 40 45
  let () = Format.printf "B: compare_exchange(40->45) returned: %d, value: %d\n%!" cmpxchg_result (get_field b3)

  (* Test B variant - failed compare_exchange *)
  let cmpxchg_result = cmpxchg_field b3 40 50
  let () = Format.printf "B: compare_exchange(40->50) returned: %d, value: %d\n%!" cmpxchg_result (get_field b3)
end
[%%expect{|
should be 2: 2
should be 7: 7

Testing compare_and_set:
A: CAS(5->10) result: true, value: 10
A: CAS(5->15) result: false, value: 10
B: CAS(8->12) result: true, value: 12
B: CAS(8->16) result: false, value: 12

Testing compare_exchange:
A: compare_exchange(20->25) returned: 20, value: 25
A: compare_exchange(20->30) returned: 25, value: 25
B: compare_exchange(40->45) returned: 40, value: 45
B: compare_exchange(40->50) returned: 45, value: 45
module Inline_record :
  sig
    type t =
        A of { x : int; mutable field : int [@atomic]; }
      | B of { x : int; y : int; mutable field : int [@atomic]; }
    val get_field : t -> int
    val set_field : t -> int -> unit
    val cas_field : t -> int -> int -> bool
    val cmpxchg_field : t -> int -> int -> int
    val a : t
    val b : t
    val a2 : t
    val b2 : t
    val cas_result : bool
    val a3 : t
    val b3 : t
    val cmpxchg_result : int
  end
|}]

(* Test atomic operations on variables *)
module Variable_tests = struct
  type record = { mutable atomic_field : int [@atomic] }

  (* Test with atomic loc stored in a variable *)
  let test_variable_atomic_loc () =
    let r = { atomic_field = 100 } in
    let atomic_loc = [%atomic.loc r.atomic_field] in

    (* Test compare_and_set with variable *)
    let cas_result = Atomic.Loc.compare_and_set atomic_loc 100 200 in
    let current_value = Atomic.Loc.get atomic_loc in
    Format.printf "Variable CAS(100->200): result=%b, value=%d\n%!" cas_result current_value;

    (* Test failed compare_and_set with variable *)
    let cas_result = Atomic.Loc.compare_and_set atomic_loc 100 300 in
    let current_value = Atomic.Loc.get atomic_loc in
    Format.printf "Variable CAS(100->300): result=%b, value=%d\n%!" cas_result current_value;

    (* Test compare_exchange with variable *)
    let cmpxchg_result = Atomic.Loc.compare_exchange atomic_loc 200 400 in
    let current_value = Atomic.Loc.get atomic_loc in
    Format.printf "Variable compare_exchange(200->400): returned=%d, value=%d\n%!" cmpxchg_result current_value;

    (* Test failed compare_exchange with variable *)
    let cmpxchg_result = Atomic.Loc.compare_exchange atomic_loc 200 500 in
    let current_value = Atomic.Loc.get atomic_loc in
    Format.printf "Variable compare_exchange(200->500): returned=%d, value=%d\n%!" cmpxchg_result current_value

  let () =
    Format.printf "\nTesting atomic operations on variables:\n%!";
    test_variable_atomic_loc ()
end
[%%expect{|

Testing atomic operations on variables:
Variable CAS(100->200): result=true, value=200
Variable CAS(100->300): result=false, value=200
Variable compare_exchange(200->400): returned=200, value=400
Variable compare_exchange(200->500): returned=400, value=400
module Variable_tests :
  sig
    type record = { mutable atomic_field : int [@atomic]; }
    val test_variable_atomic_loc : unit -> unit
  end
|}]

(* Mechanistically test all other ops *)

type ('a, 'r) op =
  | Load : (unit, int) op
  | Set : (int, unit) op
  | Exchange : (int, int) op
  | Fetch_add : (int, int) op
  | Add : (int, unit) op
  | Sub : (int, unit) op
  | Land : (int, unit) op
  | Lor : (int, unit) op
  | Lxor : (int, unit) op
[%%expect{|
type ('a, 'r) op =
    Load : (unit, int) op
  | Set : (int, unit) op
  | Exchange : (int, int) op
  | Fetch_add : (int, int) op
  | Add : (int, unit) op
  | Sub : (int, unit) op
  | Land : (int, unit) op
  | Lor : (int, unit) op
  | Lxor : (int, unit) op
|}]

type op_packed = Op : ('a, 'r) op -> op_packed

type target =
  | Field
  | Loc

type 'r outcome = {
  old_value : int
; new_value : int
; result : 'r
}

let run (type a r) t target (op : (a, r) op) ~(arg : a) : r outcome =
  let old_value = t.field in
  let result : r =
    match op, target with
    | Load, Field ->
      let loc = [%atomic.loc t.field] in
      Atomic.Loc.get loc
    | Load, Loc -> Atomic.Loc.get [%atomic.loc t.field]
    | Set, Field ->
      let loc = [%atomic.loc t.field] in
      Atomic.Loc.set loc arg
    | Set, Loc -> Atomic.Loc.set [%atomic.loc t.field] arg
    | Exchange, Field ->
      let loc = [%atomic.loc t.field] in
      Atomic.Loc.exchange loc arg
    | Exchange, Loc -> Atomic.Loc.exchange [%atomic.loc t.field] arg
    | Fetch_add, Field ->
      let loc = [%atomic.loc t.field] in
      Atomic.Loc.fetch_and_add loc arg
    | Fetch_add, Loc -> Atomic.Loc.fetch_and_add [%atomic.loc t.field] arg
    | Add, Field ->
      let loc = [%atomic.loc t.field] in
      Atomic.Loc.add loc arg
    | Add, Loc -> Atomic.Loc.add [%atomic.loc t.field] arg
    | Sub, Field ->
      let loc = [%atomic.loc t.field] in
      Atomic.Loc.sub loc arg
    | Sub, Loc -> Atomic.Loc.sub [%atomic.loc t.field] arg
    | Land, Field ->
      let loc = [%atomic.loc t.field] in
      Atomic.Loc.logand loc arg
    | Land, Loc -> Atomic.Loc.logand [%atomic.loc t.field] arg
    | Lor, Field ->
      let loc = [%atomic.loc t.field] in
      Atomic.Loc.logor loc arg
    | Lor, Loc -> Atomic.Loc.logor [%atomic.loc t.field] arg
    | Lxor, Field ->
      let loc = [%atomic.loc t.field] in
      Atomic.Loc.logxor loc arg
    | Lxor, Loc -> Atomic.Loc.logxor [%atomic.loc t.field] arg
  in
  let new_value = t.field in
  { old_value; new_value; result }

let expected_result (type a r) (op : (a, r) op) ~(arg : a) ~(old_value : int) : r =
  match op with
  | Load -> old_value
  | Set -> ()
  | Exchange -> old_value
  | Fetch_add -> old_value
  | Add -> ()
  | Sub -> ()
  | Land -> ()
  | Lor -> ()
  | Lxor -> ()
;;

let expected_value (type a r) (op : (a, r) op) ~(arg : a) ~(old_value : int) : int =
  match op with
  | Load -> old_value
  | Set -> arg
  | Exchange -> arg
  | Fetch_add -> old_value + arg
  | Add -> old_value + arg
  | Sub -> old_value - arg
  | Land -> old_value land arg
  | Lor -> old_value lor arg
  | Lxor -> old_value lxor arg
;;

let all_ops = [
  Op Load;
  Op Set;
  Op Exchange;
  Op Fetch_add;
  Op Add;
  Op Sub;
  Op Land;
  Op Lor;
  Op Lxor;
]

let generate_random_ops ?(length = 10) () : op_packed list =
  let num_ops = List.length all_ops in
  List.init length (fun _ ->
    let index = Random.int num_ops in
    List.nth all_ops index
  )

let generate_random_arg () = Random.int 100 - 50

let test_op (type a r) t target (op : (a, r) op) ~(arg : a) =
  let outcome = run t target op ~arg in
  let expected_res = expected_result op ~arg ~old_value:outcome.old_value in
  let expected_val = expected_value op ~arg ~old_value:outcome.old_value in

  let result_ok = match op with
    | Load | Exchange | Fetch_add -> outcome.result = expected_res
    | Set | Add | Sub | Land | Lor | Lxor -> true (* Ops that return unit *)
  in
  let value_ok = outcome.new_value = expected_val in

  if not result_ok || not value_ok then (
    let op_descr =
      match op, target with
      | Load, Field -> "Load(Field)"
      | Load, Loc -> "Load(Loc)"
      | Set, Field -> Printf.sprintf "Set(Field, %d)" arg
      | Set, Loc -> Printf.sprintf "Set(Loc, %d)" arg
      | Exchange, Field -> Printf.sprintf "Exchange(Field, %d)" arg
      | Exchange, Loc -> Printf.sprintf "Exchange(Loc, %d)" arg
      | Fetch_add, Field -> Printf.sprintf "Fetch_add(Field, %d)" arg
      | Fetch_add, Loc -> Printf.sprintf "Fetch_add(Loc, %d)" arg
      | Add, Field -> Printf.sprintf "Add(Field, %d)" arg
      | Add, Loc -> Printf.sprintf "Add(Loc, %d)" arg
      | Sub, Field -> Printf.sprintf "Sub(Field, %d)" arg
      | Sub, Loc -> Printf.sprintf "Sub(Loc, %d)" arg
      | Land, Field -> Printf.sprintf "Land(Field, %d)" arg
      | Land, Loc -> Printf.sprintf "Land(Loc, %d)" arg
      | Lor, Field -> Printf.sprintf "Lor(Field, %d)" arg
      | Lor, Loc -> Printf.sprintf "Lor(Loc, %d)" arg
      | Lxor, Field -> Printf.sprintf "Lxor(Field, %d)" arg
      | Lxor, Loc -> Printf.sprintf "Lxor(Loc, %d)" arg
    in
    Format.printf "FAIL: op %s failed\n" op_descr;
    Format.printf "    old_value: %d\n" outcome.old_value;
    Format.printf "%s   new_value: %d (expected %d)\n"
      (if value_ok then " " else "X")
      outcome.new_value
      expected_val;
    Format.printf "%s   result: %s (expected %s)\n"
      (if result_ok then " " else "X")
      (match op with
      | Load -> string_of_int outcome.result
      | Set -> "()"
      | Exchange -> string_of_int outcome.result
      | Fetch_add -> string_of_int outcome.result
      | Add | Sub | Land | Lor | Lxor -> "()")
      (match op with
      | Load -> string_of_int expected_res
      | Set | Add | Sub | Land | Lor | Lxor -> "()"
      | Exchange -> string_of_int expected_res
      | Fetch_add -> string_of_int expected_res
      );
    false
  ) else
    true

let test_all_ops_with_random_args ?(num_tests = 5) () =
  Format.printf "Testing all atomic operations with random arguments...\n";
  let targets = [Field; Loc] in
  let all_passed = ref true in
  List.iter (fun target ->
    for test_num = 1 to num_tests do
      List.iter (fun (Op op) ->
        let t = { first_field = 0; field = Random.int 100 } in
        let success = match op with
          | Load -> test_op t target op ~arg:()
          | Set -> test_op t target op ~arg:(generate_random_arg ())
          | Exchange -> test_op t target op ~arg:(generate_random_arg ())
          | Fetch_add -> test_op t target op ~arg:(generate_random_arg ())
          | Add -> test_op t target op ~arg:(generate_random_arg ())
          | Sub -> test_op t target op ~arg:(generate_random_arg ())
          | Land -> test_op t target op ~arg:(generate_random_arg ())
          | Lor -> test_op t target op ~arg:(generate_random_arg ())
          | Lxor -> test_op t target op ~arg:(generate_random_arg ())
        in
        if not success then all_passed := false
      ) all_ops
    done
  ) targets;

  if !all_passed then
    Format.printf "\nAll tests passed!\n%!"
  else
    Format.printf "\nSome tests failed!\n%!"
[%%expect{|
type op_packed = Op : ('a, 'r) op -> op_packed
type target = Field | Loc
type 'r outcome = { old_value : int; new_value : int; result : 'r; }
val run : t -> target -> ('a, 'r) op -> arg:'a -> 'r outcome = <fun>
val expected_result : ('a, 'r) op -> arg:'a -> old_value:int -> 'r = <fun>
val expected_value : ('a, 'r) op -> arg:'a -> old_value:int -> int = <fun>
val all_ops : op_packed list =
  [Op Load; Op Set; Op Exchange; Op Fetch_add; Op Add; Op Sub; Op Land;
   Op Lor; Op Lxor]
val generate_random_ops : ?length:int -> unit -> op_packed list = <fun>
val generate_random_arg : unit -> int = <fun>
val test_op : t -> target -> ('a, 'r) op -> arg:'a -> bool = <fun>
val test_all_ops_with_random_args : ?num_tests:int -> unit -> unit = <fun>
|}]

let () = Random.init 123456
let () = test_all_ops_with_random_args ~num_tests:100 ()

[%%expect{|
Testing all atomic operations with random arguments...

All tests passed!
|}]
