(* TEST
 flags = "-dlambda";
 stack-allocation;
 expect;
*)

(* The original example of unsoundness in #7421. *)
type t = {a: bool; mutable b: int option}

let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = _;     b = _} when (x.b <- None; false) -> 2
  | {a = true;  b = Some y} -> y
;;
(* Correctness condition: there should either be a single
   (field_mut 1) access, or the second access should include
   a Match_failure case.

   FAIL: the second occurrence of (field_mut 1) is used with a direct
   (field_imm 0) access without a constructor check. The compiler is
   unsound here. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
(let
  (f/289 =
     (function {nlocal = 0} x/291 : int
       (if (field_int 0 x/291)
         (let (*match*/295 =o? (field_mut 1 x/291))
           (if *match*/295
             (if (seq (setfield_ptr 1 x/291 0) 0) 2
               (let (*match*/296 =o? (field_mut 1 x/291))
                 (field_imm 0 *match*/296)))
             1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/289))
val f : t -> int = <fun>
|}]



(* A simple example of a complete switch
   inside a mutable position. *)
type t = {a: bool; mutable b: int option}

let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = true;  b = Some y} -> y
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
(let
  (f/302 =
     (function {nlocal = 0} x/303 : int
       (if (field_int 0 x/303)
         (let (*match*/307 =o? (field_mut 1 x/303))
           (if *match*/307 (field_imm 0 *match*/307) 1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/302))
val f : t -> int = <fun>
|}]



(* A variant of the #7421 example. *)
let f r =
  match Some r with
  | Some { contents = None } -> 0
  | _ when (r := None; false) -> 1
  | Some { contents = Some n } -> n
  | None -> 3
;;
(* Correctness condition: there should either be a single
   (field_mut 1) access, or the second access should include
   a Match_failure case.

   FAIL: the second occurrence of (field_mut 0) is used with a direct
   (field_imm 0) access without a constructor check. The compiler is
   unsound here. *)
[%%expect {|
(let
  (f/309 =
     (function {nlocal = 0} r/310 : int
       (region
         (let
           (*match*/312 =[value<(consts (0)) (non_consts ([0: ?]))>]
              (makelocalblock 0 (*) r/310))
           (catch
             (if *match*/312
               (let (*match*/314 =o? (field_mut 0 (field_imm 0 *match*/312)))
                 (if *match*/314 (exit 7) 0))
               (exit 7))
            with (7)
             (if (seq (setfield_ptr 0 r/310 0) 0) 1
               (if *match*/312
                 (let
                   (*match*/316 =o? (field_mut 0 (field_imm 0 *match*/312)))
                   (field_imm 0 *match*/316))
                 3)))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/309))
val f : int option ref -> int = <fun>
|}]



(* This example has an ill-typed counter-example: the type-checker
   finds it Total, but the pattern-matching compiler cannot see that
   (Some (Some (Bool b))) cannot occur. *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test = function
  | None -> 0
  | Some (Int n) -> n
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/320 =
     (function {nlocal = 0}
       param/323[value<(consts (0)) (non_consts ([0: ?]))>] : int
       (if param/323 (field_imm 0 (field_imm 0 param/323)) 0)))
  (apply (field_imm 1 (global Toploop!)) "test" test/320))
val test : int t option -> int = <fun>
|}]


(* This example has an ill-typed counter-example, inside
   a mutable position.  *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test = function
  | { contents = None } -> 0
  | { contents = Some (Int n) } -> n
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/328 =
     (function {nlocal = 0} param/330 : int
       (let (*match*/331 =o? (field_mut 0 param/330))
         (if *match*/331 (field_imm 0 (field_imm 0 *match*/331)) 0))))
  (apply (field_imm 1 (global Toploop!)) "test" test/328))
val test : int t option ref -> int = <fun>
|}]



(* This example has a ill-typed counter-example,
   and also mutable sub-patterns, but in different places. *)
type _ t = Int : int -> int t | Bool : bool -> bool t

let test n =
  match Some (ref true, Int 42) with
  | Some ({ contents = true }, Int n) -> n
  | Some ({ contents = false }, Int n) -> -n
  | None -> 3
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type _ t = Int : int -> int t | Bool : bool -> bool t
(let
  (test/336 =
     (function {nlocal = 0} n/337? : int
       (region
         (let
           (*match*/340 =[value<(consts (0)) (non_consts ([0: ?]))>]
              (makelocalblock 0 (value<
                                  (consts ())
                                   (non_consts ([0: *,
                                                 value<
                                                  (consts ())
                                                   (non_consts ([1:
                                                                 value<int>]
                                                   [0: value<int>]))>]))>)
                (makelocalblock 0 (*,value<
                                      (consts ())
                                       (non_consts ([1: value<int>]
                                       [0: value<int>]))>)
                  (makelocalmutable 0 (value<int>) 1) [0: 42])))
           (if *match*/340
             (let
               (*match*/341 =a? (field_imm 0 *match*/340)
                *match*/343 =o? (field_mut 0 (field_imm 0 *match*/341)))
               (if *match*/343 (field_imm 0 (field_imm 1 *match*/341))
                 (%int_neg (field_imm 0 (field_imm 1 *match*/341)))))
             3)))))
  (apply (field_imm 1 (global Toploop!)) "test" test/336))
val test : 'a -> int = <fun>
|}]
