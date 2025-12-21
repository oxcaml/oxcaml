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
  (f/301 =
     (function {nlocal = 0} x/303 : int
       (if (field_int 0 x/303)
         (let (*match*/307 =o? (field_mut 1 x/303))
           (if *match*/307
             (if (seq (setfield_ptr 1 x/303 0) 0) 2
               (let (*match*/308 =o? (field_mut 1 x/303))
                 (field_imm 0 *match*/308)))
             1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/301))
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
  (f/314 =
     (function {nlocal = 0} x/315 : int
       (if (field_int 0 x/315)
         (let (*match*/319 =o? (field_mut 1 x/315))
           (if *match*/319 (field_imm 0 *match*/319) 1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/314))
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
  (f/321 =
     (function {nlocal = 0} r/322 : int
       (region
         (let
           (*match*/324 =[value<(consts (0)) (non_consts ([0: ?]))>]
              (makelocalblock 0 (*) r/322))
           (catch
             (if *match*/324
               (let (*match*/326 =o? (field_mut 0 (field_imm 0 *match*/324)))
                 (if *match*/326 (exit 7) 0))
               (exit 7))
            with (7)
             (if (seq (setfield_ptr 0 r/322 0) 0) 1
               (if *match*/324
                 (let
                   (*match*/328 =o? (field_mut 0 (field_imm 0 *match*/324)))
                   (field_imm 0 *match*/328))
                 3)))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/321))
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
  (test/333 =
     (function {nlocal = 0}
       param/336[value<(consts (0)) (non_consts ([0: ?]))>] : int
       (if param/336 (field_imm 0 (field_imm 0 param/336)) 0)))
  (apply (field_imm 1 (global Toploop!)) "test" test/333))
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
  (test/342 =
     (function {nlocal = 0} param/344 : int
       (let (*match*/345 =o? (field_mut 0 param/344))
         (if *match*/345 (field_imm 0 (field_imm 0 *match*/345)) 0))))
  (apply (field_imm 1 (global Toploop!)) "test" test/342))
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
  (test/351 =
     (function {nlocal = 0} n/352? : int
       (region
         (let
           (*match*/355 =[value<(consts (0)) (non_consts ([0: ?]))>]
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
           (if *match*/355
             (let
               (*match*/356 =a? (field_imm 0 *match*/355)
                *match*/358 =o? (field_mut 0 (field_imm 0 *match*/356)))
               (if *match*/358 (field_imm 0 (field_imm 1 *match*/356))
                 (%int_neg (field_imm 0 (field_imm 1 *match*/356)))))
             3)))))
  (apply (field_imm 1 (global Toploop!)) "test" test/351))
val test : 'a -> int = <fun>
|}]
