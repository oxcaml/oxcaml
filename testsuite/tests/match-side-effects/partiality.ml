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
  (f/294 =
     (function {nlocal = 0} x/296 : int
       (if (field_int 0 x/296)
         (let (*match*/300 =o? (field_mut 1 x/296))
           (if *match*/300
             (if (seq (setfield_ptr 1 x/296 0) 0) 2
               (let (*match*/301 =o? (field_mut 1 x/296))
                 (field_imm 0 *match*/301)))
             1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/294))
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
  (f/307 =
     (function {nlocal = 0} x/308 : int
       (if (field_int 0 x/308)
         (let (*match*/312 =o? (field_mut 1 x/308))
           (if *match*/312 (field_imm 0 *match*/312) 1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f" f/307))
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
  (f/314 =
     (function {nlocal = 0} r/315 : int
       (region
         (let
           (*match*/317 =[value<(consts (0)) (non_consts ([0: ?]))>]
              (makelocalblock 0 (*) r/315))
           (catch
             (if *match*/317
               (let (*match*/319 =o? (field_mut 0 (field_imm 0 *match*/317)))
                 (if *match*/319 (exit 7) 0))
               (exit 7))
            with (7)
             (if (seq (setfield_ptr 0 r/315 0) 0) 1
               (if *match*/317
                 (let
                   (*match*/321 =o? (field_mut 0 (field_imm 0 *match*/317)))
                   (field_imm 0 *match*/321))
                 3)))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/314))
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
  (test/326 =
     (function {nlocal = 0}
       param/329[value<(consts (0)) (non_consts ([0: ?]))>] : int
       (if param/329 (field_imm 0 (field_imm 0 param/329)) 0)))
  (apply (field_imm 1 (global Toploop!)) "test" test/326))
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
  (test/335 =
     (function {nlocal = 0} param/337 : int
       (let (*match*/338 =o? (field_mut 0 param/337))
         (if *match*/338 (field_imm 0 (field_imm 0 *match*/338)) 0))))
  (apply (field_imm 1 (global Toploop!)) "test" test/335))
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
  (test/344 =
     (function {nlocal = 0} n/345? : int
       (region
         (let
           (*match*/348 =[value<(consts (0)) (non_consts ([0: ?]))>]
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
           (if *match*/348
             (let
               (*match*/349 =a? (field_imm 0 *match*/348)
                *match*/351 =o? (field_mut 0 (field_imm 0 *match*/349)))
               (if *match*/351 (field_imm 0 (field_imm 1 *match*/349))
                 (~ (field_imm 0 (field_imm 1 *match*/349)))))
             3)))))
  (apply (field_imm 1 (global Toploop!)) "test" test/344))
val test : 'a -> int = <fun>
|}]
