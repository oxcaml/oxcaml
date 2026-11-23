(* TEST
 flags = "-dlambda -dcanonical-ids";
 include stdlib_stable;
 expect;
*)

(* Variants of the tests in [testsuite/tests/match-side-effects/partiality.ml]
   using address patterns and dereference patterns. *)
#warnings "+degraded-to-partial-match";;
[%%expect {|
|}];;

open Stdlib_stable;;
[%%expect {|
0
|}];;

let f (a : int option addr) b =
  match #(b, a) with
  | #(false, addr_ None) -> -1
  | #(true, addr_ None) -> 0
  | #(_, _) when (Addr.set a None; false) -> 1
  | #(_, addr_ (Some n)) -> n
;;
(* Correctness condition: there should either be a single
   (get_ptr) access, or the second access should include
   a Match_failure case.

   PASS: the second access includes a Match_failure case. *)
[%%expect {|
Lines 2-6, characters 2-29:
2 | ..match #(b, a) with
3 |   | #(false, addr_ None) -> -1
4 |   | #(true, addr_ None) -> 0
5 |   | #(_, _) when (Addr.set a None; false) -> 1
6 |   | #(_, addr_ (Some n)) -> n
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/0 =
     (function {nlocal = 0} a/0[#(?, int64)] b/0[value<int>] : int
       (let
         (*match*/0 =[#(?, #(?, int64))]
            (make_unboxed_product #(value<int>, #(?, int64)) b/0 a/0))
         (catch
           (if (unboxed_product_field 0 #(?, #(?, int64)) *match*/0)
             (let
               (*match*/1 =o?
                  ((get_ptr ?)
                    (unboxed_product_field 1 #(?, #(?, int64)) *match*/0)))
               (if *match*/1 (exit 2) 0))
             (let
               (*match*/2 =o?
                  ((get_ptr ?)
                    (unboxed_product_field 1 #(?, #(?, int64)) *match*/0)))
               (if *match*/2 (exit 2) -1)))
          with (2)
           (if
             (seq
               ((set_ptr value<(consts (0)) (non_consts ([0: value<int>]))>)
                 a/0 0)
               0)
             1
             (let
               (*match*/3 =o?
                  ((get_ptr ?)
                    (unboxed_product_field 1 #(?, #(?, int64)) *match*/0)))
               (if *match*/3 (field_imm 0 *match*/3)
                 (raise
                   (makeblock 0 (getpredef Match_failure/0!!) [0: "" 2 2])))))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/0))

val f : int option addr -> bool -> int = <fun>
|}]

(* A simple example of a complete switch
   on the value at a mutable address. *)
let simple (a : int option addr) =
  match a with
  | addr_ None -> 0
  | addr_ (Some n) -> n
;;
(* Performance expectation: there should not be a Match_failure case.

   PASS: a single (get_ptr) access, and no Match_failure case. *)
[%%expect {|
(let
  (simple/0 =
     (function {nlocal = 0} a/1[#(?, int64)] : int
       (let (*match*/4 =o? ((get_ptr ?) a/1))
         (if *match*/4 (field_imm 0 *match*/4) 0))))
  (apply (field_imm 1 (global Toploop!)) "simple" simple/0))
val simple : int option addr -> int = <fun>
|}]

(* The first example again, at an immutable address. The value at
   the address cannot change between reads, so splitting the switch
   across two cases on [b] is safe. *)
let f_imm (a : int option addr_imm) b =
  match #(b, a) with
  | #(false, addr_imm_ None) -> -1
  | #(true, addr_imm_ None) -> 0
  | #(_, _) when (print_string "guard"; false) -> 1
  | #(_, addr_imm_ (Some n)) -> n
;;
(* Performance expectation: there should not be a Match_failure case.

   PASS: several (get_ptr_imm) accesses, but no Match_failure case. *)
[%%expect {|
(let
  (f_imm/0 =
     (function {nlocal = 0} a/2[#(?, int64)] b/1[value<int>] : int
       (let
         (*match*/5 =[#(?, #(?, int64))]
            (make_unboxed_product #(value<int>, #(?, int64)) b/1 a/2))
         (catch
           (if (unboxed_product_field 0 #(?, #(?, int64)) *match*/5)
             (if
               ((get_ptr_imm ?)
                 (unboxed_product_field 1 #(?, #(?, int64)) *match*/5))
               (exit 9) 0)
             (if
               ((get_ptr_imm ?)
                 (unboxed_product_field 1 #(?, #(?, int64)) *match*/5))
               (exit 9) -1))
          with (9)
           (if (seq (apply (field_imm 41 (global Stdlib!)) "guard") 0) 1
             (field_imm 0
               ((get_ptr_imm ?)
                 (unboxed_product_field 1 #(?, #(?, int64)) *match*/5))))))))
  (apply (field_imm 1 (global Toploop!)) "f_imm" f_imm/0))
val f_imm : int option addr_imm -> bool -> int = <fun>
|}]

(* Dereference patterns *)

(* The original example of unsoundness in #7421, with the guard writing
   through an address bound by a dereference pattern. *)
type t = { a : bool; mutable b : int option }

let f_deref x =
  match x with
  | { a = false; b = _ } -> 0
  | { a = _; b = None } -> 1
  | { a = _; b = deref_ p } when (Addr.set p None; false) -> 2
  | { a = true; b = Some y } -> y
;;
(* Correctness condition: there should either be a single
   (field_mut 1) access, or the second access should include
   a Match_failure case.

   PASS: the second access includes a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
Lines 4-8, characters 2-33:
4 | ..match x with
5 |   | { a = false; b = _ } -> 0
6 |   | { a = _; b = None } -> 1
7 |   | { a = _; b = deref_ p } when (Addr.set p None; false) -> 2
8 |   | { a = true; b = Some y } -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f_deref/0 =
     (function {nlocal = 0} x/0 : int
       (if (field_int 0 x/0)
         (let (*match*/6 =o? (field_mut 1 x/0))
           (if *match*/6
             (if
               (seq
                 ((set_ptr
                   value<(consts (0)) (non_consts ([0: value<int>]))>)
                   (make_unboxed_product #(?, int64) x/0 (idx_field 1)) 0)
                 0)
               2
               (let (*match*/7 =o? (field_mut 1 x/0))
                 (if *match*/7 (field_imm 0 *match*/7)
                   (raise
                     (makeblock 0 (getpredef Match_failure/0!!) [0: "" 4 2])))))
             1))
         0)))
  (apply (field_imm 1 (global Toploop!)) "f_deref" f_deref/0))

val f_deref : t -> int = <fun>
|}]

(* Dereference patterns do not read the value at their position. *)
let simple_deref x =
  match x with
  | { b = None } -> 0
  | { b = deref_ p } -> Addr.set p None; 1
;;
(* Performance expectation: there should not be a Match_failure case.

   PASS: a single (field_mut 1) access, and no Match_failure case. *)
[%%expect {|
(let
  (simple_deref/0 =
     (function {nlocal = 0} x/1 : int
       (let (*match*/8 =o? (field_mut 1 x/1))
         (if *match*/8
           (seq
             ((set_ptr value<(consts (0)) (non_consts ([0: value<int>]))>)
               (make_unboxed_product #(?, int64) x/1 (idx_field 1)) 0)
             1)
           0))))
  (apply (field_imm 1 (global Toploop!)) "simple_deref" simple_deref/0))
val simple_deref : t -> int = <fun>
|}]

(* The first example again, with the guard writing through an address bound
   by a dereference pattern below the address pattern. *)
let f_addr_deref (a : int option addr) b =
  match #(b, a) with
  | #(false, addr_ None) -> -1
  | #(true, addr_ None) -> 0
  | #(_, addr_ (deref_ p)) when (Addr.set p None; false) -> 1
  | #(_, addr_ (Some n)) -> n
;;
(* Correctness condition: there should either be a single
   (get_ptr) access, or the second access should include
   a Match_failure case.

   PASS: the second access includes a Match_failure case. As the guarded
   row matches below the address pattern, that access happens before the
   guard runs. *)
[%%expect {|
Lines 2-6, characters 2-29:
2 | ..match #(b, a) with
3 |   | #(false, addr_ None) -> -1
4 |   | #(true, addr_ None) -> 0
5 |   | #(_, addr_ (deref_ p)) when (Addr.set p None; false) -> 1
6 |   | #(_, addr_ (Some n)) -> n
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f_addr_deref/0 =
     (function {nlocal = 0} a/3[#(?, int64)] b/2[value<int>] : int
       (let
         (*match*/9 =[#(?, #(?, int64))]
            (make_unboxed_product #(value<int>, #(?, int64)) b/2 a/3))
         (catch
           (if (unboxed_product_field 0 #(?, #(?, int64)) *match*/9)
             (let
               (*match*/10 =o?
                  ((get_ptr ?)
                    (unboxed_product_field 1 #(?, #(?, int64)) *match*/9)))
               (if *match*/10 (exit 22) 0))
             (let
               (*match*/11 =o?
                  ((get_ptr ?)
                    (unboxed_product_field 1 #(?, #(?, int64)) *match*/9)))
               (if *match*/11 (exit 22) -1)))
          with (22)
           (let
             (*match*/12 =a[#(?, int64)]
                (unboxed_product_field 1 #(?, #(?, int64)) *match*/9)
              *match*/13 =o? ((get_ptr ?) *match*/12))
             (if
               (seq
                 ((set_ptr
                   value<(consts (0)) (non_consts ([0: value<int>]))>)
                   *match*/12 0)
                 0)
               1
               (if *match*/13 (field_imm 0 *match*/13)
                 (raise
                   (makeblock 0 (getpredef Match_failure/0!!) [0: "" 2 2])))))))))
  (apply (field_imm 1 (global Toploop!)) "f_addr_deref" f_addr_deref/0))

val f_addr_deref : int option addr -> bool -> int = <fun>
|}]
