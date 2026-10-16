(* TEST
 flags = "-dlambda -dcanonical-ids";
 include stdlib_stable;
 expect;
*)

(* Address-pattern variants of the tests in
   [testsuite/tests/match-side-effects/partiality.ml]. *)
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
             (seq ((set_ptr value<(consts (0)) (non_consts ([0: ?]))>) a/0 0)
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
