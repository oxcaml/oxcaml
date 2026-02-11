(* TEST
<<<<<<< HEAD
 flags = "-dlambda";
 stack-allocation;
||||||| 23e84b8c4d
 flags = "-dlambda";
=======
 flags = "-dlambda -dcanonical-ids";
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
 expect;
*)

(* We explicitly enable the warning (see the discussion in the
   "Warning reference" section of the reference manual), which makes
   it clear which examples have been intentionally pessimized by the
   compiler. *)
#warnings "+degraded-to-partial-match";;
[%%expect {|
|}];;

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

   PASS: the second access includes a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
Lines 4-8, characters 2-32:
4 | ..match x with
5 |   | {a = false; b = _} -> 0
6 |   | {a = _;     b = None} -> 1
7 |   | {a = _;     b = _} when (x.b <- None; false) -> 2
8 |   | {a = true;  b = Some y} -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
<<<<<<< HEAD
  (f/290 =
     (function {nlocal = 0} x/292 : int
       (if (field_int 0 x/292)
         (let (*match*/296 =o? (field_mut 1 x/292))
           (if *match*/296
             (if (seq (setfield_ptr 1 x/292 0) 0) 2
               (let (*match*/297 =o? (field_mut 1 x/292))
                 (field_imm 0 *match*/297)))
||||||| 23e84b8c4d
  (f/280 =
     (function x/282 : int
       (if (field_int 0 x/282)
         (let (*match*/286 =o (field_mut 1 x/282))
           (if *match*/286
             (if (seq (setfield_ptr 1 x/282 0) 0) 2
               (let (*match*/287 =o (field_mut 1 x/282))
                 (field_imm 0 *match*/287)))
=======
  (f/0 =
     (function x/0 : int
       (if (field_int 0 x/0)
         (let (*match*/0 =o (field_mut 1 x/0))
           (if *match*/0
             (if (seq (setfield_ptr 1 x/0 0) 0) 2
               (let (*match*/1 =o (field_mut 1 x/0))
                 (if *match*/1 (field_imm 0 *match*/1)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 4 2])))))
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
             1))
         0)))
<<<<<<< HEAD
  (apply (field_imm 1 (global Toploop!)) "f" f/290))
||||||| 23e84b8c4d
  (apply (field_mut 1 (global Toploop!)) "f" f/280))
=======
  (apply (field_mut 1 (global Toploop!)) "f" f/0))

>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
val f : t -> int = <fun>
|}]



(* A simple example of a complete switch
   inside a mutable position. *)
type t = {a: bool; mutable b: int option}

let simple x =
  match x with
  | {b = None} -> 1
  | {b = Some y} -> y
;;
(* Performance expectation: there should not be a Match_failure case. *)
[%%expect {|
0
type t = { a : bool; mutable b : int option; }
(let
<<<<<<< HEAD
  (f/303 =
     (function {nlocal = 0} x/304 : int
       (if (field_int 0 x/304)
         (let (*match*/308 =o? (field_mut 1 x/304))
           (if *match*/308 (field_imm 0 *match*/308) 1))
||||||| 23e84b8c4d
  (f/291 =
     (function x/292 : int
       (if (field_int 0 x/292)
         (let (*match*/296 =o (field_mut 1 x/292))
           (if *match*/296 (field_imm 0 *match*/296) 1))
=======
  (simple/0 =
     (function x/1 : int
       (let (*match*/2 =o (field_mut 1 x/1))
         (if *match*/2 (field_imm 0 *match*/2) 1))))
  (apply (field_mut 1 (global Toploop!)) "simple" simple/0))
val simple : t -> int = <fun>
|}]

(* This more complex case has the switch on [b] split across two cases
   on [a], so it may need a [Match_failure] for soundness -- it does
   if the two accesses to [b] are done on different reads of the same
   mutable field.

   PASS: two reads of [field_mut 1 x], and a Match_failure case. *)
let f x =
  match x with
  | {a = false; b = _} -> 0
  | {a = _;     b = None} -> 1
  | {a = true;  b = Some y} -> y
;;
[%%expect {|
Lines 2-5, characters 2-32:
2 | ..match x with
3 |   | {a = false; b = _} -> 0
4 |   | {a = _;     b = None} -> 1
5 |   | {a = true;  b = Some y} -> y
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (f/1 =
     (function x/2 : int
       (if (field_int 0 x/2)
         (let (*match*/3 =o (field_mut 1 x/2))
           (if *match*/3 (field_imm 0 *match*/3)
             (let (*match*/4 =o (field_mut 1 x/2))
               (if *match*/4
                 (raise (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))
                 1))))
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
         0)))
<<<<<<< HEAD
  (apply (field_imm 1 (global Toploop!)) "f" f/303))
||||||| 23e84b8c4d
  (apply (field_mut 1 (global Toploop!)) "f" f/291))
=======
  (apply (field_mut 1 (global Toploop!)) "f" f/1))

>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
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
   (field_mut 0) access, or the second access should include
   a Match_failure case.

   PASS: two different reads (field_mut 0), and a Match_failure case. *)
[%%expect {|
Lines 2-6, characters 2-13:
2 | ..match Some r with
3 |   | Some { contents = None } -> 0
4 |   | _ when (r := None; false) -> 1
5 |   | Some { contents = Some n } -> n
6 |   | None -> 3
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
<<<<<<< HEAD
  (f/310 =
     (function {nlocal = 0} r/311 : int
       (region
         (let
           (*match*/313 =[value<(consts (0)) (non_consts ([0: ?]))>]
              (makelocalblock 0 (*) r/311))
           (catch
             (if *match*/313
               (let (*match*/315 =o? (field_mut 0 (field_imm 0 *match*/313)))
                 (if *match*/315 (exit 7) 0))
               (exit 7))
            with (7)
             (if (seq (setfield_ptr 0 r/311 0) 0) 1
               (if *match*/313
                 (let
                   (*match*/317 =o? (field_mut 0 (field_imm 0 *match*/313)))
                   (field_imm 0 *match*/317))
                 3)))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/310))
||||||| 23e84b8c4d
  (f/298 =
     (function r/299 : int
       (let (*match*/301 = (makeblock 0 r/299))
         (catch
           (if *match*/301
             (let (*match*/303 =o (field_mut 0 (field_imm 0 *match*/301)))
               (if *match*/303 (exit 7) 0))
             (exit 7))
          with (7)
           (if (seq (setfield_ptr 0 r/299 0) 0) 1
             (if *match*/301
               (let (*match*/305 =o (field_mut 0 (field_imm 0 *match*/301)))
                 (field_imm 0 *match*/305))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/298))
=======
  (f/2 =
     (function r/0 : int
       (let (*match*/5 = (makeblock 0 r/0))
         (catch
           (if *match*/5
             (let (*match*/6 =o (field_mut 0 (field_imm 0 *match*/5)))
               (if *match*/6 (exit 13) 0))
             (exit 13))
          with (13)
           (if (seq (setfield_ptr 0 r/0 0) 0) 1
             (if *match*/5
               (let (*match*/7 =o (field_mut 0 (field_imm 0 *match*/5)))
                 (if *match*/7 (field_imm 0 *match*/7)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/2))

>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
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
<<<<<<< HEAD
  (test/321 =
     (function {nlocal = 0}
       param/324[value<(consts (0)) (non_consts ([0: ?]))>] : int
       (if param/324 (field_imm 0 (field_imm 0 param/324)) 0)))
  (apply (field_imm 1 (global Toploop!)) "test" test/321))
||||||| 23e84b8c4d
  (test/309 =
     (function param/312 : int
       (if param/312 (field_imm 0 (field_imm 0 param/312)) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/309))
=======
  (test/0 =
     (function param/0 : int
       (if param/0 (field_imm 0 (field_imm 0 param/0)) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/0))
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
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
<<<<<<< HEAD
  (test/329 =
     (function {nlocal = 0} param/331 : int
       (let (*match*/332 =o? (field_mut 0 param/331))
         (if *match*/332 (field_imm 0 (field_imm 0 *match*/332)) 0))))
  (apply (field_imm 1 (global Toploop!)) "test" test/329))
||||||| 23e84b8c4d
  (test/317 =
     (function param/319 : int
       (let (*match*/320 =o (field_mut 0 param/319))
         (if *match*/320 (field_imm 0 (field_imm 0 *match*/320)) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/317))
=======
  (test/1 =
     (function param/1 : int
       (let (*match*/8 =o (field_mut 0 param/1))
         (if *match*/8 (field_imm 0 (field_imm 0 *match*/8)) 0))))
  (apply (field_mut 1 (global Toploop!)) "test" test/1))
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
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
<<<<<<< HEAD
  (test/337 =
     (function {nlocal = 0} n/338? : int
       (region
         (let
           (*match*/341 =[value<(consts (0)) (non_consts ([0: ?]))>]
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
           (if *match*/341
             (let
               (*match*/342 =a? (field_imm 0 *match*/341)
                *match*/344 =o? (field_mut 0 (field_imm 0 *match*/342)))
               (if *match*/344 (field_imm 0 (field_imm 1 *match*/342))
                 (%int_neg (field_imm 0 (field_imm 1 *match*/342)))))
             3)))))
  (apply (field_imm 1 (global Toploop!)) "test" test/337))
||||||| 23e84b8c4d
  (test/325 =
     (function n/326 : int
       (let
         (*match*/329 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/329
           (let
             (*match*/330 =a (field_imm 0 *match*/329)
              *match*/332 =o (field_mut 0 (field_imm 0 *match*/330)))
             (if *match*/332 (field_imm 0 (field_imm 1 *match*/330))
               (~ (field_imm 0 (field_imm 1 *match*/330)))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/325))
=======
  (test/2 =
     (function n/0 : int
       (let
         (*match*/9 =
            (makeblock 0 (makeblock 0 (makemutable 0 (int) 1) [0: 42])))
         (if *match*/9
           (let
             (*match*/10 =a (field_imm 0 *match*/9)
              *match*/11 =o (field_mut 0 (field_imm 0 *match*/10)))
             (if *match*/11 (field_imm 0 (field_imm 1 *match*/10))
               (~ (field_imm 0 (field_imm 1 *match*/10)))))
           3))))
  (apply (field_mut 1 (global Toploop!)) "test" test/2))
>>>>>>> d505d53be15ca18a648496b70604a7b4db15db2a
val test : 'a -> int = <fun>
|}]



(* In this example, the constructor on which unsound assumptions could
   be made is not located directly below a mutable constructor, but
   one level deeper inside an immutable pair constructor (below the
   mutable constructor). This checks that there is a form of
   "transitive" propagation of mutability.

   Correctness condition: either there is a single mutable field read,
   or the accesses below the second mutable read have a Match_failure
   case.
*)
let deep r =
  match Some r with
  | Some { contents = ((), None) } -> 0
  | _ when (r := ((), None); false) -> 1
  | Some { contents = ((), Some n) } -> n
  | None -> 3
;;
(* PASS: two different reads (field_mut 0), and a Match_failure case. *)
[%%expect {|
Lines 2-6, characters 2-13:
2 | ..match Some r with
3 |   | Some { contents = ((), None) } -> 0
4 |   | _ when (r := ((), None); false) -> 1
5 |   | Some { contents = ((), Some n) } -> n
6 |   | None -> 3
Warning 74 [degraded-to-partial-match]: This pattern-matching is compiled as
  partial, even if it appears to be total. It may generate a "Match_failure"
  exception. This typically occurs due to complex matches on mutable fields.
  (see manual section 13.5.5)
(let
  (deep/0 =
     (function r/1 : int
       (let (*match*/12 = (makeblock 0 r/1))
         (catch
           (if *match*/12
             (let (*match*/13 =o (field_mut 0 (field_imm 0 *match*/12)))
               (if (field_imm 1 *match*/13) (exit 21) 0))
             (exit 21))
          with (21)
           (if (seq (setfield_ptr 0 r/1 [0: 0 0]) 0) 1
             (if *match*/12
               (let
                 (*match*/14 =o (field_mut 0 (field_imm 0 *match*/12))
                  *match*/15 =a (field_imm 1 *match*/14))
                 (if *match*/15 (field_imm 0 *match*/15)
                   (raise
                     (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))
               3))))))
  (apply (field_mut 1 (global Toploop!)) "deep" deep/0))

val deep : (unit * int option) ref -> int = <fun>
|}]


(* In this example:
   - the pattern-matching is total, with subtle GADT usage
     (only the type-checker can tell that it is Total)
   - there are no mutable fields

   Performance expectation: there should not be a Match_failure clause.

   This example is a reduction of a regression caused by #13076 on the
   'CamlinternalFormat.trans' function in the standard library.
*)
type _ t = Bool : bool t | Int : int t | Char : char t;;
let test : type a . a t * a t -> unit = function
  | Int, Int -> ()
  | Bool, Bool -> ()
  | _, Char -> ()
;;
(* PASS: no Match_failure clause generated. *)
[%%expect {|
0
type _ t = Bool : bool t | Int : int t | Char : char t
(let
  (test/3 =
     (function param/2 : int
       (catch
         (if (>= (field_imm 0 param/2) 2) (exit 24)
           (if (>= (field_imm 1 param/2) 2) (exit 24) 0))
        with (24) 0)))
  (apply (field_mut 1 (global Toploop!)) "test" test/3))
val test : 'a t * 'a t -> unit = <fun>
|}];;

(* Another regression testcase from #13076, proposed by Nick Roberts.

   Performance expectation: no Match_failure clause.
*)
type nothing = |
type t = A | B | C of nothing
let f : bool * t -> int = function
  | true, A -> 3
  | false, A -> 4
  | _, B -> 5
  | _, C _ -> .
(* PASS: no Match_failure clause generated. *)
[%%expect {|
0
type nothing = |
0
type t = A | B | C of nothing
(let
  (f/3 =
     (function param/3 : int
       (catch
         (if (field_imm 0 param/3)
           (switch* (field_imm 1 param/3)
            case int 0: 3
            case int 1: (exit 27))
           (switch* (field_imm 1 param/3)
            case int 0: 4
            case int 1: (exit 27)))
        with (27) 5)))
  (apply (field_mut 1 (global Toploop!)) "f" f/3))
val f : bool * t -> int = <fun>
|}];;


(* Another regression testcase from #13076, proposed by Nick Roberts.

   Performance expectation: no Match_failure clause.
*)
type t =
  | A of int
  | B of string
  | C of string
  | D of string

let compare t1 t2 =
  match t1, t2 with
  | A i, A j -> Int.compare i j
  | B l1, B l2 -> String.compare l1 l2
  | C l1, C l2 -> String.compare l1 l2
  | D l1, D l2 -> String.compare l1 l2
  | A _, (B _ | C _ | D _ ) -> -1
  | (B _ | C _ | D _ ), A _ -> 1
  | B _, (C _ | D _) -> -1
  | (C _ | D _), B _ -> 1
  | C _, D _ -> -1
  | D _, C _ -> 1
(* PASS: no Match_failure clause generated. *)
[%%expect {|
0
type t = A of int | B of string | C of string | D of string
(let
  (compare/0 =
     (function t1/0 t2/0 : int
       (catch
         (switch* t1/0
          case tag 0:
           (switch t2/0
            case tag 0:
             (apply (field_imm 8 (global Stdlib__Int!)) (field_imm 0 t1/0)
               (field_imm 0 t2/0))
            default: -1)
          case tag 1:
           (catch
             (switch* t2/0
              case tag 0: (exit 31)
              case tag 1:
               (apply (field_imm 9 (global Stdlib__String!))
                 (field_imm 0 t1/0) (field_imm 0 t2/0))
              case tag 2: (exit 36)
              case tag 3: (exit 36))
            with (36) -1)
          case tag 2:
           (switch* t2/0
            case tag 0: (exit 31)
            case tag 1: (exit 31)
            case tag 2:
             (apply (field_imm 9 (global Stdlib__String!)) (field_imm 0 t1/0)
               (field_imm 0 t2/0))
            case tag 3: -1)
          case tag 3:
           (switch* t2/0
            case tag 0: (exit 31)
            case tag 1: (exit 31)
            case tag 2: 1
            case tag 3:
             (apply (field_imm 9 (global Stdlib__String!)) (field_imm 0 t1/0)
               (field_imm 0 t2/0))))
        with (31) (switch* t2/0 case tag 0: 1
                                case tag 1: 1))))
  (apply (field_mut 1 (global Toploop!)) "compare" compare/0))
val compare : t -> t -> int = <fun>
|}];;


(* Different testcases involving or-patterns and polymorphic variants,
   proposed by Nick Roberts. In both cases, we do *not* expect a Match_failure case. *)

let f x y =
 match x, y with
 | _, `Y1 -> 0
 | `X1, `Y2 -> 1
 | (`X2 | `X3), `Y3 -> 2
 | `X1, `Y3
 | `X2, `Y2
 | `X3, _  -> 3
(* PASS: no Match_failure generated *)
[%%expect {|
(let
  (f/4 =
     (function x/3[int] y/0[int] : int
       (catch
         (catch
           (catch (if (isint y/0) (if (!= y/0 19896) (exit 45) 0) (exit 45))
            with (45)
             (if (!= x/3 19674)
               (if (>= x/3 19675) (exit 44) (if (>= y/0 19898) (exit 42) 1))
               (if (isint y/0) (if (!= y/0 19897) (exit 44) (exit 42))
                 (exit 44))))
          with (44)
           (if (isint y/0) (if (!= y/0 19898) (exit 42) 2) (exit 42)))
        with (42) 3)))
  (apply (field_mut 1 (global Toploop!)) "f" f/4))
val f : [< `X1 | `X2 | `X3 ] -> [< `Y1 | `Y2 | `Y3 ] -> int = <fun>
|}];;


let check_results r1 r2 =
  match r1 r2 with
  | (Ok _ as r), _ | _, (Ok _ as r) -> r
  | (Error `A as r), Error _
  | Error _, (Error `A as r) -> r
  | (Error `B as r), Error `B -> r
(* PASS: no Match_failure case generated *)
[%%expect {|
(let
  (check_results/0 =
     (function r1/0 r2/0
       (let (*match*/16 = (apply r1/0 r2/0))
         (catch
           (catch
             (let (r/2 =a (field_imm 0 *match*/16))
               (catch
                 (switch* r/2
                  case tag 0: (exit 50 r/2)
                  case tag 1:
                   (catch
                     (if (>= (field_imm 0 r/2) 66)
                       (let (*match*/17 =a (field_imm 1 *match*/16))
                         (switch* *match*/17
                          case tag 0: (exit 52)
                          case tag 1:
                           (let (*match*/18 =a (field_imm 0 *match*/17))
                             (if (isint *match*/18)
                               (if (!= *match*/18 66) (exit 53) r/2)
                               (exit 53)))))
                       (switch* (field_imm 1 *match*/16)
                        case tag 0: (exit 52)
                        case tag 1: (exit 51 r/2)))
                    with (53) (exit 51 (field_imm 1 *match*/16))))
                with (52) (exit 50 (field_imm 1 *match*/16))))
            with (50 r/3) r/3)
          with (51 r/4) r/4))))
  (apply (field_mut 1 (global Toploop!)) "check_results" check_results/0))
val check_results :
  ('a -> ('b, [< `A | `B ]) result * ('b, [< `A | `B ]) result) ->
  'a -> ('b, [> `A | `B ]) result = <fun>
|}];;
