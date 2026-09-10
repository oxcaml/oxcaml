(* TEST
 flambda2;
 { expect; expect.opt; }
*)

(* CR zeisbach: maybe we should try to do better than this. *)
(* There is a cap on the number of elements in the scannable prefix. The error
   is reported when the mixed tuple is built, since [t_capped] doesn't get a
   decl. *)
type ptr = string
type t_capped =
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    float#
[%%expect{|
type ptr = string
type t_capped =
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * float#
|}]

let p : ptr = "p"
let capped : t_capped =
  (
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    #1.0
  )
[%%expect{|
val p : ptr = "p"
Lines 3-21, characters 2-3:
 3 | ..(
 4 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
 5 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
 6 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
 7 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
...
18 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
19 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
20 |     #1.0
21 |   )
Error: Mixed tuples may contain at most 254 value fields prior to the flat suffix, but this one contains 255.
|}]

type ('a : any) t_any_capped =
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    'a
[%%expect{|
type ('a : any) t_any_capped =
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr * ptr *
    ptr * ptr * 'a
|}]

let any_capped_ok : int t_any_capped =
  (
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    42
  )
[%%expect{|
val any_capped_ok : int t_any_capped =
  ("p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p", "p",
   42)
|}]

let float_u_capped_err : float# t_any_capped =
  (
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
    #1.0
  )
[%%expect{|
Lines 2-20, characters 2-3:
 2 | ..(
 3 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
 4 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
 5 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
 6 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
...
17 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
18 |     p, p, p, p, p, p, p, p, p, p, p, p, p, p, p,
19 |     #1.0
20 |   )
Error: Mixed tuples may contain at most 254 value fields prior to the flat suffix, but this one contains 255.
|}]

(* CR zeisbach: add a similar case but with [any] once Joe's PR lands *)

(* regression test: partial pattern matching counterexample generation *)

type t = int * float# * #((unit * string) * unit#)

let partial_match_t3_none : t option -> unit = function
  | None -> ()
[%%expect{|
type t = int * float# * #((unit * string) * unit#)
Lines 3-4, characters 47-14:
3 | ...............................................function
4 |   | None -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Some (_, _, _)"

val partial_match_t3_none : t option -> unit = <fun>
|}]

let partial_match_t3_some : t option -> unit = function
  | Some (6, #7.0, _) -> ()
[%%expect{|
Lines 1-2, characters 47-27:
1 | ...............................................function
2 |   | Some (6, #7.0, _) -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Some (6, #0., _)"

val partial_match_t3_some : t option -> unit = <fun>
|}]


(* GADT partial matches with mixed tuples of different kinds. *)

module M = struct
  type _ repr = R1 : (float# * string) repr | R2 : (float * string) repr

  let f (type a) (r1 : a repr) (r2 : a repr) (a : a) =
    match r1, r2, a with
    | R1, _, (#3.0, "") -> ()
    | _, R2, (1.0, "") -> ()
end
[%%expect{|
Lines 5-7, characters 4-28:
5 | ....match r1, r2, a with
6 |     | R1, _, (#3.0, "") -> ()
7 |     | _, R2, (1.0, "") -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "(R1, R1, (#3.0, "*"))"

module M :
  sig
    type _ repr = R1 : (float# * string) repr | R2 : (float * string) repr
    val f : 'a repr -> 'a repr -> 'a -> unit
  end
|}]

(* [let*] and [and*] desugar into a mixed tuple. *)

let letop_mixed =
  let ( let* ) (p : #(int * string) * bool) f = f p in
  let ( and* ) (x : #(int * string)) (y : bool) = (x, y) in
  let* #(n, s) = #(4, "hi")
  and* b = true
  in
  (n, s, b)
[%%expect{|
val letop_mixed : int * string * bool = (4, "hi", true)
|}]

(* value_rec_compiler checks *)
