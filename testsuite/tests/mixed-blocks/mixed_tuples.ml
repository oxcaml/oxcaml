(* TEST
 flambda2;
 { expect; expect.opt; }
*)

(* basics *)

type t1 = int * #(string * bool)
[%%expect{|
type t1 = int * #(string * bool)
|}]

let reconstruct_t1 : t1 =
  match 4, #("hi", false) with
  | x, #(y, z) -> x, #(y, z)
[%%expect{|
val reconstruct_t1 : t1 = (4, #("hi", false))
|}]

type t_void : void

type t2 = t_void * t_void * int
let make_t2 (v : t_void) = (v, v, 42)
[%%expect{|
type t_void : void
type t2 = t_void * t_void * int
val make_t2 : t_void -> t_void * t_void * int = <fun>
|}]

type t3 = int * bool# * #((unit * string) * unit#)
let reconstruct_t3 : t3 =
  match (42, #true, #(((), "hi"), #())) with
  | (a, b, #((c, d), e)) -> (a, b, #((c, d), e))
[%%expect{|
type t3 = int * bool# * #((unit * string) * unit#)
val reconstruct_t3 : t3 = (42, <abstr>, #(((), "hi"), <abstr>))
|}]

(* CR zeisbach: should this pass? what tests are needed for this behavior?
   I guess it's cool if it just works, but I'm very low confidence. I should
   look at Will's tests once that gets merged. *)
type all_void = t_void * t_void
let make_all_void (v : t_void) = (v, v)
[%%expect{|
type all_void = t_void * t_void
val make_all_void : t_void -> t_void * t_void = <fun>
|}]

type ('a : any) t4 = 'a * int
let v4_uniform : int t4 = (6, 7)
let v4_mixed : float# t4 = (#6.0, 7)
[%%expect{|
type all_void = t_void * t_void
val make_all_void : t_void -> t_void * t_void = <fun>
|}]

(* There is a cap on the number of elements in the scannable prefix. The error
   is reported when the mixed tuple is built, since [t_capped] doesn't get a
   decl. *)

(* CR zeisbach: can we do any better than this? *)
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

(* regression test: partial pattern matching counterexample generation *)

let partial_match_t3_none : t3 option -> unit = function
  | None -> ()
[%%expect{|
Lines 1-2, characters 48-14:
1 | ................................................function
2 |   | None -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Some (_, _, _)"

val partial_match_t3_none : t3 option -> unit = <fun>
|}]

let partial_match_t3_some : t3 option -> unit = function
  | Some (3, #true, _) -> ()
[%%expect{|
Lines 1-2, characters 48-28:
1 | ................................................function
2 |   | Some (3, #true, _) -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Some (3, #false, _)"

val partial_match_t3_some : t3 option -> unit = <fun>
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
  (#(n + 1, s), b)
[%%expect{|
val letop_mixed : #(int * string) * bool = (#(5, "hi"), true)
|}]
