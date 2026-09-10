(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 { expect; }
*)

(* The native toplevel prints mixed tuples as [<abstr>], so we check values
   with structural equality helpers instead. *)

module Float_u = Stdlib_upstream_compatible.Float_u

let block_kind x =
  let uniform_or_mixed = Obj.Uniform_or_mixed.of_block (Obj.repr x) in
  match Obj.Uniform_or_mixed.repr uniform_or_mixed with
  | Uniform -> "uniform"
  | Mixed { scannable_prefix_len } ->
    Printf.sprintf "mixed (scannable_prefix_len = %d)" scannable_prefix_len
[%%expect{|
module Float_u = Stdlib_upstream_compatible.Float_u
val block_kind : 'a -> string = <fun>
|}]

(* basics *)

type t1 = int * #(string * bool)

let equal_t1 ((x, #(y, z)) : t1) ((x', #(y', z')) : t1) =
  Int.equal x x' && String.equal y y' && Bool.equal z z'

let reconstruct_t1 =
  let reconstructed =
    match 4, #("hi", false) with
    | x, #(y, z) -> x, #(y, z)
  in
  assert (equal_t1 reconstructed (4, #("hi", false)));
  block_kind reconstructed
[%%expect{|
type t1 = int * #(string * bool)
val equal_t1 : t1 -> t1 -> bool = <fun>
val reconstruct_t1 : string = "uniform"
|}]

type t2 = unit# * unit# * int

let make_t2 = (#(), #(), 42)
let make_t2_kind = block_kind make_t2
[%%expect{|
type t2 = unit# * unit# * int
val make_t2 : unit# * unit# * int = <abstr>
val make_t2_kind : string = "uniform"
|}]

type t3 = int * float# * #((unit * string) * unit#)

let equal_t3 ((a, b, #((c, d), #())) : t3)
    ((a', b', #((c', d'), #())) : t3) =
  Int.equal a a' && Float_u.equal b b'
  && Unit.equal c c' && String.equal d d'

let reconstruct_t3 =
  let reconstructed =
    match (42, #4.0, #(((), "hi"), #())) with
    | (a, b, #((c, d), e)) -> (a, b, #((c, d), e))
  in
  assert (equal_t3 reconstructed (42, #4.0, #(((), "hi"), #())));
  block_kind reconstructed
[%%expect{|
type t3 = int * float# * #((unit * string) * unit#)
val equal_t3 : t3 -> t3 -> bool = <fun>
val reconstruct_t3 : string = "mixed (scannable_prefix_len = 2)"
|}]

(* CR zeisbach: should this pass? what tests are needed for this behavior?
   I guess it's cool if it just works, but I'm very low confidence. I should
   look at Will's tests once that gets merged. *)
type t_void : void
type all_void = t_void * t_void
let make_all_void (v : t_void) = (v, v)
[%%expect{|
type t_void : void
type all_void = t_void * t_void
val make_all_void : t_void -> t_void * t_void = <fun>
|}]

type all_unit_u = unit# * unit# * unit#
let all_unit_v = (#(), #(), #())
(* CR zeisbach: should this be mixed to match mixed records? what would the
   benefits of doing that be? Probably for consistency! *)
let all_unit_v_kind = block_kind all_unit_v
[%%expect{|
type all_unit_u = unit# * unit# * unit#
val all_unit_v : unit# * unit# * unit# = <abstr>
val all_unit_v_kind : string = "uniform"
|}]

type ('a : any) t4 = 'a * int

let equal_t4_float ((f, n) : float# t4) ((f', n') : float# t4) =
  Float_u.equal f f' && Int.equal n n'
[%%expect{|
type ('a : any) t4 = 'a * int
val equal_t4_float : float# t4 -> float# t4 -> bool = <fun>
|}]

let check =
  let v4_uniform : int t4 = (6, 7) in
  let v4_mixed : float# t4 = (#6.0, 7) in
  assert (v4_uniform = (6, 7));
  assert (equal_t4_float v4_mixed (#6.0, 7));
  block_kind v4_uniform, block_kind v4_mixed
[%%expect{|
val check : string * string = ("uniform", "mixed (scannable_prefix_len = 1)")
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
  | Some (6, #7.0, _) -> ()
[%%expect{|
Lines 1-2, characters 48-27:
1 | ................................................function
2 |   | Some (6, #7.0, _) -> ()
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "Some (6, #0., _)"

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
  (n, s, b)
[%%expect{|
val letop_mixed : int * string * bool = (4, "hi", true)
|}]

(* value_rec_compiler checks *)
