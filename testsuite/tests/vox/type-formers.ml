(* TEST
 expect;
*)

(* Vox type formers: refinement types [t{p}] and the dependent-arrow
   binder.  This piece is inert: refinements parse, translate, print and
   travel through the type graph, but there are no introduction or
   elimination rules yet, so these tests declare, annotate and print
   rather than use. *)

(* --- The four spellings --------------------------------------------- *)

(* positional, hole *)
type t1 = int{ _ > 0 } -> unit;;
[%%expect{|
type t1 = int{ _ > 0 } -> unit
|}]

(* positional, named: using the name is what makes it a binder *)
type t2 = n:int{ n > 0 } -> unit;;
[%%expect{|
type t2 = n:int{ n > 0 } -> unit
|}]

(* labelled, hole: [x] does not occur in a refinement, so it stays a
   label *)
type t3 = x:int{ _ > 0 } -> unit;;
[%%expect{|
type t3 = x:int{ _ > 0 } -> unit
|}]

(* labelled, named: [~x:] is always a label, and the name refers to the
   argument's own value in its own refinement *)
type t4 = ~x:int{ x > 0 } -> unit;;
[%%expect{|
type t4 = ~x:int{ x > 0 } -> unit
|}]

(* --- Calling conventions -------------------------------------------- *)

(* The binder is positional: the calling convention is unchanged, so the
   type unifies with itself spelled through a hole-free alias. *)
type pos_conv = int{ _ > 0 } -> unit
let l : pos_conv list = ([] : (int{ _ > 0 } -> unit) list);;
[%%expect{|
type pos_conv = int{ _ > 0 } -> unit
val l : pos_conv list = []
|}]

(* The hole and a named binder are different predicates: equality is
   syntactic alpha-equivalence, with no normalization between [_] and a
   name for the same value. *)
let l : (n:int{ n > 0 } -> unit) list = ([] : (int{ _ > 0 } -> unit) list);;
[%%expect{|
Line 1, characters 40-74:
1 | let l : (n:int{ n > 0 } -> unit) list = ([] : (int{ _ > 0 } -> unit) list);;
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(int{ _ > 0 } -> unit) list"
       but an expression was expected of type "(n:int{ n > 0 } -> unit) list"
       Type "int{ _ > 0 }" is not compatible with type "int{ n > 0 }"
|}]

(* A bare name that binds is not a label: labelled functions do not
   unify with it. *)
let l : (n:int{ n > 0 } -> unit) list = ([] : (n:int -> unit) list);;
[%%expect{|
Line 1, characters 40-67:
1 | let l : (n:int{ n > 0 } -> unit) list = ([] : (n:int -> unit) list);;
                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(n:int -> unit) list"
       but an expression was expected of type "(n:int{ n > 0 } -> unit) list"
       The first argument is labeled "n",
       but an unlabeled argument was expected
|}]

(* [~x:] is a label: it unifies with the same label *)
let l : (~x:int{ x > 0 } -> unit) list = ([] : (~x:int{ x > 0 } -> unit) list);;
[%%expect{|
val l : (~x:int{ x > 0 } -> unit) list = []
|}]

(* --- Binding --------------------------------------------------------- *)

(* A name occurring only in a later refinement binds *)
type later = x:int -> int{ _ >= x };;
[%%expect{|
type later = x:int -> int{ _ >= x }
|}]

(* One binder for both the argument's own refinement and the codomain *)
type once = x:int{ x > 0 } -> int{ _ >= x };;
[%%expect{|
type once = x:int{ x > 0 } -> int{ _ >= x }
|}]

(* Alpha-equivalence: differently-named binders compare equal *)
let l : (x:int{ x > 0 } -> int{ _ >= x }) list =
  ([] : (y:int{ y > 0 } -> int{ _ >= y }) list);;
[%%expect{|
val l : (x:int{ x > 0 } -> int{ _ >= x }) list = []
|}]

(* ... but different predicates do not *)
let l : (x:int{ x > 0 } -> int{ _ >= x }) list =
  ([] : (y:int{ y > 1 } -> int{ _ >= y }) list);;
[%%expect{|
Line 2, characters 2-47:
2 |   ([] : (y:int{ y > 1 } -> int{ _ >= y }) list);;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "(y:int{ y > 1 } -> int{ _ >= y }) list"
       but an expression was expected of type
         "(x:int{ x > 0 } -> int{ _ >= x }) list"
       Type "int{ y > 1 }" is not compatible with type "int{ x > 0 }"
|}]

(* One-sided refinement is a clash *)
let l : int{ _ > 0 } list = ([] : int list);;
[%%expect{|
Line 1, characters 28-43:
1 | let l : int{ _ > 0 } list = ([] : int list);;
                                ^^^^^^^^^^^^^^^
Error: This expression has type "int list"
       but an expression was expected of type "int{ _ > 0 } list"
       Type "int" is not compatible with type "int{ _ > 0 }"
|}]

(* --- The occurrence test -------------------------------------------- *)

(* A name shadowed by a predicate's own [let] does not bind *)
type shadow_let = x:int{ let x = 1 in x > 0 } -> unit;;
[%%expect{|
type shadow_let = x:int{ let x = 1 in x > 0 } -> unit
|}]

(* A name shadowed by a [match] case binder does not bind *)
type shadow_match =
  x:int option{ match _ with Some x -> x > 0 | None -> true } -> unit;;
[%%expect{|
type shadow_match =
    x:int option{ match _ with | Some x -> x > 0 | None -> true } -> unit
|}]

(* A name occurring only inside a nested arrow's refinement does not
   escape that arrow *)
type nested = x:int -> (x:int -> int{ _ >= x }) -> unit;;
[%%expect{|
type nested = x:int -> (x:int -> int{ _ >= x }) -> unit
|}]

(* Optional parameters never bind *)
type opt = ?x:int{ _ > 0 } -> unit -> unit;;
[%%expect{|
type opt = ?x:int{ _ > 0 } -> unit -> unit
|}]

type bad = ?x:int{ x > 0 } -> unit -> unit;;
[%%expect{|
Line 1, characters 19-20:
1 | type bad = ?x:int{ x > 0 } -> unit -> unit;;
                       ^
Error: Unbound value "x"
|}]

(* [~x:] scopes over the whole argument, and only the argument *)
type tilde_tuple = ~x:(int{ x > 0 } * int) -> unit;;
[%%expect{|
type tilde_tuple = ~x:int{ x > 0 } * int -> unit
|}]

type bad = ~x:int -> int{ _ >= x };;
[%%expect{|
Line 1, characters 31-32:
1 | type bad = ~x:int -> int{ _ >= x };;
                                   ^
Error: Unbound value "x"
|}]

(* A positional binder scopes over refinements anywhere in its domain *)
type nested_domain = x:(int{ x > 0 } * int) -> unit;;
[%%expect{|
type nested_domain = x:int{ x > 0 } * int -> unit
|}]

(* A [fun] parameter in the predicate shadows the name, so [x] stays a
   label *)
type still_label = x:int list{ List.for_all (fun x -> x > 0) _ } -> unit;;
[%%expect{|
type still_label = x:int list{ List.for_all (fun x -> x > 0) _ } -> unit
|}]

(* A predicate-local binder is in scope in nested refinements through a
   constraint type *)
type local_nested = int{ (fun x -> (x : int{ x = 0 })) 0 = 0 };;
[%%expect{|
type local_nested = int{ ((fun x -> (x : int{ x = 0 })) 0) = 0 }
|}]

(* Mixed arrow: a positional binder and a bare-spelled label in one
   chain warns *)
type mixed = x:int -> y:int -> int{ _ >= x };;
[%%expect{|
Line 1, characters 13-44:
1 | type mixed = x:int -> y:int -> int{ _ >= x };;
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 230 [vox-mixed-arrow-conventions]: This arrow type mixes positional value binders and bare-spelled
  labels; reading it requires scanning the whole type.
  Spell the labels with a tilde ("~x:") to make the convention explicit.

type mixed = x:int -> y:int -> int{ _ >= x }
|}]

(* --- Predicates ------------------------------------------------------ *)

(* Predicates are ordinary expressions: applications, field access,
   let, fun, match, if, tuples, constants, constructors *)
type pred_apply = s:string -> int{ _ < String.length s } -> char;;
[%%expect{|
type pred_apply = s:string -> int{ _ < (String.length s) } -> char
|}]

type pred_fun = int list{ List.for_all (fun x -> x > 0) _ };;
[%%expect{|
type pred_fun = int list{ List.for_all (fun x -> x > 0) _ }
|}]

type pred_if = int{ if _ > 0 then _ < 10 else _ > -10 };;
[%%expect{|
type pred_if = int{ if _ > 0 then _ < 10 else _ > (-10) }
|}]

(* A proposition on unit; no name needed *)
type prop = unit{ 1 + 1 = 2 };;
[%%expect{|
type prop = unit{ (1 + 1) = 2 }
|}]

(* Rejections: unbound name *)
type bad = int{ _ > n };;
[%%expect{|
Line 1, characters 20-21:
1 | type bad = int{ _ > n };;
                        ^
Error: Unbound value "n"
|}]

(* Rejections: non-total forms *)
type bad = int{ while true do () done };;
[%%expect{|
Line 1, characters 16-37:
1 | type bad = int{ while true do () done };;
                    ^^^^^^^^^^^^^^^^^^^^^
Error: While loops are not allowed in a refinement predicate:
       predicates must be total.
|}]

type bad = int{ (); true };;
[%%expect{|
Line 1, characters 16-24:
1 | type bad = int{ (); true };;
                    ^^^^^^^^
Error: Sequencing is not allowed in a refinement predicate:
       predicates must be total.
|}]

(* Total but not yet part of the predicate sublanguage: a real located
   error, not a crash *)
type bad = int{ { contents = 1 } = _ };;
[%%expect{|
Line 1, characters 16-32:
1 | type bad = int{ { contents = 1 } = _ };;
                    ^^^^^^^^^^^^^^^^
Error: A record expression is not supported in refinement predicates.
|}]

type bad = unit{ ref true };;
[%%expect{|
Line 1, characters 17-20:
1 | type bad = unit{ ref true };;
                     ^^^
Error: References are not allowed in a refinement predicate:
       predicates must be total.
|}]

(* --- Positions ------------------------------------------------------- *)

(* Record fields, constructor arguments, type declaration bodies *)
type wf = { size : int{ _ >= 0 }; mutable used : int };;
[%%expect{|
type wf = { size : int{ _ >= 0 }; mutable used : int; }
|}]

type pos = Pos of int{ _ > 0 } | Neg of int{ _ < 0 };;
[%%expect{|
type pos = Pos of int{ _ > 0 } | Neg of int{ _ < 0 }
|}]

type nat = int{ _ >= 0 };;
[%%expect{|
type nat = int{ _ >= 0 }
|}]

(* A recursive declaration whose predicate mentions the type being
   defined, through an interior type *)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
let well_formed (_ : 'a tree) = true;;
[%%expect{|
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree
val well_formed : 'a tree -> bool = <fun>
|}]

type wft = t tree{ well_formed (_ : t tree) }
and t = int;;
[%%expect{|
type wft = t tree{ well_formed (_ : t tree) }
and t = int
|}]

(* String constants compare by contents, not by where they were
   written *)
type s = string{ _ = "a" }
let l : s list = ([] : string{ _ = "a" } list);;
[%%expect{|
type s = string{ _ = "a" }
val l : s list = []
|}]

(* Type variables inside a predicate are live in the type graph:
   instantiating the declaration substitutes them *)
type 'a t = int{ (_ : 'a list) = [] }
let l : int t list = ([] : int{ (_ : int list) = [] } list);;
[%%expect{|
type 'a t = int{ (_ : 'a list) = [] }
val l : int t list = []
|}]

(* A self-referential predicate, to pin what the occur check does *)
type u = int{ (_ : u) = _ };;
[%%expect{|
Line 1, characters 0-27:
1 | type u = int{ (_ : u) = _ };;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type abbreviation "u" is cyclic:
         "u" = "int{ (_ : u) = _ }",
         "int{ (_ : u) = _ }" contains "u"
|}]

(* Predicates print resolved names: a functor application substitutes the
   value path *)
module F (X : sig val p : int end) = struct
  type t = int{ _ > X.p }
end
module A = struct let p = 0 end
module R = F (A);;
[%%expect{|
module F :
  functor (X : sig val p : int end) -> sig type t = int{ _ > X.p } end
module A : sig val p : int end
module R : sig type t = int{ _ > A.p } end
|}]

(* A label whose name is a value mentioned in a refinement is escaped, so
   the printed form re-parses to the same type *)
let x = 0
type esc = ~x:int -> int{ x > 0 };;
[%%expect{|
val x : int = 0
type esc = ~x:int -> int{ x > 0 }
|}]

(* --- Inertness ------------------------------------------------------- *)

(* No introduction rule: a refined type cannot be consumed or produced *)
let f (x : int{ _ > 0 }) = x + 1;;
[%%expect{|
Line 1, characters 27-28:
1 | let f (x : int{ _ > 0 }) = x + 1;;
                               ^
Error: The value "x" has type "int{ _ > 0 }"
       but an expression was expected of type "int"
|}]

(* Partial application preserves the binder of an omitted argument *)
let mk (g : ~x:int{ x > 0 } -> y:int -> unit) : ~x:int{ x > 0 } -> unit =
  g ~y:0;;
[%%expect{|
val mk : (~x:int{ x > 0 } -> y:int -> unit) -> ~x:int{ x > 0 } -> unit =
  <fun>
|}]

(* Applying a dependent arrow lets the domain type escape its binder; the
   escaped occurrence prints under the binder's name.  Elimination rules —
   including whether this should be rejected or substituted — belong to a
   later piece; this pins the current, inert behaviour. *)
let apply (f : x:int{ x > 0 } -> unit) v = f v;;
[%%expect{|
val apply : (x:int{ x > 0 } -> unit) -> int{ x > 0 } -> unit = <fun>
|}]
