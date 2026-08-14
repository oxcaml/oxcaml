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
type bad = int{ while true do () done; true };;
[%%expect{|
Line 1, characters 16-43:
1 | type bad = int{ while true do () done; true };;
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Sequencing is not allowed in a refinement predicate:
       predicates must be total.
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
