(* TEST
 flags = "-w -8 -dlambda -dno-unique-ids";
 expect;
*)

(* Lambda tests tracking the value kinds of function parameters whose type
   is constrained by a GADT constructor match. *)

type 'a rep = Block : (int * int) rep | Int : int rep
[%%expect{|
0
type 'a rep = Block : (int * int) rep | Int : int rep
|}]

(* Control: no GADT narrowing involved. The parameter's kind must stay
   precise. *)
let fst2 (p : int * int) = match p with a, _b -> a
[%%expect{|
(let
  (fst2 =
     (function {nlocal = 1}
       p[L][value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int (field_imm 0 p)))
  (apply (field_imm 1 (global Toploop!)) "fst2" fst2))
val fst2 : int * int -> int = <fun>
|}]

(* Control: a non-GADT constructor pattern does not narrow the type; the
   kind of [o] must stay precise either way. *)
let get (o : int option) = match o with Some x -> x | None -> 0
[%%expect{|
(let
  (get =
     (function {nlocal = 1} o[L][value<(consts (0)) (non_consts ([0: ?]))>]
       : int (if o (field_imm 0 o) 0)))
  (apply (field_imm 1 (global Toploop!)) "get" get))
val get : int option -> int = <fun>
|}]

(* Sound: a total multi-case GADT match; the parameter's kind is the join of
   the branches' kinds, which here agree exactly. *)
type _ ab = A : int ab | B : bool ab

let m : type a. a ab * a -> int = function
  | A, x -> x
  | B, b -> if b then 1 else 0
[%%expect{|
0
type _ ab = A : int ab | B : bool ab
(let
  (m =
     (function {nlocal = 0}
       param[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int
       (if (field_imm 0 param) (if (field_imm 1 param) 1 0)
         (field_imm 1 param))))
  (apply (field_imm 1 (global Toploop!)) "m" m))
val m : 'a ab * 'a -> int = <fun>
|}]

(* Sound: a total multi-case GADT match whose branch kinds disagree; the
   join keeps the tuple shape and widens only the disagreeing component. *)
let n : type a. a rep * a -> int = function
  | Block, (a, _b) -> a
  | Int, x -> x
[%%expect{|
(let
  (n =
     (function {nlocal = 0}
       param[value<(consts ()) (non_consts ([0: value<int>, *]))>] : int
       (if (field_imm 0 param) (field_imm 1 param)
         (field_imm 0 (field_imm 1 param)))))
  (apply (field_imm 1 (global Toploop!)) "n" n))
val n : 'a rep * 'a -> int = <fun>
|}]

(* Sound: the disagreeing component is a mixed block whose scannable field is
   [int] (an immediate) in one branch and [string] (a pointer) in the other.
   Both are scannable, so the join keeps the mixed shape -- including its
   [float#] flat suffix -- and widens only that scannable slot. *)
type im = { vi : int; d : float# }
type sm = { vs : string; e : float# }
type _ rep2 = RI : im rep2 | RS : sm rep2

let mixed : type a. a rep2 * a -> int = function
  | RI, x -> x.vi
  | RS, x -> String.length x.vs
[%%expect{|
0
type im = { vi : int; d : float#; }
0
type sm = { vs : string; e : float#; }
0
type _ rep2 = RI : im rep2 | RS : sm rep2
(let
  (mixed =
     (function {nlocal = 0}
       param[value<
              (consts ())
               (non_consts ([0: value<int>,
                             value<
                              (consts ()) (non_consts ([0: *, float64]))>]))>]
       : int
       (if (field_imm 0 param)
         (string.length (mixedfield 0  (*,float64) (field_imm 1 param)))
         (mixedfield 0  (value<int>,float64) (field_imm 1 param)))))
  (apply (field_imm 1 (global Toploop!)) "mixed" mixed))
val mixed : 'a rep2 * 'a -> int = <fun>
|}]

(* Sound: two mixed blocks whose flat suffixes disagree ([float#] versus
   [float32#]) cannot be joined -- their representations differ -- so the
   whole component widens to a generic value even though the scannable
   prefixes agree. *)
type fa = { fx : int; fd : float# }
type fb = { gx : int; ge : float32# }
type _ rep3 = RF : fa rep3 | RG : fb rep3

let mixed_flat : type a. a rep3 * a -> int = function
  | RF, x -> x.fx
  | RG, x -> x.gx
[%%expect{|
0
type fa = { fx : int; fd : float#; }
0
type fb = { gx : int; ge : float32#; }
0
type _ rep3 = RF : fa rep3 | RG : fb rep3
(let
  (mixed_flat =
     (function {nlocal = 0}
       param[value<(consts ()) (non_consts ([0: value<int>, *]))>] : int
       (if (field_imm 0 param)
         (mixedfield 0  (value<int>,float32) (field_imm 1 param))
         (mixedfield 0  (value<int>,float64) (field_imm 1 param)))))
  (apply (field_imm 1 (global Toploop!)) "mixed_flat" mixed_flat))
val mixed_flat : 'a rep3 * 'a -> int = <fun>
|}]
