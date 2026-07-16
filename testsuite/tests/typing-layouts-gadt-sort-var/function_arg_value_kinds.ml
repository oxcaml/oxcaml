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
     (function {nlocal = 0}
       p[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>] : int
       (field_imm 0 p)))
  (apply (field_imm 1 (global Toploop!)) "fst2" fst2))
val fst2 : int * int -> int = <fun>
|}]

(* Control: a non-GADT constructor pattern does not narrow the type; the
   kind of [o] must stay precise either way. *)
let get (o : int option) = match o with Some x -> x | None -> 0
[%%expect{|
(let
  (get =
     (function {nlocal = 0} o[value<(consts (0)) (non_consts ([0: ?]))>]
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
