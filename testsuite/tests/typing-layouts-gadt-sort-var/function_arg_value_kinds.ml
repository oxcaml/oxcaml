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

(* The second parameter's kind must not be the (int * int) tuple kind from
   the [Block] equation. *)
let f : type a. a rep -> a -> int = fun Block (a, _b) -> a
[%%expect{|
(let
  (f =
     (function {nlocal = 0} param[value<int>]
       param[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int
       (if param
         (raise (makeblock 0 (getpredef Match_failure!!) [0: "" 1 40]))
         (field_imm 0 param))))
  (apply (field_imm 1 (global Toploop!)) "f" f))
val f : 'a rep -> 'a -> int = <fun>
|}]

(* Same for a total match: the kind of [x] must not come from the [Block]
   branch's pattern type. *)
let g : type a. a rep -> a -> int = fun r x ->
  match r, x with Block, (a, _b) -> a | Int, y -> y
[%%expect{|
(let
  (g = (function {nlocal = 0} r[value<int>] x : int (if r x (field_imm 0 x))))
  (apply (field_imm 1 (global Toploop!)) "g" g))
val g : 'a rep -> 'a -> int = <fun>
|}]

(* [@local] functions: the same applies to the catch parameters' kinds. *)
let h b =
  let[@local] f : type a. a rep -> a -> int = fun Block (a, _b) -> a in
  if b then f Block (1, 2) else f Int 0
[%%expect{|
(let
  (h =
     (function {nlocal = 0} b[value<int>] : int
       (catch (if b (exit 9 0 [0: 1 2]) (exit 9 1 0))
        with (9 param[value<int>] param[value<
                                         (consts ())
                                          (non_consts ([0: value<int>,
                                                        value<int>]))>])
         (if param
           (raise (makeblock 0 (getpredef Match_failure!!) [0: "" 2 50]))
           (field_imm 0 param)))))
  (apply (field_imm 1 (global Toploop!)) "h" h))
val h : bool -> int = <fun>
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

(* Sound cases: the refinement below is implied by more than the matched
   pattern. *)

(* Sound: the function's own type already determines the argument type; no
   GADT equation is needed for precision. *)
let i : (int * int) rep -> int * int -> int = fun Block (a, _b) -> a
[%%expect{|
(let
  (i =
     (function {nlocal = 0} param[value<int>]
       param[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int (field_imm 0 param)))
  (apply (field_imm 1 (global Toploop!)) "i" i))
val i : (int * int) rep -> int * int -> int = <fun>
|}]

(* Sound: the equation from matching [r] is established before the inner
   closures are built, so it holds for every call of those closures. *)
let j (type a) (r : a rep) : a -> int =
  match r with Block -> (fun (a, _b) -> a) | Int -> fun x -> x
[%%expect{|
(let
  (j =
     (function {nlocal = 0} r[value<int>]
       (if r (function {nlocal = 0} x[value<int>] : int x)
         (function {nlocal = 0}
           param[value<
                  (consts ()) (non_consts ([0: value<int>, value<int>]))>]
           : int (field_imm 0 param)))))
  (apply (field_imm 1 (global Toploop!)) "j" j))
val j : 'a rep -> 'a -> int = <fun>
|}]

(* Sound: a single-constructor GADT; every caller's instantiation satisfies
   the equation, and the total match lets the kind stay precise. *)
type _ only = Only : (int * int) only

let k : type a. a only -> a -> int = fun Only (a, _b) -> a
[%%expect{|
0
type _ only = Only : (int * int) only
(let
  (k =
     (function {nlocal = 0} param[value<int>]
       param[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int (field_imm 0 param)))
  (apply (field_imm 1 (global Toploop!)) "k" k))
val k : 'a only -> 'a -> int = <fun>
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

(* A parameter following a partial GADT match gets its kind from the
   function's own type, which the caller must satisfy, rather than from the
   pattern's environment, which contains the equations. *)
let p : type a. a rep -> int * int -> int = fun Block (x, _y) -> x
[%%expect{|
(let
  (p =
     (function {nlocal = 0} param[value<int>]
       param[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int
       (if param
         (raise (makeblock 0 (getpredef Match_failure!!) [0: "" 1 48]))
         (field_imm 0 param))))
  (apply (field_imm 1 (global Toploop!)) "p" p))
val p : 'a rep -> int * int -> int = <fun>
|}]
