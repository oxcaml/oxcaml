(* TEST
 flags = "-dlambda -dno-unique-ids";
 expect;
*)

(* Dereference patterns on elements of syntactic tuples need the tuple in
   memory. It is then allocated once and matched like any other value. *)

(* A single dereference pattern *)
let single x y = match x, y with deref_ a, _ -> a
[%%expect{|
(let
  (single =
     (function {nlocal = 0} x? y? : #(?, int64)
       (let (*match* = (makeblock 0 x y))
         (make_unboxed_product #(?, int64) *match*
           (idx_mixed_field  (?,?) 0 )))))
  (apply (field_imm 1 (global Toploop!)) "single" single))
val single : 'a -> 'b -> 'a addr_imm = <fun>
|}]

(* The tuple is shared by dereference patterns and aliases of the tuple *)
let shared x y =
  match x, y with
  | (deref_ a, 0) as t -> #(a, t)
  | (deref_ a, _) as t -> #(a, t)
[%%expect{|
(let
  (shared =
     (function {nlocal = 0} x? y[value<int>] : #(#(?, int64), ?)
       (let (t = (makeblock 0 (?,value<int>) x y))
         (if (%int_notequal (mixedfield 1  (?,?) t) 0)
           (make_unboxed_product #(#(?, int64), value<
                                                 (consts ())
                                                  (non_consts ([0: ?,
                                                                value<int>]))>)
             (make_unboxed_product #(?, int64) t (idx_mixed_field  (?,?) 0 ))
             t)
           (make_unboxed_product #(#(?, int64), value<
                                                 (consts ())
                                                  (non_consts ([0: ?,
                                                                value<int>]))>)
             (make_unboxed_product #(?, int64) t (idx_mixed_field  (?,?) 0 ))
             t)))))
  (apply (field_imm 1 (global Toploop!)) "shared" shared))
val shared : 'a -> int -> #('a addr_imm * ('a * int)) = <fun>
|}]

(* Let-bound syntactic tuples are allocated *)
let let_tuple x y = let (deref_ a, _) = (x, y) in a
[%%expect{|
(let
  (let_tuple =
     (function {nlocal = 0} x? y? : #(?, int64)
       (let
         (*match* =[value<(consts ()) (non_consts ([0: ?, ?]))>]
            (makeblock 0 x y))
         (make_unboxed_product #(?, int64) *match*
           (idx_mixed_field  (?,?) 0 )))))
  (apply (field_imm 1 (global Toploop!)) "let_tuple" let_tuple))
val let_tuple : 'a -> 'b -> 'a addr_imm = <fun>
|}]

(* The tuple is allocated on the stack if the address does not escape *)
let stack_match (x : int) y =
  match x, y with (deref_ a, _) -> (match a with addr_imm_ v -> v)
[%%expect{|
(let
  (stack_match =
     (function {nlocal = 0} x[value<int>] y? : int
       (region
         (let (*match* = (makelocalblock 0 (value<int>,?) x y))
           ((get_ptr_imm ?)
             (make_unboxed_product #(?, int64) *match*
               (idx_mixed_field  (?,?) 0 )))))))
  (apply (field_imm 1 (global Toploop!)) "stack_match" stack_match))
val stack_match : int -> 'a -> int = <fun>
|}]

let stack_let (x : int) y =
  let (deref_ a, _) = (x, y) in match a with addr_imm_ v -> v
[%%expect{|
(let
  (stack_let =
     (function {nlocal = 0} x[value<int>] y? : int
       (region
         (let
           (*match* =[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
              (makelocalblock 0 (value<int>,?) x y))
           ((get_ptr_imm ?)
             (make_unboxed_product #(?, int64) *match*
               (idx_mixed_field  (?,?) 0 )))))))
  (apply (field_imm 1 (global Toploop!)) "stack_let" stack_let))
val stack_let : int -> 'a -> int = <fun>
|}]

(* Dereference patterns on syntactic tuples only allocate the tuple if they
   are on its elements *)
type r = { mutable f : float#; g : string }
let in_element r y = match r, y with ({ f = deref_ a; _ }, _) -> a
[%%expect{|
0
type r = { mutable f : float#; g : string; }
(let
  (in_element =
     (function {nlocal = 0} r y? : #(?, int64)
       (make_unboxed_product #(?, int64) r (idx_mixed_field  (float64,?) 0 ))))
  (apply (field_imm 1 (global Toploop!)) "in_element" in_element))
val in_element : r -> 'a -> float# addr = <fun>
|}]

(* Tupled functions are not flattened *)
let tupled (deref_ a, _) = a
[%%expect{|
(let
  (tupled =
     (function {nlocal = 0}
       param[value<(consts ()) (non_consts ([0: ?, ?]))>] : #(?, int64)
       (make_unboxed_product #(?, int64) param (idx_mixed_field  (?,?) 0 ))))
  (apply (field_imm 1 (global Toploop!)) "tupled" tupled))
val tupled : 'a * 'b -> 'a addr_imm = <fun>
|}]

(* Fields of mixed records and positions below address patterns *)
type p = #{ x : string; y : float# }
type m = { mutable mp : p }
let mixed { mp = #{ y = deref_ a; _ } } = a
let deepen (addr_ #{ y = deref_ a; _ } : p addr) = a
[%%expect{|
0
type p = #{ x : string; y : float#; }
0
type m = { mutable mp : p; }
(let
  (mixed =
     (function {nlocal = 0} param : #(?, int64)
       (make_unboxed_product #(?, int64) param
         (idx_mixed_field  (product  (?,float64)) 0 1))))
  (apply (field_imm 1 (global Toploop!)) "mixed" mixed))
val mixed : m -> float# addr = <fun>
(let
  (deepen =
     (function {nlocal = 0} param[#(?, int64)] : #(?, int64)
       (make_unboxed_product #(?, int64)
         (unboxed_product_field 0 #(?, int64) param)
         (idx_deepen product  (?,float64) 1
           (unboxed_product_field 1 #(?, int64) param)))))
  (apply (field_imm 1 (global Toploop!)) "deepen" deepen))
val deepen : p addr -> float# addr = <fun>
|}]
