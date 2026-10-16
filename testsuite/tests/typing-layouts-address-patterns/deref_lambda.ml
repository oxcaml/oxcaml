(* TEST
 flags = "-dlambda -dcanonical-ids";
 expect;
*)

(* Dereference patterns on elements of syntactic tuples need the tuple in
   memory. It is then allocated once and matched like any other value. *)

(* A single dereference pattern *)
let single x y = match x, y with deref_ a, _ -> a
[%%expect{|
(let
  (single/0 =
     (function {nlocal = 0} x/0? y/0? : #(?, int64)
       (let (*match*/0 = (makeblock 0 x/0 y/0))
         (make_unboxed_product #(?, int64) *match*/0 (idx_field 0)))))
  (apply (field_imm 1 (global Toploop!)) "single" single/0))
val single : 'a -> 'b -> 'a addr_imm = <fun>
|}]

(* The tuple is shared by dereference patterns and aliases of the tuple *)
let shared x y =
  match x, y with
  | (deref_ a, 0) as t -> #(a, t)
  | (deref_ a, _) as t -> #(a, t)
[%%expect{|
(let
  (shared/0 =
     (function {nlocal = 0} x/1? y/1[value<int>] : #(#(?, int64), ?)
       (let (t/0 = (makeblock 0 x/1 y/1))
         (if (%int_notequal (field_imm 1 t/0) 0)
           (make_unboxed_product #(#(?, int64), value<
                                                 (consts ())
                                                  (non_consts ([0: ?,
                                                                value<int>]))>)
             (make_unboxed_product #(?, int64) t/0 (idx_field 0)) t/0)
           (make_unboxed_product #(#(?, int64), value<
                                                 (consts ())
                                                  (non_consts ([0: ?,
                                                                value<int>]))>)
             (make_unboxed_product #(?, int64) t/0 (idx_field 0)) t/0)))))
  (apply (field_imm 1 (global Toploop!)) "shared" shared/0))
val shared : 'a -> int -> #('a addr_imm * ('a * int)) = <fun>
|}]

(* Dereference patterns on nested tuples don't need the syntactic tuple *)
let nested x y = match (x, 1), y with ((deref_ a, _), _) -> a
[%%expect{|
(let
  (nested/0 =
     (function {nlocal = 0} x/2? y/2? : #(?, int64)
       (let
         (*match*/1 =[value<(consts ()) (non_consts ([0: ?, value<int>]))>]
            (makeblock 0 (?,value<int>) x/2 1))
         (make_unboxed_product #(?, int64) *match*/1 (idx_field 0)))))
  (apply (field_imm 1 (global Toploop!)) "nested" nested/0))
val nested : 'a -> 'b -> 'a addr_imm = <fun>
|}]

(* Without dereference patterns, aliases of the tuple are allocated where they
   are used *)
let alias x y =
  match x, y with
  | (_, 0) as t -> t
  | (_, 1) as t -> t
  | _ -> (x, y)
[%%expect{|
(let
  (alias/0 =
     (function {nlocal = 0} x/3? y/3[value<int>]
       : (consts ()) (non_consts ([0: ?, value<int>]))
       (if (isout 1 y/3) (makeblock 0 (?,value<int>) x/3 y/3)
         (makeblock 0 x/3 y/3))))
  (apply (field_imm 1 (global Toploop!)) "alias" alias/0))
val alias : 'a -> int -> 'a * int = <fun>
|}]

(* Let-bound syntactic tuples are allocated *)
let let_tuple x y = let (deref_ a, _) = (x, y) in a
[%%expect{|
(let
  (let_tuple/0 =
     (function {nlocal = 0} x/4? y/4? : #(?, int64)
       (let
         (*match*/2 =[value<(consts ()) (non_consts ([0: ?, ?]))>]
            (makeblock 0 x/4 y/4))
         (make_unboxed_product #(?, int64) *match*/2 (idx_field 0)))))
  (apply (field_imm 1 (global Toploop!)) "let_tuple" let_tuple/0))
val let_tuple : 'a -> 'b -> 'a addr_imm = <fun>
|}]

(* Matches with exception cases are compiled differently *)
let exn x y d = match x, y with (deref_ a, _) -> a | exception _ -> d
[%%expect{|
(let
  (exn/0 =
     (function {nlocal = 0} x/5? y/5? d/0[#(?, int64)] : #(?, int64)
       (catch (try (exit 22 x/5 y/5) with exn/1 d/0) with (22 val/0? val/1?)
         (let (*match*/3 = (makeblock 0 val/0 val/1))
           (make_unboxed_product #(?, int64) *match*/3 (idx_field 0))))))
  (apply (field_imm 1 (global Toploop!)) "exn" exn/0))
val exn : 'a -> 'b -> 'a addr_imm -> 'a addr_imm = <fun>
|}]

(* Let-bound constant tuples are not split into their elements *)
let let_constant () = let (deref_ a, _) = (1, 2) in a
[%%expect{|
(let
  (let_constant/0 =
     (function {nlocal = 0} param/0[value<int>] : #(?, int64)
       (let
         (*match*/4 =[value<
                       (consts ()) (non_consts ([0: value<int>, value<int>]))>]
            [0: 1 2])
         (make_unboxed_product #(?, int64) *match*/4 (idx_field 0)))))
  (apply (field_imm 1 (global Toploop!)) "let_constant" let_constant/0))
val let_constant : unit -> int addr_imm = <fun>
|}]
