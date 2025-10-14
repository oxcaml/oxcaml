(* TEST
 flags = "-dlambda -dno-unique-ids";
 expect;
*)

let mk (type t : value_or_null mod non_float external_) (x : t) = [| x |]
[%%expect{|
(let
  (mk =
     (function {nlocal = 0} x[value_or_null<int>] : gcignorableaddrarray
       (makearray[gcignorableaddr] x)))
  (apply (field_imm 1 (global Toploop!)) "mk" mk))
val mk : ('t : value_or_null mod external_ non_float). 't -> 't array = <fun>
|}]

external[@layout_poly] make : ('a : any mod separable).
  int -> 'a -> 'a array = "%makearray_dynamic"

external[@layout_poly] get : ('a : any mod separable).
  'a array -> int -> 'a = "%array_safe_get"

external[@layout_poly] set : ('a : any mod separable).
  'a array -> int -> 'a -> unit = "%array_safe_set"
[%%expect{|
0
external make : ('a : any mod separable). int -> 'a -> 'a array
  = "%makearray_dynamic" [@@layout_poly]
0
external get : ('a : any mod separable). 'a array -> int -> 'a
  = "%array_safe_get" [@@layout_poly]
0
external set : ('a : any mod separable). 'a array -> int -> 'a -> unit
  = "%array_safe_set" [@@layout_poly]
|}]

let get' (type t : value_or_null mod non_float external_) (xs : t array) i = get xs i
[%%expect{|
(let
  (get' =
     (function {nlocal = 0} xs[value<gcignorableaddrarray>] i[value<int>]
       : int (array.get[gcignorableaddr indexed by int] xs i)))
  (apply (field_imm 1 (global Toploop!)) "get'" get'))
val get' :
  ('t : value_or_null mod external_ non_float). 't array -> int -> 't = <fun>
|}]

let set' (type t : value_or_null mod non_float external_) (xs : t array) x i = set xs i x

[%%expect{|
(let
  (set' =
     (function {nlocal = 0} xs[value<gcignorableaddrarray>]
       x[value_or_null<int>] i[value<int>] : int
       (array.set[gcignorableaddr indexed by int] xs i x)))
  (apply (field_imm 1 (global Toploop!)) "set'" set'))
val set' :
  ('t : value_or_null mod external_ non_float). 't array -> 't -> int -> unit =
  <fun>
|}]

module X : sig
  type t : immediate_or_null

  val x1 : t
  val x2 : t
end = struct
  type t = int or_null

  let x1 = Null
  let x2 = This 5
end

[%%expect{|
(apply (field_imm 1 (global Toploop!)) "X/309"
  (let (x1 =? <null> x2 =? 5) (makeblock 0 x1 x2)))
module X : sig type t : immediate_or_null val x1 : t val x2 : t end
|}]

let () =
  let xs = make 4 X.x1 in
  set xs 1 X.x2;
  set xs 2 X.x2;
  assert (get xs 0 = get xs 3);
  assert (get xs 1 = get xs 2);
  assert (not (get xs 0 = get xs 1))
;;

[%%expect{|
(let
  (X =? (apply (field_imm 0 (global Toploop!)) "X/309")
   *match* =[value<int>]
     (let
       (xs =[value<gcignorableaddrarray>]
          (makearray_any[gcignorableaddr] 4 (field_imm 0 X)))
       (seq (array.set[gcignorableaddr indexed by int] xs 1 (field_imm 1 X))
         (array.set[gcignorableaddr indexed by int] xs 2 (field_imm 1 X))
         (if
           (caml_equal (array.get[gcignorableaddr indexed by int] xs 0)
             (array.get[gcignorableaddr indexed by int] xs 3))
           0 (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 5 2])))
         (if
           (caml_equal (array.get[gcignorableaddr indexed by int] xs 1)
             (array.get[gcignorableaddr indexed by int] xs 2))
           0 (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 6 2])))
         (if
           (not
             (caml_equal (array.get[gcignorableaddr indexed by int] xs 0)
               (array.get[gcignorableaddr indexed by int] xs 1)))
           0 (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 7 2]))))))
  0)
|}]
