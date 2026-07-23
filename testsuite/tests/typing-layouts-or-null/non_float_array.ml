(* TEST
 flags = "-dlambda -dno-unique-ids -extension-universe upstream_compatible";
 expect;
*)

(* For tests of [genarray] (polymorphic ['a array]), see
   non_float_array-no-flat-float-array.ml: under the float array optimization a
   polymorphic value array specializes to [genarray] (with dynamic
   float-tag checks), whereas with the optimization disabled it
   specializes to [addrarray] like a [non_float] array. *)

(* [non_float] arrays are [addrarray]s. Operations on [addrarray]s
   skip checks related to floats.

   Here we can see that our operations are postfixed with [addr]. *)

let mk (type t : value mod non_float) (x : t) = [| x |]
[%%expect{|
(let (mk = (function {nlocal = 0} x : addrarray (makearray[addr] x)))
  (apply (field_imm 1 (global Toploop!)) "mk" mk))
val mk : ('t : value non_float). 't -> 't array = <fun>
|}]

let get (type t : value mod non_float) (xs : t array) i = xs.(i)
[%%expect{|
(let
  (get =
     (function {nlocal = 1} xs[L][value<addrarray>] i[L][value<int>]
       (array.get[addr indexed by int] xs i)))
  (apply (field_imm 1 (global Toploop!)) "get" get))
val get : ('t : value non_float). 't array -> int -> 't = <fun>
|}]

let set (type t : value mod non_float) (xs : t array) x i = xs.(i) <- x

[%%expect{|
(let
  (set =
     (function {nlocal = 1} xs[L][value<addrarray>] x i[L][value<int>] : int
       (array.set[addr(local) indexed by int] xs i x)))
  (apply (field_imm 1 (global Toploop!)) "set" set))
val set : ('t : value non_float). 't array -> 't -> int -> unit = <fun>
|}]

(* A concrete example. *)

module X : sig
  type t : immutable_data

  val x1 : t
  val x2 : t
end = struct
  type t = { a: string; b: int }

  let x1 = { a = "first"; b = 1 }
  let x2 = { a = "second"; b = 2 }
end

[%%expect{|
(apply (field_imm 1 (global Toploop!)) "X/367"
  (let
    (x1 =[value<(consts ()) (non_consts ([0: *, value<int>]))>]
       [0: "first" 1]
     x2 =[value<(consts ()) (non_consts ([0: *, value<int>]))>]
       [0: "second" 2])
    (makeblock 0 x1 x2)))
module X : sig type t : immutable_data val x1 : t val x2 : t end
|}]

(* Create an [addrarray] and perform [addr] operations on it. *)

let () =
  let xs = Array.make 4 X.x1 in
  xs.(1) <- X.x2;
  xs.(2) <- X.x2;
  assert (xs.(0) = xs.(3));
  assert (xs.(1) = xs.(2));
  assert (not (xs.(0) = xs.(1)))
;;

[%%expect{|
(let
  (X =? (apply (field_imm 0 (global Toploop!)) "X/367")
   *match* =[value<int>]
     (let (xs =[value<addrarray>] (caml_array_make 4 (field_imm 0 X)))
       (seq (array.set[addr indexed by int] xs 1 (field_imm 1 X))
         (array.set[addr indexed by int] xs 2 (field_imm 1 X))
         (if
           (caml_equal (array.get[addr indexed by int] xs 0)
             (array.get[addr indexed by int] xs 3))
           0 (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 5 2])))
         (if
           (caml_equal (array.get[addr indexed by int] xs 1)
             (array.get[addr indexed by int] xs 2))
           0 (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 6 2])))
         (if
           (not
             (caml_equal (array.get[addr indexed by int] xs 0)
               (array.get[addr indexed by int] xs 1)))
           0 (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 7 2]))))))
  0)
|}]
