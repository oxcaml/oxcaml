(* TEST
   flags = "-dlambda -dno-unique-ids";
   expect;
*)

module Yielding : sig
  type t
  val with_ : (t @ yielding -> 'r) -> 'r
end = struct
  type t = unit
  let[@inline never] with_ f = f (() : _ @ yielding)
end
let[@inline never] yield (_ : Yielding.t @ yielding) = ()
let[@inline never] add x y = x + y
[%%expect{|
(apply[unyielding] (field_imm 1 (global Toploop!)) "Yielding/295"
  (let (with_ = (function {nlocal = 0} f never_inline (apply f 0)))
    (makeblock 0 with_)))
module Yielding : sig type t val with_ : (t @ yielding -> 'r) -> 'r end
(let (yield = (function {nlocal = 0} param never_inline : int 0))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "yield" yield))
val yield : Yielding.t @ yielding -> unit = <fun>
(let
  (add =
     (function {nlocal = 0} x[value<int>] y[value<int>] never_inline : int
       (%int_add x y)))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "add" add))
val add : int -> int -> int = <fun>
|}]


(* basics *)

let () = Yielding.with_ (fun y ->
  let _ = add 2 2 in
  yield y;
  let _ = add 4 4 in
  ())
(* Note the [unyielding] on every apply except [yield] *)
[%%expect{|
(let
  (add =? (apply[unyielding] (field_imm 0 (global Toploop!)) "add")
   yield =? (apply[unyielding] (field_imm 0 (global Toploop!)) "yield")
   Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295")
   *match* =[value<int>]
     (apply[unyielding] (field_imm 0 Yielding)
       (function {nlocal = 0} y : int
         (seq (apply[unyielding] add 2 2) (apply yield y)
           (apply[unyielding] add 4 4) 0))))
  0)
|}]

(* tailcalls *)

let () = Yielding.with_ (fun y ->
  let _ = add 2 2 in
  yield y[@tail])
(* Note the [unyielding] on every apply except [yield] *)
[%%expect{|
(let
  (add =? (apply[unyielding] (field_imm 0 (global Toploop!)) "add")
   yield =? (apply[unyielding] (field_imm 0 (global Toploop!)) "yield")
   Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295")
   *match* =[value<int>]
     (apply[unyielding] (field_imm 0 Yielding)
       (function {nlocal = 0} y : int
         (seq (apply[unyielding] add 2 2) (apply yield y)))))
  0)
|}]


let () = Yielding.with_ (fun y ->
  let _ = add 2 2 in
  yield y[@nontail])
(* Note the [unyielding] on every apply except [yield] *)
[%%expect{|
(let
  (add =? (apply[unyielding] (field_imm 0 (global Toploop!)) "add")
   yield =? (apply[unyielding] (field_imm 0 (global Toploop!)) "yield")
   Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295")
   *match* =[value<int>]
     (apply[unyielding] (field_imm 0 Yielding)
       (function {nlocal = 0} y : int
         (seq (apply[unyielding] add 2 2) (applynontail yield y)))))
  0)
|}]

let (_ : int) = Yielding.with_ (fun y ->
  let _ = add 2 2 in
  yield y;
  add 4 4 [@nontail])
(* Note the [unyielding] on every apply except [yield] *)
[%%expect{|
(let
  (add =? (apply[unyielding] (field_imm 0 (global Toploop!)) "add")
   yield =? (apply[unyielding] (field_imm 0 (global Toploop!)) "yield")
   Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295"))
  (apply[unyielding] (field_imm 0 Yielding)
    (function {nlocal = 0} y : int
      (seq (apply[unyielding] add 2 2) (apply yield y)
        (applynontail[unyielding] add 4 4)))))
- : int = 8
|}]

(* Higher-order functions *)

module List : sig
  val map : ('a -> 'b) @ local -> 'a list -> 'b list
  val init : (int -> 'a) @ local -> int -> 'a list
end = struct
  let rec map f = function
    | [] -> []
    | x :: xs -> f x :: map f xs

  let init f i =
    let rec loop = function
      | 0 -> []
      | i -> f i :: loop (i - 1)
    in
    loop i |> List.rev
  ;;
end
[%%expect{|
(apply[unyielding] (field_imm 1 (global Toploop!)) "List/397"
  (letrec
    (map
       (function {nlocal = 2} f[L]
         param[value<
                (consts (0))
                 (non_consts ([0: ?,
                               value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
         : (consts (0))
            (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
         (if param
           (makeblock 0 (?,value<
                            (consts (0))
                             (non_consts ([0: ?,
                                           value<
                                            (consts (0))
                                             (non_consts ([0: ?, *]))>]))>)
             (apply f (field_imm 0 param)) (apply map f (field_imm 1 param)))
           0)))
    (let
      (init =
         (function {nlocal = 2} f[L] i[value<int>]
           : (consts (0))
              (non_consts ([0: ?,
                            value<(consts (0)) (non_consts ([0: ?, *]))>]))
           (region
             (letrec
               (loop
                  (function[L] {nlocal = 1} i[value<int>]
                    : (consts (0))
                       (non_consts ([0: ?,
                                     value<
                                      (consts (0)) (non_consts ([0: ?, *]))>]))
                    (if (%int_notequal i 0)
                      (makeblock 0 (?,value<
                                       (consts (0))
                                        (non_consts ([0: ?,
                                                      value<
                                                       (consts (0))
                                                        (non_consts (
                                                        [0: ?, *]))>]))>)
                        (apply f i) (apply loop (%int_sub i 1)))
                      0)))
               (applytail[unyielding] (field_imm 10 (global Stdlib__List!))
                 (apply loop i))))))
      (makeblock 0 map init))))
module List :
  sig
    val map : ('a -> 'b) @ local -> 'a list -> 'b list
    val init : (int -> 'a) @ local -> int -> 'a list
  end
|}]

let f (l : int list) =
  Yielding.with_ (fun y ->
    (* CR aspsmith: It would be nice if the first application of List.map also
       got [unyielding] *)
    let (_ : int list) = List.map (fun x -> x + 7) l in
    List.map (fun x -> yield y; x + 4) l) [@nontail]
[%%expect{|
(let
  (yield =? (apply[unyielding] (field_imm 0 (global Toploop!)) "yield")
   List =? (apply[unyielding] (field_imm 0 (global Toploop!)) "List/397")
   Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295")
   f =
     (function {nlocal = 0}
       l[value<
          (consts (0))
           (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
       : (consts (0))
          (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
       (applynontail[unyielding] (field_imm 0 Yielding)
         (function {nlocal = 0} y
           : (consts (0))
              (non_consts ([0: ?,
                            value<(consts (0)) (non_consts ([0: ?, *]))>]))
           (region
             (seq
               (apply (field_imm 0 List)
                 (function[L] {nlocal = 1} x[value<int>] : int
                   (%int_add x 7))
                 l)
               (applytail (field_imm 0 List)
                 (function {nlocal = 0} x[value<int>] : int
                   (seq (apply yield y) (%int_add x 4)))
                 l)))))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "f" f))
val f : int list -> int list = <fun>
|}]

(* Local yielding functions *)

let () =
  Yielding.with_ (fun y ->
    let do_yield() = yield y [@nontail] in
    let[@inline never] don't_yield () = Sys.opaque_identity () in
    do_yield();
    don't_yield();
    do_yield();
    ignore (List.init (fun _ -> do_yield ()) 10);
    (* CR aspsmith: It would be nice if the application of List.init here also got
       [unyielding] *)
    ignore (List.init (fun _ -> don't_yield ()) 10))
[%%expect{|
(let
  (yield =? (apply[unyielding] (field_imm 0 (global Toploop!)) "yield")
   List =? (apply[unyielding] (field_imm 0 (global Toploop!)) "List/397")
   Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295")
   *match* =[value<int>]
     (apply[unyielding] (field_imm 0 Yielding)
       (function {nlocal = 0} y : int
         (region
           (let
             (do_yield =
                (function[L] {nlocal = 1} param[value<int>] : int
                  (applynontail yield y))
              don't_yield =
                (function[L] {nlocal = 1} param[value<int>] never_inline
                  : int (opaque 0)))
             (seq (apply do_yield 0) (apply[unyielding] don't_yield 0)
               (apply do_yield 0)
               (ignore
                 (apply (field_imm 1 List)
                   (function[L] {nlocal = 1} param[value<int>] : int
                     (apply do_yield 0))
                   10))
               (ignore
                 (apply (field_imm 1 List)
                   (function[L] {nlocal = 1} param[value<int>] : int
                     (apply[unyielding] don't_yield 0))
                   10))))))))
  0)
|}]

(* Demonstrate that lazy values must be yielding (justifying that the implicit
   lazy force generated by lazy pattern matching may always be unyielding) *)
let () =
  Yielding.with_ (fun y ->
    let do_yield () = yield y [@nontail] in
    let lazy_val = lazy (do_yield (); 2 + 2) in
    match lazy_val with
    | lazy n -> assert (n = 4))
[%%expect{|
Line 4, characters 25-33:
4 |     let lazy_val = lazy (do_yield (); 2 + 2) in
                             ^^^^^^^^
Error: The value "do_yield" is "yielding"
         because it closes over the value "y" at line 3, characters 28-29
         which is "yielding".
       However, the value "do_yield" highlighted is expected to be "unyielding"
         because it is used inside the lazy expression at line 4, characters 19-44
         which is expected to be "unyielding"
         because lazy expressions always need to be allocated on the heap.
|}]

(* Lazy pattern matching is in fact unyielding *)
let () =
  let lazy_val = lazy (2 + 2) in
  match lazy_val with
  | lazy n -> assert (n = 4)
[%%expect{|
(let
  (*match* =[value<int>]
     (let
       (lazy_val =
          (makelazyblock
            (function {nlocal = 0} param[value<int>] (%int_add 2 2)))
        n =?
          (let (tag =a[value<int>] (caml_obj_tag lazy_val))
            (if (%int_equal tag 250) (field_mut 0 lazy_val)
              (if (|| (%int_equal tag 246) (%int_equal tag 244))
                (apply[unyielding] (field_imm 1 (global CamlinternalLazy!))
                  (opaque lazy_val) never_inline)
                lazy_val))))
       (if (%eq n 4) 0
         (raise (makeblock 0 (getpredef Assert_failure!!) [0: "" 4 14])))))
  0)
|}]


(* Recursive modules: the [CamlinternalMod] calls that allocate the module
   placeholder ([init_mod], field 0) and backpatch it ([update_mod], field 1)
   never run user code, so they are unyielding *)
module rec A : sig val f : int -> int end = struct
  let f n = if n <= 0 then 0 else B.g (n - 1)
end
and B : sig val g : int -> int end = struct
  let g n = A.f n
end
[%%expect{|
(let
  (A =
     (apply[unyielding] (field_imm 0 (global CamlinternalMod!)) [0: "" 1 44]
       [0: [0: 0]])
   B =
     (apply[unyielding] (field_imm 0 (global CamlinternalMod!)) [0: "" 4 37]
       [0: [0: 0]]))
  (seq
    (apply[unyielding] (field_imm 1 (global CamlinternalMod!)) [0: [0: 0]] A
      (let
        (f =
           (function {nlocal = 0} n[value<int>] : int
             (if (%int_lessequal n 0) 0
               (apply[unyielding] (field_imm 0 B) (%int_sub n 1)))))
        (makeblock 0 f)))
    (apply[unyielding] (field_imm 1 (global CamlinternalMod!)) [0: [0: 0]] B
      (let
        (g =
           (function {nlocal = 0} n[value<int>] : int
             (apply[unyielding] (field_imm 0 A) n)))
        (makeblock 0 g)))
    (apply[unyielding] (field_imm 1 (global Toploop!)) "A" A)
    (apply[unyielding] (field_imm 1 (global Toploop!)) "B" B)))
module rec A : sig val f : int -> int end
and B : sig val g : int -> int end
|}]


(* A [let rec] bound to a lazy value that isn't a literal [lazy ...] is
   backpatched through [CamlinternalLazy.indirect] (field 3), which only
   allocates a forwarding block and so is unyielding *)
let f x =
  let rec l = (let v = lazy x in v) in
  l
[%%expect{|
(let
  (f =
     (function {nlocal = 0} x
       (let (l =? (caml_alloc_dummy_lazy 0))
         (seq
           (caml_update_dummy_lazy l
             (apply[unyielding] (field_imm 3 (global CamlinternalLazy!))
               (makeforwardblock x)))
           l))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "f" f))
val f : 'a -> 'a lazy_t = <fun>
|}]


(* Functional object update [{< ... >}] copies the object block via
   [CamlinternalOO.copy], which never runs user code and so is unyielding *)
let o = object
  val x = 1
  method with_x n = {< x = n >}
end
[%%expect{|
(let
  (shared =a (opaque [0: #"with_x"])
   o =
     (let
       (class =?
          (opaque
            (apply[unyielding] (field_imm 15 (global CamlinternalOO!))
              shared))
        obj_init =
          (let
            (ids =?
               (opaque
                 (apply[unyielding] (field_imm 3 (global CamlinternalOO!))
                   class shared (opaque [0: #"x"])))
             with_x =o? (field_mut 0 ids)
             x =o? (field_mut 1 ids))
            (seq
              (opaque
                (apply[unyielding] (field_imm 9 (global CamlinternalOO!))
                  class with_x
                  (function {nlocal = 0} self-1 n[value<int>]
                    (let
                      (copy =
                         (apply[unyielding]
                           (field_imm 21 (global CamlinternalOO!)) self-1))
                      (seq (setfield_imm_computed copy x n) copy)))))
              (function {nlocal = 0} env
                (let
                  (self =?
                     (opaque
                       (apply[unyielding]
                         (field_imm 23 (global CamlinternalOO!)) 0 class)))
                  (seq (setfield_imm_computed self x 1) self))))))
       (seq
         (opaque
           (apply[unyielding] (field_imm 16 (global CamlinternalOO!)) class))
         (opaque (apply[unyielding] obj_init 0)))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "o" o))
val o : < with_x : int -> 'a > as 'a = <obj>
|}]
