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


(* A recursive binding whose definition isn't a literal [fun] -- here
   [let g = fun ... in g] -- is compiled by [Value_rec_compiler] through an
   eta-expanding wrapper [fun x -> <lifted f> x]. The wrapper forwards to [f],
   so its call is [unyielding] exactly when [f] is; here [f] is unyielding,
   so we get [apply[unyielding]]. (This relies on threading the closure's
   yielding mode through [Lambda.lfunction].) *)
let rec f =
  let g = fun x -> if x <= 0 then 0 else f (x - 1) in
  g
[%%expect{|
(let (letrec_function_context =? (caml_alloc_dummy 1))
  (letrec
    (f
       (function {nlocal = 0} x[value<int>] stub : int
         (apply[unyielding] (field_imm 0 letrec_function_context) x)))
    (seq
      (caml_update_dummy letrec_function_context
        (let
          (g =
             (function {nlocal = 0} x[value<int>] : int
               (if (%int_lessequal x 0) 0
                 (apply[unyielding] f (%int_sub x 1)))))
          (makeblock 0 g)))
      (apply[unyielding] (field_imm 1 (global Toploop!)) "f" f))))
val f : int -> int = <fun>
|}]

(* With mutual recursion this time *)
let (_ : int) =
  let rec f =
    let g = fun x ->
      if x <= 0
      then 0
      else begin match h with
        | `Foo f -> f (x - 1)
      end in
    g
  and h =
    `Foo (fun x -> f x)
  in
  f 5
[%%expect{|
(let
  (letrec_function_context =? (caml_alloc_dummy 1) h =? (caml_alloc_dummy 2))
  (letrec
    (f
       (function {nlocal = 0} x[value<int>] stub : int
         (apply[unyielding] (field_imm 0 letrec_function_context) x)))
    (seq
      (caml_update_dummy letrec_function_context
        (let
          (g =
             (function {nlocal = 0} x[value<int>] : int
               (if (%int_lessequal x 0) 0
                 (apply[unyielding] (field_imm 1 h) (%int_sub x 1)))))
          (makeblock 0 g)))
      (caml_update_dummy h
        (makeblock 0 3505894
          (function {nlocal = 0} x[value<int>] : int (apply[unyielding] f x))))
      (apply[unyielding] f 5))))
- : int = 0
|}]

(* Mutually recursive functions which are not themselves yielding, but take a
   yielding argument *)
let (_ : int) =
  Yielding.with_ begin fun y ->
    let rec f =
      let g = fun y x ->
        if x <= 0
        then 0
        else begin match h with
          | `Foo f -> f y (x - 1)
        end in
      g
    and h =
      `Foo (fun y x -> f y x)
    in
    f y 5
  end
;;
[%%expect{|
(let
  (Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295"))
  (apply[unyielding] (field_imm 0 Yielding)
    (function {nlocal = 0} y : int
      (let
        (letrec_function_context =? (caml_alloc_dummy 1)
         h =? (caml_alloc_dummy 2))
        (letrec
          (f
             (function {nlocal = 2} y? x[value<int>] stub : int
               (apply (field_imm 0 letrec_function_context) y x)))
          (seq
            (caml_update_dummy letrec_function_context
              (let
                (g =
                   (function {nlocal = 2} y? x[value<int>] : int
                     (if (%int_lessequal x 0) 0
                       (apply (field_imm 1 h) y (%int_sub x 1)))))
                (makeblock 0 g)))
            (caml_update_dummy h
              (makeblock 0 3505894
                (function {nlocal = 2} y? x[value<int>] : int (apply f y x))))
            (apply f y 5)))))))
- : int = 0
|}]


(* The same wrapper, but for a recursive function that's yielding. The
   forwarding call correctly stays a plain [apply] (may-yield) *)
let (_ : int) =
  Yielding.with_ (fun y ->
    let rec f =
      let g = fun x ->
        yield y;
        if x <= 0
        then 0
        else begin match h with
          | `Foo f -> f (x - 1)
        end in
      g
    and h =
      `Foo (fun x -> yield y; f x)
    in
    f 5)
[%%expect{|
(let
  (yield =? (apply[unyielding] (field_imm 0 (global Toploop!)) "yield")
   Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295"))
  (apply[unyielding] (field_imm 0 Yielding)
    (function {nlocal = 0} y : int
      (let
        (letrec_function_context =? (caml_alloc_dummy 1)
         h =? (caml_alloc_dummy 2))
        (letrec
          (f
             (function {nlocal = 0} x[value<int>] stub : int
               (apply (field_imm 0 letrec_function_context) x)))
          (seq
            (caml_update_dummy letrec_function_context
              (let
                (g =
                   (function {nlocal = 0} x[value<int>] : int
                     (seq (apply yield y)
                       (if (%int_lessequal x 0) 0
                         (apply (field_imm 1 h) (%int_sub x 1))))))
                (makeblock 0 g)))
            (caml_update_dummy h
              (makeblock 0 3505894
                (function {nlocal = 0} x[value<int>] : int
                  (seq (apply yield y) (apply f x)))))
            (apply f 5)))))))
- : int = 0
|}]


(* The same wrapper, but the function is yielding because it receives a
   yielding value as the implicit parameter of a [function | ...] body (rather
   than closing over one). The forwarding call must still be a plain [apply]. *)
let () =
  Yielding.with_ (fun y ->
    let rec f =
      let g = function
        | yarg -> (match h with `Foo f -> f yarg)
      in g
    and h = `Foo (fun yarg -> yield yarg)
    in
    f y)
[%%expect{|
(let
  (yield =? (apply[unyielding] (field_imm 0 (global Toploop!)) "yield")
   Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295")
   *match* =[value<int>]
     (apply[unyielding] (field_imm 0 Yielding)
       (function {nlocal = 0} y : int
         (let
           (letrec_function_context =? (caml_alloc_dummy 1)
            h =? (caml_alloc_dummy 2))
           (letrec
             (f
                (function {nlocal = 0} yarg stub : int
                  (apply (field_imm 0 letrec_function_context) yarg)))
             (seq
               (caml_update_dummy letrec_function_context
                 (let
                   (g =
                      (function {nlocal = 0} yarg : int
                        (apply (field_imm 1 h) yarg)))
                   (makeblock 0 g)))
               (caml_update_dummy h
                 (makeblock 0 3505894
                   (function {nlocal = 0} yarg : int (apply yield yarg))))
               (apply f y)))))))
  0)
|}]


(* Functor application runs the functor body. It is unyielding when the
   functor closes over nothing yielding (its own mode) and is given no
   yielding argument (the argument's mode) -- as here. *)
module F (X : sig val n : int end) = struct let m = X.n + 1 end
module N = struct let n = 41 end
module R = F(N)
[%%expect{|
(apply[unyielding] (field_imm 1 (global Toploop!)) "F/692"
  (function {nlocal = 0} X is_a_functor never_loop
    (let (m =[value<int>] (%int_add (field_imm 0 X) 1)) (makeblock 0 m))))
module F : functor (X : sig val n : int end) -> sig val m : int end
(apply[unyielding] (field_imm 1 (global Toploop!)) "N/697"
  (let (n =[value<int>] 41) (makeblock 0 n)))
module N : sig val n : int end
(let
  (N =? (apply[unyielding] (field_imm 0 (global Toploop!)) "N/697")
   F =? (apply[unyielding] (field_imm 0 (global Toploop!)) "F/692"))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "R/699"
    (apply[unyielding] F N)))
module R : sig val m : int end
|}]

(* A functor with a [@ yielding] parameter propagates yielding: a call to the
   yielding argument in its body may yield, and applying it is itself a
   yielding application (contrast the unyielding [F(N)] above). *)
module Yf (X : sig val f : unit -> unit end @ yielding) = struct
  let g () = X.f ()
end
[%%expect{|
(apply[unyielding] (field_imm 1 (global Toploop!)) "Yf/705"
  (function {nlocal = 0} X is_a_functor never_loop
    (let
      (g =
         (function {nlocal = 0} param[value<int>] : int
           (apply (field_imm 0 X) 0)))
      (makeblock 0 g))))
module Yf :
  functor (X : sig val f : unit -> unit end @ yielding) ->
    sig val g : unit -> unit end @ yielding
|}]
let () =
  Yielding.with_ (fun y ->
    let module R = Yf (struct let f () = yield y end) in
    R.g ())
[%%expect{|
(let
  (yield =? (apply[unyielding] (field_imm 0 (global Toploop!)) "yield")
   Yf =? (apply[unyielding] (field_imm 0 (global Toploop!)) "Yf/705")
   Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295")
   *match* =[value<int>]
     (apply[unyielding] (field_imm 0 Yielding)
       (function {nlocal = 0} y : int
         (let
           (R =
              (apply Yf
                (let
                  (f =
                     (function {nlocal = 0} param[value<int>] : int
                       (apply yield y)))
                  (makeblock 0 f))))
           (apply (field_imm 0 R) 0)))))
  0)
|}]

(* A functor with a default (unyielding) parameter rejects a yielding argument. *)
module Uf (X : sig val f : unit -> unit end) = struct
  let g () = X.f ()
end
let () =
  Yielding.with_ (fun y ->
    let module R = Uf (struct let f () = yield y end) in
    R.g ())
[%%expect{|
(apply[unyielding] (field_imm 1 (global Toploop!)) "Uf/721"
  (function {nlocal = 0} X is_a_functor never_loop
    (let
      (g =
         (function {nlocal = 0} param[value<int>] : int
           (apply[unyielding] (field_imm 0 X) 0)))
      (makeblock 0 g))))
module Uf :
  functor (X : sig val f : unit -> unit end) -> sig val g : unit -> unit end
Line 6, characters 19-53:
6 |     let module R = Uf (struct let f () = yield y end) in
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Modules do not match: sig val f : unit -> unit end @ yielding
     is not included in sig val f : unit -> unit end @ unyielding
     Values do not match:
       val f : unit -> unit (* in a structure at yielding *)
     is not included in
       val f : unit -> unit (* in a structure at unyielding *)
     The first is "yielding"
       because it closes over the value "y" at line 6, characters 47-48
       which is "yielding".
     However, the second is "unyielding".
|}]

(* Unlike objects (below), a module may close over a yielding value; the inner
   [yield] is correctly inferred as yielding. *)
let () =
  Yielding.with_ (fun y ->
    let module M = struct let () = yield y end in
    let module _ = M in
    ())
[%%expect{|
(let
  (yield =? (apply[unyielding] (field_imm 0 (global Toploop!)) "yield")
   Yielding =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "Yielding/295")
   *match* =[value<int>]
     (apply[unyielding] (field_imm 0 Yielding)
       (function {nlocal = 0} y : int
         (let
           (M = (let (*match* =[value<int>] (apply yield y)) (makeblock 0)))
           0))))
  0)
|}]


(* Can OO user code close over a yielding value? These tests document the mode
   in each context, which determines the [ap_yielding] for [new], method
   calls, and class compilation. *)

(* An [initializer] block: *)
let () =
  Yielding.with_ (fun y ->
    let _o = object
      initializer yield y
    end in
    ())
[%%expect{|
Line 4, characters 24-25:
4 |       initializer yield y
                            ^
Error: The value "y" is "yielding"
       but is expected to be "unyielding"
         because it is used in an object (at lines 3-5, characters 13-7).
|}]

(* An instance-variable initializer: *)
let () =
  Yielding.with_ (fun y ->
    let _o = object
      val _x : int = (yield y; 0)
    end in
    ())
[%%expect{|
Line 4, characters 28-29:
4 |       val _x : int = (yield y; 0)
                                ^
Error: The value "y" is "yielding"
       but is expected to be "unyielding"
         because it is used in an object (at lines 3-5, characters 13-7).
|}]

(* A method body: *)
let _ =
  Yielding.with_ (fun y ->
    object
      method m = yield y
    end)
[%%expect{|
Line 4, characters 23-24:
4 |       method m = yield y
                           ^
Error: The value "y" is "yielding"
       but is expected to be "unyielding"
         because it is used in an object (at lines 3-5, characters 4-7).
|}]

(* An ancestor method, called through [inherit ... as super]: *)
let _ =
  Yielding.with_ (fun y ->
    let module M = struct
      class c = object method m = yield y end
    end in
    object
      inherit M.c as super
      method n = super#m
    end)
[%%expect{|
Line 4, characters 40-41:
4 |       class c = object method m = yield y end
                                            ^
Error: The value "y" is "yielding"
       but is expected to be "unyielding"
         because it is used in a class (at line 4, characters 16-45).
|}]

(* Since object code can never yield, [new] and ancestor-method calls are
   [unyielding]. (Look for [apply[unyielding]] on [field_mut 0 c] -- the [new]
   call -- and on [m self] -- the [super#m] call.) *)
class c = object method m = 0 end
let mk () = new c
class d = object inherit c as super method n = super#m end
[%%expect{|
(let
  (c =?
     (let
       (c_init =
          (function {nlocal = 0} class?
            (let
              (m =?
                 (opaque
                   (apply[unyielding] (field_imm 6 (global CamlinternalOO!))
                     class #"m")))
              (seq
                (opaque
                  (apply[unyielding] (field_imm 10 (global CamlinternalOO!))
                    class (opaque (makeblock 0 m 0 0))))
                (function {nlocal = 0} env self?
                  (opaque
                    (apply[unyielding]
                      (field_imm 23 (global CamlinternalOO!)) self class)))))))
       (opaque
         (apply[unyielding] (field_imm 18 (global CamlinternalOO!))
           (opaque [0: #"m"]) c_init))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "c/769" c))
class c : object method m : int end
(let
  (c =? (apply[unyielding] (field_imm 0 (global Toploop!)) "c/769")
   mk =
     (function {nlocal = 0} param[value<int>]
       (apply[unyielding] (field_mut 0 c) 0)))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "mk" mk))
val mk : unit -> c = <fun>
(let
  (c =? (apply[unyielding] (field_imm 0 (global Toploop!)) "c/769")
   shared =a (opaque [0: #"m"])
   shared =a (opaque [0: #"n" #"m"])
   d =?
     (let
       (d_init =
          (function {nlocal = 0} class?
            (let
              (ids =?
                 (opaque
                   (apply[unyielding] (field_imm 7 (global CamlinternalOO!))
                     class shared))
               n =o? (field_mut 0 ids)
               inh =[value<genarray>]
                 (opaque
                   (apply[unyielding] (field_imm 17 (global CamlinternalOO!))
                     class 0 0 shared c 1))
               obj_init =o? (field_mut 0 inh)
               m =o? (field_mut 1 inh))
              (seq
                (opaque
                  (apply[unyielding] (field_imm 9 (global CamlinternalOO!))
                    class n
                    (function {nlocal = 0} self-7 : int
                      (apply[unyielding] m self-7))))
                (function {nlocal = 0} env self?
                  (let
                    (self =?
                       (opaque
                         (apply[unyielding]
                           (field_imm 23 (global CamlinternalOO!)) self
                           class)))
                    (seq (opaque (apply[unyielding] obj_init self))
                      (opaque
                        (apply[unyielding]
                          (field_imm 25 (global CamlinternalOO!)) self self
                          class)))))))))
       (opaque
         (apply[unyielding] (field_imm 18 (global CamlinternalOO!))
           (opaque [0: #"m" #"n"]) d_init))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "d/797" d))
class d : object method m : int method n : int end
|}]


(* Used as a first-class value (not directly applied), [%revapply]/[%apply]
   become a closure [fun x f -> f x]. Its synthesized application is unyielding
   when the operator's parameter modes are unyielding (the default). *)
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
let std_pipe = ( |> )
[%%expect{|
0
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
(let
  (std_pipe =
     (function {nlocal = 0} prim prim stub (apply[unyielding] prim prim)))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "std_pipe" std_pipe))
val std_pipe : 'a -> ('a -> 'b) -> 'b = <fun>
|}]

(* It may yield when the operator is declared with yielding parameters. *)
(* CR dkalinichenko: I know that's not the point of the test, but shouldn't
   it be [('a @ yielding -> 'b) @ yielding]? *)
external pipe_y : 'a @ yielding -> ('a -> 'b) @ yielding -> 'b = "%revapply"
let yielding_pipe = pipe_y
[%%expect{|
0
external pipe_y : 'a @ yielding -> ('a -> 'b) @ yielding -> 'b = "%revapply"
(let
  (yielding_pipe = (function {nlocal = 0} prim prim stub (apply prim prim)))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "yielding_pipe"
    yielding_pipe))
val yielding_pipe : 'a @ yielding -> ('a -> 'b) @ yielding -> 'b = <fun>
|}]
