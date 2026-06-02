(* TEST
 flags += "-dlambda -extension mode_polymorphism_alpha";
 expect;
*)

(* MUTABLE RECORD FIELDS *)

(* If mutating a record field can be done on both local and global
  records, it must be compiled to caml_modify_local.
  Only when the record is always global can be compiled as caml_modify *)

type 'a myref = { mutable i : 'a }
[%%expect{|
0
type 'a myref = { mutable i : 'a; }
|}]

(* CR ageorges: the following can accept stack locations but
  uses setfield_ptr(maybe-stack). This is a soundness bug *)

(* Must be [setfield_ptr(maybe-stack)] *)
let foo r x = r.i <- x
[%%expect{|
(let
  (foo/0 = (function {nlocal = 1} r/0[L] x/0 : int (setfield_ptr 0 r/0 x/0)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/0))
val foo : 'a myref -> 'a -> unit = <fun>
|}]

let foo (r @ local) x = r.i <- x
[%%expect{|
(let
  (foo/1 =
     (function {nlocal = 1} r/1[L] x/1 : int
       (setfield_ptr(maybe-stack) 0 r/1 x/1)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/1))
val foo : 'a myref @ local -> 'a -> unit = <fun>
|}]

(* Can be [setfield_ptr] *)
let foo (r @ global) x = r.i <- x
[%%expect{|
(let (foo/2 = (function {nlocal = 1} r/2 x/2 : int (setfield_ptr 0 r/2 x/2)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/2))
val foo : 'a myref -> 'a -> unit = <fun>
|}]

let foo () =
  let r = { i = "bar" } in
  let store r = r.i <- "foobar" in
  fun () -> store r
[%%expect{|
(let
  (foo/3 =
     (function {nlocal = 1} param/0[L][value<int>] : stack
       (let
         (r/3 = (makemutable 0 (*) "bar")
          store/0 =
            (function {nlocal = 0} r/4 : int (setfield_ptr 0 r/4 "foobar")))
         (function {nlocal = 1} param/1[L][value<int>] : int
           (apply store/0 r/3)))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/3))
val foo : unit -> unit -> unit = <fun>
|}]

let foo () =
  let r @ local = { i = "bar" } in
  let store r = r.i <- "foobar" in
  store
[%%expect{|
Line 2, characters 6-7:
2 |   let r @ local = { i = "bar" } in
          ^
Warning 26 [unused-var]: unused variable "r".
(let
  (foo/4 =
     (function {nlocal = 1} param/2[L][value<int>] : stack
       (region
         (let (r/5 =mut "bar")
           (function {nlocal = 1} r/6[L] : int (setfield_ptr 0 r/6 "foobar"))))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/4))

val foo : unit -> string myref -> unit = <fun>
|}]

let foo () =
  let r @ global = { i = "bar" } in
  let store r = r.i <- "foobar" in
  fun () -> store r
[%%expect{|
(let
  (foo/5 =
     (function {nlocal = 1} param/3[L][value<int>] : stack
       (let
         (r/7 = (makemutable 0 (*) "bar")
          store/1 =
            (function {nlocal = 0} r/8 : int (setfield_ptr 0 r/8 "foobar")))
         (function {nlocal = 1} param/4[L][value<int>] : int
           (apply store/1 r/7)))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/5))
val foo : unit -> unit -> unit = <fun>
|}]


(* FUNCTIONS *)

(* In order to soundly choose the allocation of a return value, functions default
  to global returns, unless a return is explicitly set to local, in which case it must
  be always local.

  The following tests assert the locality of returned functions *)

(* CR ageorges: the following two functions return local functions.
  This will cause a crash if applied as global, (see [foo] below),
  and is unsound *)

let fst x = fun y -> x
[%%expect{|
(let
  (fst/0 =
     (function {nlocal = 1} x/3? : stack
       (function {nlocal = 1} y/0[L]? : stack x/3)))
  (apply (field_imm 1 (global Toploop!)) "fst" fst/0))
val fst : 'a -> 'b -> 'a = <fun>
|}]

let fst' x y = x
[%%expect{|
(let (fst'/0 = (function {nlocal = 1} x/4[L]? y/1[L]? : stack x/4))
  (apply (field_imm 1 (global Toploop!)) "fst'" fst'/0))
val fst' : 'a -> 'b -> 'a = <fun>
|}]

(* if explicitly annotated, the returned function is local [function[L]],
  the inner [x] is returned locally *)
let fst_local (x @ local) = exclave_ fun y -> x
[%%expect{|
(let
  (fst_local/0 =
     (function {nlocal = 1} x/5[L]? : stack
       (function[L] {nlocal = 1} y/2[L]? : stack x/5)))
  (apply (field_imm 1 (global Toploop!)) "fst_local" fst_local/0))
val fst_local : 'a @ local -> 'b -> 'a @ local = <fun>
|}]

let foo = fst 42
[%%expect{|
(let
  (fst/0 =? (apply (field_imm 0 (global Toploop!)) "fst")
   foo/6 = (apply fst/0 42))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/6))
val foo : '_weak1 -> int = <fun>
|}]

let foo () =
  exclave_ (fst_local 42)
[%%expect{|
(let
  (fst_local/0 =? (apply (field_imm 0 (global Toploop!)) "fst_local")
   foo/7 =
     (function {nlocal = 1} param/5[L][value<int>] : stack
       (apply[L] fst_local/0 42)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/7))
val foo : unit -> ('a -> int @ local) @ local = <fun>
|}]


(* YIELDING *)

(* An application is compiled to [apply[yielding]] when the function or any of
  its arguments may be yielding, and to a plain [apply] only when all of them
  are known to be unyielding. *)

let use_yield (_ @ yielding) = ()
let use_unyielding (_ @ unyielding) = ()
let id x = x
[%%expect{|
(let (use_yield/0 = (function {nlocal = 1} param/6[L]? : int 0))
  (apply (field_imm 1 (global Toploop!)) "use_yield" use_yield/0))
val use_yield : 'a @ yielding -> unit = <fun>
(let (use_unyielding/0 = (function {nlocal = 1} param/7[L]? : int 0))
  (apply (field_imm 1 (global Toploop!)) "use_unyielding" use_unyielding/0))
val use_unyielding : 'a -> unit = <fun>
(let (id/0 = (function {nlocal = 1} x/6[L]? : stack x/6))
  (apply (field_imm 1 (global Toploop!)) "id" id/0))
val id : 'a -> 'a = <fun>
|}]

(* Toplevel [id] and constant [42] are unyielding: can be plain [apply]. *)
let apply_unyielding () = id 42
[%%expect{|
(let
  (id/0 =? (apply (field_imm 0 (global Toploop!)) "id")
   apply_unyielding/0 =
     (function {nlocal = 1} param/8[L][value<int>] : int (apply id/0 42)))
  (apply (field_imm 1 (global Toploop!)) "apply_unyielding"
    apply_unyielding/0))
val apply_unyielding : unit -> int = <fun>
|}]

(* [x] is yielding, despite [id] being mode-polymorphic: must be
  [apply[yielding]]. *)
let apply_yielding (x @ yielding) = id x
[%%expect{|
(let
  (id/0 =? (apply (field_imm 0 (global Toploop!)) "id")
   apply_yielding/0 =
     (function {nlocal = 1} x/7? : stack (apply[yielding] id/0 x/7)))
  (apply (field_imm 1 (global Toploop!)) "apply_yielding" apply_yielding/0))
val apply_yielding : 'a @ yielding -> 'a @ yielding = <fun>
|}]

(* [f] and [x] have polymorphic modes, so either may be yielding: must be
  [apply[yielding]]. *)
let app f x = f x
[%%expect{|
(let
  (app/0 =
     (function {nlocal = 1} f/0[L] x/8[L]? : stack (apply[yielding] f/0 x/8)))
  (apply (field_imm 1 (global Toploop!)) "app" app/0))
val app : ('a -> 'b) -> 'a -> 'b = <fun>
|}]

(* Both arguments are yielding: must be [apply[yielding]]. *)
let app_yielding (f @ yielding) (x @ yielding) = app f x
[%%expect{|
(let
  (app/0 =? (apply (field_imm 0 (global Toploop!)) "app")
   app_yielding/0 =
     (function {nlocal = 1} f/1[L] x/9[L]? : stack
       (apply[yielding] app/0 f/1 x/9)))
  (apply (field_imm 1 (global Toploop!)) "app_yielding" app_yielding/0))
val app_yielding : ('a @ yielding -> 'b) @ yielding -> 'a @ yielding -> 'b =
  <fun>
|}]

(* The value-rec wrapper forwards at the closure's yielding mode, polymorphic
  here: must be [apply[yielding]] (plain in typing-modes/yielding_lambda.ml).
  The direct call [forward (x - 1)] is unyielding: can be plain [apply]. *)
let rec forward =
  let g = fun x -> if x <= 0 then 0 else forward (x - 1) in
  g
[%%expect{|
(let (letrec_function_context/0 =? (caml_alloc_dummy 1))
  (letrec
    (forward/0
       (function {nlocal = 1} x/10[L][value<int>] stub : int
         (apply[yielding] (field_imm 0 letrec_function_context/0) x/10)))
    (seq
      (caml_update_dummy letrec_function_context/0
        (let
          (g/0 =
             (function {nlocal = 1} x/11[L][value<int>] : int
               (if (%int_lessequal x/11 0) 0
                 (apply forward/0 (%int_sub x/11 1)))))
          (makeblock 0 g/0)))
      (apply (field_imm 1 (global Toploop!)) "forward" forward/0))))
val forward : int -> int = <fun>
|}]

(* Same wrapper, but closing over the yielding [y]: all calls must be
  [apply[yielding]]. *)
let forward_yielding (y @ yielding) =
  let rec f =
    let g = fun x -> use_yield y; if x <= 0 then 0 else f (x - 1) in
    g
  in
  f
[%%expect{|
(let
  (use_yield/0 =? (apply (field_imm 0 (global Toploop!)) "use_yield")
   forward_yielding/0 =
     (function {nlocal = 1} y/3? : stack
       (let (letrec_function_context/1 =? (caml_alloc_dummy 1))
         (letrec
           (f/2
              (function {nlocal = 1} x/12[L][value<int>] stub : int
                (apply[yielding] (field_imm 0 letrec_function_context/1)
                  x/12)))
           (seq
             (caml_update_dummy letrec_function_context/1
               (let
                 (g/1 =
                    (function {nlocal = 1} x/13[L][value<int>] : int
                      (seq (apply[yielding] use_yield/0 y/3)
                        (if (%int_lessequal x/13 0) 0
                          (apply[yielding] f/2 (%int_sub x/13 1))))))
                 (makeblock 0 g/1)))
             f/2)))))
  (apply (field_imm 1 (global Toploop!)) "forward_yielding"
    forward_yielding/0))
val forward_yielding : 'a @ yielding -> int -> int = <fun>
|}]

(* A first-class primitive's synthesized application ([Id_prim]) uses its
  declared parameter modes, unyielding by default: can be plain [apply]. *)
external revapply : 'a -> ('a -> 'b) -> 'b = "%revapply"
let pipe = revapply
[%%expect{|
0
external revapply : 'a -> ('a -> 'b) -> 'b = "%revapply"
(let
  (pipe/0 = (function {nlocal = 0} prim/0 prim/1 stub (apply prim/1 prim/0)))
  (apply (field_imm 1 (global Toploop!)) "pipe" pipe/0))
val pipe : 'a -> ('a -> 'b) -> 'b = <fun>
|}]
