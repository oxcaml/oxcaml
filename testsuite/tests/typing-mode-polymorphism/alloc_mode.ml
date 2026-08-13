(* TEST
 flags += "-dlambda -extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
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

(* Must be [setfield_ptr(maybe-stack)] *)
let foo r x = r.i <- x
[%%expect{|
(let
  (foo/331 =
     (function {nlocal = 0} r/333[L] x/334 : int
       (setfield_ptr(maybe-stack) 0 r/333 x/334)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/331))
val foo :
  'a myref @ [< 'm @@ past & global corrupted write] ->
  ('a @ [< global many uncontended forkable unyielding read_write] ->
   unit @ 'n) @ [> 'm | corruptible writing] =
  <fun>
|}]

let foo (r @ local) x = r.i <- x
[%%expect{|
(let
  (foo/335 =
     (function {nlocal = 2} r/336[L] x/337 : int
       (setfield_ptr(maybe-stack) 0 r/336 x/337)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/335))
val foo :
  'a myref @ [< 'm @@ past & corrupted write > local unforkable yielding] ->
  ('a @ [< global many uncontended forkable unyielding read_write] ->
   unit @ 'n) @ [> 'm | local corruptible unforkable yielding writing] =
  <fun>
|}]

(* Can be [setfield_ptr] *)
let foo (r @ global) x = r.i <- x
[%%expect{|
(let
  (foo/338 =
     (function {nlocal = 0} r/339 x/340 : int (setfield_ptr 0 r/339 x/340)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/338))
val foo :
  'a myref @ [< 'm @@ past & global corrupted forkable unyielding write] ->
  ('a @ [< global many uncontended forkable unyielding read_write] ->
   unit @ 'n) @ [> 'm | corruptible writing] =
  <fun>
|}]

let foo () =
  let r = { i = "bar" } in
  let store r = r.i <- "foobar" in
  fun () -> store r
[%%expect{|
(let
  (foo/341 =
     (function {nlocal = 1} param/347[L][value<int>]
       (let
         (r/342 = (makemutable 0 (*) "bar")
          store/343 =
            (function {nlocal = 0} r/345 : int
              (setfield_ptr 0 r/345 "foobar")))
         (function {nlocal = 1} param/346[L][value<int>] : int
           (apply store/343 r/342)))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/341))
val foo :
  unit @ 'n -> (unit @ 'm -> unit @ [> dynamic]) @ [> corruptible writing] =
  <fun>
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
  (foo/349 =
     (function {nlocal = 1} param/354[L][value<int>]
       (region
         (let (r/350 =mut "bar")
           (function {nlocal = 1} r/353[L] : int
             (setfield_ptr(maybe-stack) 0 r/353 "foobar"))))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/349))

val foo : unit @ 'o -> (string myref @ [< corrupted write] -> unit @ 'n) @ 'm =
  <fun>
|}]

let foo () =
  let r @ global = { i = "bar" } in
  let store r = r.i <- "foobar" in
  fun () -> store r
[%%expect{|
(let
  (foo/356 =
     (function {nlocal = 1} param/362[L][value<int>]
       (let
         (r/357 = (makemutable 0 (*) "bar")
          store/358 =
            (function {nlocal = 0} r/360 : int
              (setfield_ptr 0 r/360 "foobar")))
         (function {nlocal = 1} param/361[L][value<int>] : int
           (apply store/358 r/357)))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/356))
val foo :
  unit @ 'n -> (unit @ 'm -> unit @ [> dynamic]) @ [> corruptible writing] =
  <fun>
|}]


(* FUNCTIONS *)

(* In order to soundly choose the allocation of a return value, functions default
  to global returns, unless a return is explicitly set to local, in which case it must
  be always local.

  The following tests assert the locality of returned functions *)

let fst x = fun y -> x
[%%expect{|
(let
  (fst/364 =
     (function {nlocal = 0} x/365? (function {nlocal = 1} y/366[L]? x/365)))
  (apply (field_imm 1 (global Toploop!)) "fst" fst/364))
val fst : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

let fst' x y = x
[%%expect{|
(let (fst'/367 = (function {nlocal = 1} x/369[L]? y/370[L]? x/369))
  (apply (field_imm 1 (global Toploop!)) "fst'" fst'/367))
val fst' : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

(* if explicitly annotated, the returned function is local [function[L]],
  the inner [x] is returned locally *)
let fst_local (x @ local) = exclave_ fun y -> x
[%%expect{|
(let
  (fst_local/371 =
     (function {nlocal = 1} x/373[L]? : stack
       (function[L] {nlocal = 1} y/374[L]? x/373)))
  (apply (field_imm 1 (global Toploop!)) "fst_local" fst_local/371))
val fst_local :
  'a @ [< 'm > local unforkable yielding] ->
  ('b @ 'n -> 'a @ [> 'm | local unforkable yielding]) @ [> close('m) | local unforkable yielding] =
  <fun>
|}]

let foo = fst 42
[%%expect{|
(let
  (fst/364 =? (apply (field_imm 0 (global Toploop!)) "fst")
   foo/375 = (apply fst/364 42))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/375))
val foo : '_weak1 -> int @ [> aliased] = <fun>
|}]

let foo () =
  exclave_ (fst_local 42)
[%%expect{|
(let
  (fst_local/371 =? (apply (field_imm 0 (global Toploop!)) "fst_local")
   foo/376 =
     (function {nlocal = 1} param/377[L][value<int>] : stack
       (apply[L] fst_local/371 42)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/376))
val foo :
  unit @ 'n ->
  ('a @ 'm -> int @ [> local unforkable yielding]) @ [> local unforkable yielding dynamic] =
  <fun>
|}]


(* YIELDING *)

(* An application is compiled to [apply[yielding]] when the function or any of
  its arguments may be yielding, and to a plain [apply] only when all of them
  are known to be unyielding. *)

let use_yield (_ @ yielding) = ()
let use_unyielding (_ @ unyielding) = ()
let id x = x
[%%expect{|
(let (use_yield/378 = (function {nlocal = 1} param/380[L]? : int 0))
  (apply (field_imm 1 (global Toploop!)) "use_yield" use_yield/378))
val use_yield : 'a @ [> yielding] -> unit @ 'm = <fun>
(let (use_unyielding/381 = (function {nlocal = 1} param/383[L]? : int 0))
  (apply (field_imm 1 (global Toploop!)) "use_unyielding" use_unyielding/381))
val use_unyielding : 'a @ [< unyielding] -> unit @ 'm = <fun>
(let (id/384 = (function {nlocal = 1} x/386[L]? x/386))
  (apply (field_imm 1 (global Toploop!)) "id" id/384))
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

(* Toplevel [id] and constant [42] are unyielding: can be plain [apply]. *)
let apply_unyielding () = id 42
[%%expect{|
(let
  (id/384 =? (apply (field_imm 0 (global Toploop!)) "id")
   apply_unyielding/387 =
     (function {nlocal = 1} param/389[L][value<int>] : int (apply id/384 42)))
  (apply (field_imm 1 (global Toploop!)) "apply_unyielding"
    apply_unyielding/387))
val apply_unyielding : unit @ 'm -> int @ [> dynamic] = <fun>
|}]

(* [x] is yielding, despite [id] being mode-polymorphic: must be
  [apply[yielding]]. *)
let apply_yielding (x @ yielding) = id x
[%%expect{|
(let
  (id/384 =? (apply (field_imm 0 (global Toploop!)) "id")
   apply_yielding/390 =
     (function {nlocal = 0} x/392? (apply[yielding] id/384 x/392)))
  (apply (field_imm 1 (global Toploop!)) "apply_yielding" apply_yielding/390))
val apply_yielding :
  'a @ [< 'm & global > yielding] -> 'a @ [> 'm | yielding dynamic] = <fun>
|}]

(* [f] and [x] have polymorphic modes, so either may be yielding: must be
  [apply[yielding]]. *)
let app f x = f x
[%%expect{|
(let
  (app/393 =
     (function {nlocal = 1} f/395[L] x/396[L]? (apply[yielding] f/395 x/396)))
  (apply (field_imm 1 (global Toploop!)) "app" app/393))
val app :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o @@ past & global] ->
  ('a @ [< 'n] -> 'b @ [> 'm | dynamic]) @ [> 'o] = <fun>
|}]

(* Both arguments are yielding: must be [apply[yielding]]. *)
let app_yielding (f @ yielding) (x @ yielding) = app f x
[%%expect{|
(let
  (app/393 =? (apply (field_imm 0 (global Toploop!)) "app")
   app_yielding/397 =
     (function {nlocal = 1} f/399 x/400[L]?
       (apply[yielding] app/393 f/399 x/400)))
  (apply (field_imm 1 (global Toploop!)) "app_yielding" app_yielding/397))
val app_yielding :
  ('a @ [> 'n | yielding] -> 'b @ [< 'm & global]) @ [< 'o @@ past & global > yielding] ->
  ('a @ [< 'n > yielding] -> 'b @ [> 'm | dynamic]) @ [> 'o | nonportable yielding stateful] =
  <fun>
|}]

(* The value-rec wrapper forwards at the closure's yielding mode, polymorphic
  here: must be [apply[yielding]] (plain in typing-modes/yielding_lambda.ml).
  The direct call [forward (x - 1)] is unyielding: can be plain [apply]. *)
let rec forward =
  let g = fun x -> if x <= 0 then 0 else forward (x - 1) in
  g
[%%expect{|
(let (letrec_function_context/405 =? (caml_alloc_dummy 1))
  (letrec
    (forward/401
       (function {nlocal = 1} x/406[L][value<int>] stub : int
         (apply[yielding] (field_imm 0 letrec_function_context/405) x/406)))
    (seq
      (caml_update_dummy letrec_function_context/405
        (let
          (g/402 =
             (function {nlocal = 1} x/404[L][value<int>] : int
               (if (%int_lessequal x/404 0) 0
                 (apply forward/401 (%int_sub x/404 1)))))
          (makeblock 0 g/402)))
      (apply (field_imm 1 (global Toploop!)) "forward" forward/401))))
val forward :
  int @ [< many uncontended read_write > dynamic] ->
  int @ [< global > dynamic] = <fun>
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
  (use_yield/378 =? (apply (field_imm 0 (global Toploop!)) "use_yield")
   forward_yielding/407 =
     (function {nlocal = 0} y/409?
       (let (letrec_function_context/414 =? (caml_alloc_dummy 1))
         (letrec
           (f/410
              (function {nlocal = 1} x/415[L][value<int>] stub : int
                (apply[yielding] (field_imm 0 letrec_function_context/414)
                  x/415)))
           (seq
             (caml_update_dummy letrec_function_context/414
               (let
                 (g/411 =
                    (function {nlocal = 1} x/413[L][value<int>] : int
                      (seq (apply[yielding] use_yield/378 y/409)
                        (if (%int_lessequal x/413 0) 0
                          (apply[yielding] f/410 (%int_sub x/413 1))))))
                 (makeblock 0 g/411)))
             f/410)))))
  (apply (field_imm 1 (global Toploop!)) "forward_yielding"
    forward_yielding/407))
val forward_yielding :
  'a @ [< 'm @@ past & global many > yielding] ->
  (int @ [< many uncontended read_write > dynamic] ->
   int @ [< global > dynamic]) @ [> 'm | nonportable yielding stateful] =
  <fun>
|}]

(* A first-class primitive's synthesized application ([Id_prim]) uses its
  declared parameter modes, unyielding by default: can be plain [apply]. *)
external revapply : 'a -> ('a -> 'b) -> 'b = "%revapply"
let pipe = revapply
[%%expect{|
0
external revapply : 'a -> ('a -> 'b) -> 'b = "%revapply"
(let
  (pipe/417 =
     (function {nlocal = 0} prim/419 prim/418 stub (apply prim/418 prim/419)))
  (apply (field_imm 1 (global Toploop!)) "pipe" pipe/417))
val pipe : 'a -> ('a -> 'b) -> 'b = <fun>
|}]
