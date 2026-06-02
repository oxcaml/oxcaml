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
  (foo/293 =
     (function {nlocal = 0} r/295[L] x/296 : int
       (setfield_ptr(maybe-stack) 0 r/295 x/296)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/293))
val foo :
  'a myref @ [< 'm @@ past & global corrupted write] ->
  ('a @ [< global many uncontended forkable unyielding read_write] ->
   unit @ 'n) @ [> 'm | corruptible writing] =
  <fun>
|}]

let foo (r @ local) x = r.i <- x
[%%expect{|
(let
  (foo/297 =
     (function {nlocal = 2} r/298[L] x/299 : int
       (setfield_ptr(maybe-stack) 0 r/298 x/299)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/297))
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
  (foo/300 =
     (function {nlocal = 0} r/301 x/302 : int (setfield_ptr 0 r/301 x/302)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/300))
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
  (foo/303 =
     (function {nlocal = 1} param/309[L][value<int>]
       (let
         (r/304 = (makemutable 0 (*) "bar")
          store/305 =
            (function {nlocal = 0} r/307 : int
              (setfield_ptr 0 r/307 "foobar")))
         (function {nlocal = 1} param/308[L][value<int>] : int
           (apply store/305 r/304)))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/303))
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
  (foo/311 =
     (function {nlocal = 1} param/316[L][value<int>]
       (region
         (let (r/312 =mut "bar")
           (function {nlocal = 1} r/315[L] : int
             (setfield_ptr(maybe-stack) 0 r/315 "foobar"))))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/311))

val foo : unit @ 'o -> (string myref @ [< corrupted write] -> unit @ 'n) @ 'm =
  <fun>
|}]

let foo () =
  let r @ global = { i = "bar" } in
  let store r = r.i <- "foobar" in
  fun () -> store r
[%%expect{|
(let
  (foo/318 =
     (function {nlocal = 1} param/324[L][value<int>]
       (let
         (r/319 = (makemutable 0 (*) "bar")
          store/320 =
            (function {nlocal = 0} r/322 : int
              (setfield_ptr 0 r/322 "foobar")))
         (function {nlocal = 1} param/323[L][value<int>] : int
           (apply store/320 r/319)))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/318))
val foo :
  unit @ 'n -> (unit @ 'm -> unit @ [> dynamic]) @ [> corruptible writing] =
  <fun>
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
  (fst/326 =
     (function {nlocal = 0} x/327? (function {nlocal = 1} y/328[L]? x/327)))
  (apply (field_imm 1 (global Toploop!)) "fst" fst/326))
val fst : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

let fst' x y = x
[%%expect{|
(let (fst'/329 = (function {nlocal = 1} x/331[L]? y/332[L]? x/331))
  (apply (field_imm 1 (global Toploop!)) "fst'" fst'/329))
val fst' : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

(* if explicitly annotated, the returned function is local [function[L]],
  the inner [x] is returned locally *)
let fst_local (x @ local) = exclave_ fun y -> x
[%%expect{|
(let
  (fst_local/333 =
     (function {nlocal = 1} x/335[L]? : stack
       (function[L] {nlocal = 1} y/336[L]? x/335)))
  (apply (field_imm 1 (global Toploop!)) "fst_local" fst_local/333))
val fst_local :
  'a @ [< 'm > local unforkable yielding] ->
  ('b @ 'n -> 'a @ [> 'm | local unforkable yielding]) @ [> close('m) | local unforkable yielding] =
  <fun>
|}]

let foo = fst 42
[%%expect{|
(let
  (fst/326 =? (apply (field_imm 0 (global Toploop!)) "fst")
   foo/337 = (apply fst/326 42))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/337))
val foo : '_weak1 -> int @ [> aliased] = <fun>
|}]

let foo () =
  exclave_ (fst_local 42)
[%%expect{|
(let
  (fst_local/333 =? (apply (field_imm 0 (global Toploop!)) "fst_local")
   foo/338 =
     (function {nlocal = 1} param/339[L][value<int>] : stack
       (apply[L] fst_local/333 42)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/338))
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
(let (use_yield/340 = (function {nlocal = 1} param/342[L]? : int 0))
  (apply (field_imm 1 (global Toploop!)) "use_yield" use_yield/340))
val use_yield : 'a @ [> yielding] -> unit @ 'm = <fun>
(let (use_unyielding/343 = (function {nlocal = 1} param/345[L]? : int 0))
  (apply (field_imm 1 (global Toploop!)) "use_unyielding" use_unyielding/343))
val use_unyielding : 'a @ [< unyielding] -> unit @ 'm = <fun>
(let (id/346 = (function {nlocal = 1} x/348[L]? x/348))
  (apply (field_imm 1 (global Toploop!)) "id" id/346))
val id : 'a @ [< 'm] -> 'a @ [> 'm] = <fun>
|}]

(* Toplevel [id] and constant [42] are unyielding: can be plain [apply]. *)
let apply_unyielding () = id 42
[%%expect{|
(let
  (id/346 =? (apply (field_imm 0 (global Toploop!)) "id")
   apply_unyielding/349 =
     (function {nlocal = 1} param/351[L][value<int>] : int (apply id/346 42)))
  (apply (field_imm 1 (global Toploop!)) "apply_unyielding"
    apply_unyielding/349))
val apply_unyielding : unit @ 'm -> int @ [> dynamic] = <fun>
|}]

(* [x] is yielding, despite [id] being mode-polymorphic: must be
  [apply[yielding]]. *)
let apply_yielding (x @ yielding) = id x
[%%expect{|
(let
  (id/346 =? (apply (field_imm 0 (global Toploop!)) "id")
   apply_yielding/352 =
     (function {nlocal = 0} x/354? (apply[yielding] id/346 x/354)))
  (apply (field_imm 1 (global Toploop!)) "apply_yielding" apply_yielding/352))
val apply_yielding :
  'a @ [< 'm & global > yielding] -> 'a @ [> 'm | yielding dynamic] = <fun>
|}]

(* [f] and [x] have polymorphic modes, so either may be yielding: must be
  [apply[yielding]]. *)
let app f x = f x
[%%expect{|
(let
  (app/355 =
     (function {nlocal = 1} f/357[L] x/358[L]? (apply[yielding] f/357 x/358)))
  (apply (field_imm 1 (global Toploop!)) "app" app/355))
val app :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o @@ past & global] ->
  ('a @ [< 'n] -> 'b @ [> 'm | dynamic]) @ [> 'o] = <fun>
|}]

(* Both arguments are yielding: must be [apply[yielding]]. *)
let app_yielding (f @ yielding) (x @ yielding) = app f x
[%%expect{|
(let
  (app/355 =? (apply (field_imm 0 (global Toploop!)) "app")
   app_yielding/359 =
     (function {nlocal = 1} f/361 x/362[L]?
       (apply[yielding] app/355 f/361 x/362)))
  (apply (field_imm 1 (global Toploop!)) "app_yielding" app_yielding/359))
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
(let (letrec_function_context/367 =? (caml_alloc_dummy 1))
  (letrec
    (forward/363
       (function {nlocal = 1} x/368[L][value<int>] stub : int
         (apply[yielding] (field_imm 0 letrec_function_context/367) x/368)))
    (seq
      (caml_update_dummy letrec_function_context/367
        (let
          (g/364 =
             (function {nlocal = 1} x/366[L][value<int>] : int
               (if (%int_lessequal x/366 0) 0
                 (apply forward/363 (%int_sub x/366 1)))))
          (makeblock 0 g/364)))
      (apply (field_imm 1 (global Toploop!)) "forward" forward/363))))
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
  (use_yield/340 =? (apply (field_imm 0 (global Toploop!)) "use_yield")
   forward_yielding/369 =
     (function {nlocal = 0} y/371?
       (let (letrec_function_context/376 =? (caml_alloc_dummy 1))
         (letrec
           (f/372
              (function {nlocal = 1} x/377[L][value<int>] stub : int
                (apply[yielding] (field_imm 0 letrec_function_context/376)
                  x/377)))
           (seq
             (caml_update_dummy letrec_function_context/376
               (let
                 (g/373 =
                    (function {nlocal = 1} x/375[L][value<int>] : int
                      (seq (apply[yielding] use_yield/340 y/371)
                        (if (%int_lessequal x/375 0) 0
                          (apply[yielding] f/372 (%int_sub x/375 1))))))
                 (makeblock 0 g/373)))
             f/372)))))
  (apply (field_imm 1 (global Toploop!)) "forward_yielding"
    forward_yielding/369))
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
  (pipe/379 =
     (function {nlocal = 0} prim/381 prim/380 stub (apply prim/380 prim/381)))
  (apply (field_imm 1 (global Toploop!)) "pipe" pipe/379))
val pipe : 'a -> ('a -> 'b) -> 'b = <fun>
|}]
