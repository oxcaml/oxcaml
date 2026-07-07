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
  'a myref @ [< 'm @@ past & global corrupted] ->
  ('a @ [< global many uncontended] -> unit @ 'n) @ [> 'm | corruptible] =
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
  'a myref @ [< 'm @@ past & corrupted > local] ->
  ('a @ [< global many uncontended] -> unit @ 'n) @ [> 'm | local corruptible] =
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
  'a myref @ [< 'm @@ past & global corrupted] ->
  ('a @ [< global many uncontended] -> unit @ 'n) @ [> 'm | corruptible] =
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
val foo : unit @ 'o -> (unit @ 'n -> unit @ 'm) @ [> corruptible] = <fun>
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

val foo : unit @ 'o -> (string myref @ [< corrupted] -> unit @ 'n) @ 'm =
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
val foo : unit @ 'o -> (unit @ 'n -> unit @ 'm) @ [> corruptible] = <fun>
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
  'a @ [< 'm > local] ->
  ('b @ 'n -> 'a @ [> 'm | local]) @ [> close('m) | local] = <fun>
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
val foo : unit @ 'n -> ('a @ 'm -> int @ [> local]) @ [> local] = <fun>
|}]
