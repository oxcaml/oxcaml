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
  (foo/292 =
     (function {nlocal = 0} r/294 x/295 : int
       (setfield_ptr(maybe-stack) 0 r/294 x/295)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/292))
val foo :
  'a myref @ [< 'm @@ past & global corrupted] ->
  ('a @ [< global many uncontended] -> unit @ 'n) @ [> 'm | corruptible] =
  <fun>
|}]

let foo (r @ local) x = r.i <- x
[%%expect{|
(let
  (foo/296 =
     (function {nlocal = 2} r/297[L] x/298 : int
       (setfield_ptr(maybe-stack) 0 r/297 x/298)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/296))
val foo :
  'a myref @ [< 'm @@ past & corrupted > local] ->
  ('a @ [< global many uncontended] -> unit @ 'n) @ [> 'm | local corruptible] =
  <fun>
|}]

(* Can be [setfield_ptr] *)
let foo (r @ global) x = r.i <- x
[%%expect{|
(let
  (foo/299 =
     (function {nlocal = 0} r/300 x/301 : int (setfield_ptr 0 r/300 x/301)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/299))
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
  (foo/302 =
     (function {nlocal = 0} param/308[value<int>]
       (let
         (r/303 = (makemutable 0 (*) "bar")
          store/304 =
            (function {nlocal = 0} r/306 : int
              (setfield_ptr 0 r/306 "foobar")))
         (function {nlocal = 0} param/307[value<int>] : int
           (apply store/304 r/303)))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/302))
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
  (foo/310 =
     (function {nlocal = 0} param/315[value<int>]
       (region
         (let (r/311 =mut "bar")
           (function {nlocal = 0} r/314 : int
             (setfield_ptr(maybe-stack) 0 r/314 "foobar"))))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/310))

val foo : unit @ 'o -> (string myref @ [< corrupted] -> unit @ 'n) @ 'm =
  <fun>
|}]

let foo () =
  let r @ global = { i = "bar" } in
  let store r = r.i <- "foobar" in
  fun () -> store r
[%%expect{|
(let
  (foo/317 =
     (function {nlocal = 0} param/323[value<int>]
       (let
         (r/318 = (makemutable 0 (*) "bar")
          store/319 =
            (function {nlocal = 0} r/321 : int
              (setfield_ptr 0 r/321 "foobar")))
         (function {nlocal = 0} param/322[value<int>] : int
           (apply store/319 r/318)))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/317))
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
  (fst/325 =
     (function {nlocal = 0} x/326? (function {nlocal = 0} y/327? x/326)))
  (apply (field_imm 1 (global Toploop!)) "fst" fst/325))
val fst : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

let fst' x y = x
[%%expect{|
(let (fst'/328 = (function {nlocal = 0} x/330? y/331? x/330))
  (apply (field_imm 1 (global Toploop!)) "fst'" fst'/328))
val fst' : 'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [> 'm]) @ [> close('m)] =
  <fun>
|}]

(* if explicitly annotated, the returned function is local [function[L]],
  the inner [x] is returned locally *)
let fst_local (x @ local) = exclave_ fun y -> x
[%%expect{|
(let
  (fst_local/332 =
     (function {nlocal = 1} x/334[L]? : stack
       (function[L] {nlocal = 1} y/335? x/334)))
  (apply (field_imm 1 (global Toploop!)) "fst_local" fst_local/332))
val fst_local :
  'a @ [< 'm > local] ->
  ('b @ 'n -> 'a @ [> 'm | local]) @ [> close('m) | local] = <fun>
|}]

let foo = fst 42
[%%expect{|
(let
  (fst/325 =? (apply (field_imm 0 (global Toploop!)) "fst")
   foo/336 = (apply fst/325 42))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/336))
val foo : '_weak1 -> int @ [> aliased] = <fun>
|}]

let foo () =
  exclave_ (fst_local 42)
[%%expect{|
(let
  (fst_local/332 =? (apply (field_imm 0 (global Toploop!)) "fst_local")
   foo/337 =
     (function {nlocal = 1} param/338[value<int>] : stack
       (apply[L] fst_local/332 42)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/337))
val foo : unit @ 'n -> ('a @ 'm -> int @ [> local]) @ [> local] = <fun>
|}]
