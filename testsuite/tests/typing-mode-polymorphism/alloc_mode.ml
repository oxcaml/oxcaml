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

(* Must be [setfield_ptr(maybe-stack)] *)
let foo r x = r.i <- x
[%%expect{|
(let
  (foo/288 =
     (function {nlocal = 0} r/290[L] x/291 : int
       (setfield_ptr(maybe-stack) 0 r/290 x/291)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/288))
val foo :
  'a myref @ [< global uncontended] ->
  ('a @ [< global many uncontended] -> unit @ [< global]) @ [< global > nonportable] =
  <fun>
|}]

let foo (r @ local) x = r.i <- x
[%%expect{|
(let
  (foo/292 =
     (function {nlocal = 2} r/293[L] x/294 : int
       (setfield_ptr(maybe-stack) 0 r/293 x/294)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/292))
val foo :
  'a myref @ [< uncontended > local] ->
  ('a @ [< global many uncontended] -> unit @ [< global]) @ [> local nonportable] =
  <fun>
|}]

(* Can be [setfield_ptr] *)
let foo (r @ global) x = r.i <- x
[%%expect{|
(let
  (foo/295 =
     (function {nlocal = 0} r/296 x/297 : int (setfield_ptr 0 r/296 x/297)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/295))
val foo :
  'a myref @ [< global uncontended] ->
  ('a @ [< global many uncontended] -> unit @ [< global]) @ [< global > nonportable] =
  <fun>
|}]

let foo () =
  let r = { i = "bar" } in
  let store r = r.i <- "foobar" in
  fun () -> store r
[%%expect{|
(let
  (foo/298 =
     (function {nlocal = 1} param/304[L][value<int>]
       (let
         (r/299 = (makemutable 0 (*) "bar")
          store/300 =
            (function {nlocal = 0} r/302 : int
              (setfield_ptr 0 r/302 "foobar")))
         (function {nlocal = 1} param/303[L][value<int>] : int
           (apply store/300 r/299)))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/298))
val foo :
  unit @ 'n -> (unit @ 'm -> unit @ [< global]) @ [< global > nonportable] =
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
Warning 26 [unused-var]: unused variable r.
(let
  (foo/306 =
     (function {nlocal = 1} param/311[L][value<int>]
       (region
         (let (r/307 =mut "bar")
           (function {nlocal = 1} r/310[L] : int
             (setfield_ptr(maybe-stack) 0 r/310 "foobar"))))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/306))

val foo :
  unit @ 'm ->
  (string myref @ [< uncontended] -> unit @ [< global]) @ [< global] = <fun>
|}]

let foo () =
  let r @ global = { i = "bar" } in
  let store r = r.i <- "foobar" in
  fun () -> store r
[%%expect{|
(let
  (foo/313 =
     (function {nlocal = 1} param/319[L][value<int>]
       (let
         (r/314 = (makemutable 0 (*) "bar")
          store/315 =
            (function {nlocal = 0} r/317 : int
              (setfield_ptr 0 r/317 "foobar")))
         (function {nlocal = 1} param/318[L][value<int>] : int
           (apply store/315 r/314)))))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/313))
val foo :
  unit @ 'n -> (unit @ 'm -> unit @ [< global]) @ [< global > nonportable] =
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
  (fst/321 =
     (function {nlocal = 0} x/322? (function {nlocal = 1} y/323[L]? x/322)))
  (apply (field_imm 1 (global Toploop!)) "fst" fst/321))
val fst :
  'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [< global > 'm]) @ [< global] =
  <fun>
|}]

let fst' x y = x
[%%expect{|
(let (fst'/324 = (function {nlocal = 1} x/326[L]? y/327[L]? x/326))
  (apply (field_imm 1 (global Toploop!)) "fst'" fst'/324))
val fst' :
  'a @ [< 'm & global] -> ('b @ 'n -> 'a @ [< global > 'm]) @ [< global] =
  <fun>
|}]

(* if explicitly annotated, the returned function is local [function[L]],
  the inner [x] is returned locally *)
let fst_local (x @ local) = exclave_ fun y -> x
[%%expect{|
(let
  (fst_local/328 =
     (function {nlocal = 1} x/330[L]? : local
       (function[L] {nlocal = 1} y/331[L]? : local x/330)))
  (apply (field_imm 1 (global Toploop!)) "fst_local" fst_local/328))
val fst_local :
  'a @ [< 'm > local] -> ('b @ 'n -> 'a @ [> 'm | local]) @ [> local] = <fun>
|}]

let foo = fst 42
[%%expect{|
(let
  (fst/321 =? (apply (field_imm 0 (global Toploop!)) "fst")
   foo/332 = (apply fst/321 42))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/332))
val foo : '_weak1 -> int @ [< global > aliased] = <fun>
|}]

let foo () =
  exclave_ (fst_local 42)
[%%expect{|
(let
  (fst_local/328 =? (apply (field_imm 0 (global Toploop!)) "fst_local")
   foo/333 =
     (function {nlocal = 1} param/334[L][value<int>] : local
       (apply[L] fst_local/328 42)))
  (apply (field_imm 1 (global Toploop!)) "foo" foo/333))
val foo : unit @ 'n -> ('a @ 'm -> int @ [> local]) @ [> local] = <fun>
|}]
