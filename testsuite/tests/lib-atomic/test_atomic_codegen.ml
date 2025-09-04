(* TEST
   flags = "-dlambda -dno-locations -dno-unique-ids";
   expect;
*)

let a = Atomic.make 0
let _ = Atomic.get a
let _ = Atomic.set a 1
let _ = Atomic.exchange a 2 
let _ = Atomic.compare_and_set a 2 3
let _ = Atomic.compare_exchange a 3 4
let _ = Atomic.fetch_and_add a 1
let _ = Atomic.add a 1
let _ = Atomic.sub a 1
let _ = Atomic.logand a 1
let _ = Atomic.logor a 1
let _ = Atomic.logxor a 1

[%%expect{|
(let (a = (makemutable 0 (value<int>) 0))
  (apply (field_imm 1 (global Toploop!)) "a" a))
val a : int Atomic.t = {Atomic.contents = 0}
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_load_field_imm a 0))
- : int = 0
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_set_field_imm a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_exchange_field_imm a 0 2))
- : int = 1
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_compare_set_field_imm a 0 2 3))
- : bool = true
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_compare_exchange_field_imm a 0 3 4))
- : int = 3
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_fetch_add_field a 0 1))
- : int = 4
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_add_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_sub_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_land_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_lor_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_lxor_field a 0 1))
- : unit = ()
|}]

type 'a atomic = { mutable x : 'a [@atomic] }

[%%expect{|
0
type 'a atomic = { mutable x : 'a [@atomic]; }
|}]

let a = {x = 0}
let _ = a.x
let _ = a.x <- 1

[%%expect{|
(let (a = (makemutable 0 (value<int>) 0))
  (apply (field_imm 1 (global Toploop!)) "a" a))
val a : int atomic = {x = 0}
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_load_field_imm a 0))
- : int = 0
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_set_field_imm a 0 1))
- : unit = ()
|}]

let a = {x = 0}
let _ = Atomic.Loc.get [%atomic.loc a.x]
let _ = Atomic.Loc.set [%atomic.loc a.x] 1
let _ = Atomic.Loc.exchange [%atomic.loc a.x] 2 
let _ = Atomic.Loc.compare_and_set [%atomic.loc a.x] 2 3
let _ = Atomic.Loc.compare_exchange [%atomic.loc a.x] 3 4
let _ = Atomic.Loc.fetch_and_add [%atomic.loc a.x] 1
let _ = Atomic.Loc.add [%atomic.loc a.x] 1
let _ = Atomic.Loc.sub [%atomic.loc a.x] 1
let _ = Atomic.Loc.logand [%atomic.loc a.x] 1
let _ = Atomic.Loc.logor [%atomic.loc a.x] 1
let _ = Atomic.Loc.logxor [%atomic.loc a.x] 1

[%%expect{|
(let (a = (makemutable 0 (value<int>) 0))
  (apply (field_imm 1 (global Toploop!)) "a" a))
val a : int atomic = {x = 0}
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_load_field_imm a 0))
- : int = 0
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_set_field_imm a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_exchange_field_imm a 0 2))
- : int = 1
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_compare_set_field_imm a 0 2 3))
- : bool = true
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_compare_exchange_field_imm a 0 3 4))
- : int = 3
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_fetch_add_field a 0 1))
- : int = 4
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_add_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_sub_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_land_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_lor_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_lxor_field a 0 1))
- : unit = ()
|}]

external atomic_get_field : 'a atomic -> int -> 'a = "%atomic_load_field"
external atomic_set_field : 'a atomic -> int -> 'a -> unit = "%atomic_set_field"
external atomic_exchange_field : 'a atomic -> int -> 'a -> 'a = "%atomic_exchange_field"
external atomic_compare_exchange_field : 'a atomic -> int -> 'a -> 'a -> 'a = "%atomic_compare_exchange_field"
external atomic_compare_and_set_field : 'a atomic -> int -> 'a -> 'a -> bool = "%atomic_cas_field"
external atomic_fetch_and_add_field : int atomic -> int -> int -> int = "%atomic_fetch_add_field"
external atomic_add_field : int atomic -> int -> int -> unit = "%atomic_add_field"
external atomic_sub_field : int atomic -> int -> int -> unit = "%atomic_sub_field"
external atomic_logand_field : int atomic -> int -> int -> unit = "%atomic_land_field"
external atomic_logor_field : int atomic -> int -> int -> unit = "%atomic_lor_field"
external atomic_logxor_field : int atomic -> int -> int -> unit = "%atomic_lxor_field"

[%%expect{|
0
external atomic_get_field : 'a atomic -> int -> 'a = "%atomic_load_field"
0
external atomic_set_field : 'a atomic -> int -> 'a -> unit
  = "%atomic_set_field"
0
external atomic_exchange_field : 'a atomic -> int -> 'a -> 'a
  = "%atomic_exchange_field"
0
external atomic_compare_exchange_field : 'a atomic -> int -> 'a -> 'a -> 'a
  = "%atomic_compare_exchange_field"
0
external atomic_compare_and_set_field : 'a atomic -> int -> 'a -> 'a -> bool
  = "%atomic_cas_field"
0
external atomic_fetch_and_add_field : int atomic -> int -> int -> int
  = "%atomic_fetch_add_field"
0
external atomic_add_field : int atomic -> int -> int -> unit
  = "%atomic_add_field"
0
external atomic_sub_field : int atomic -> int -> int -> unit
  = "%atomic_sub_field"
0
external atomic_logand_field : int atomic -> int -> int -> unit
  = "%atomic_land_field"
0
external atomic_logor_field : int atomic -> int -> int -> unit
  = "%atomic_lor_field"
0
external atomic_logxor_field : int atomic -> int -> int -> unit
  = "%atomic_lxor_field"
|}]

let a = {x = 0}
let _ = atomic_get_field a 0
let _ = atomic_set_field a 0 1
let _ = atomic_exchange_field a 0 2 
let _ = atomic_compare_and_set_field a 0 2 3
let _ = atomic_compare_exchange_field a 0 3 4
let _ = atomic_fetch_and_add_field a 0 1
let _ = atomic_add_field a 0 1
let _ = atomic_sub_field a 0 1
let _ = atomic_logand_field a 0 1
let _ = atomic_logor_field a 0 1
let _ = atomic_logxor_field a 0 1

[%%expect{|
(let (a = (makemutable 0 (value<int>) 0))
  (apply (field_imm 1 (global Toploop!)) "a" a))
val a : int atomic = {x = 0}
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_load_field_imm a 0))
- : int = 0
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_set_field_imm a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_exchange_field_imm a 0 2))
- : int = 1
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_compare_set_field_imm a 0 2 3))
- : bool = true
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_compare_exchange_field_imm a 0 3 4))
- : int = 3
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_fetch_add_field a 0 1))
- : int = 4
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_add_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_sub_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_land_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_lor_field a 0 1))
- : unit = ()
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_lxor_field a 0 1))
- : unit = ()
|}]

(* Not specialized by the frontend, but should be specialized by flambda2.
   On amd64, the assembly should not include any calls to [caml_atomic_*]. *)

type maybe_imm = I | P of unit
[%%expect{|
0
type maybe_imm = I | P of unit
|}]

let a = Atomic.make I
let _ = Atomic.compare_and_set a I I
let _ = Atomic.compare_exchange a I I

[%%expect{|
(let
  (a = (makemutable 0 (value<(consts (0)) (non_consts ([0: value<int>]))>) 0))
  (apply (field_imm 1 (global Toploop!)) "a" a))
val a : maybe_imm Atomic.t = {Atomic.contents = I}
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_compare_set_field_ptr a 0 0 0))
- : bool = true
(let (a =? (apply (field_imm 0 (global Toploop!)) "a"))
  (atomic_compare_exchange_field_ptr a 0 0 0))
- : maybe_imm = I
|}]
