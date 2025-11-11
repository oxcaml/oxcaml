(* TEST
   expect;
*)

(* In this file, we test that [@@ global] always implies [@@ aliased]. *)

(* Types. *)

type 'a t1 : value mod global = { x1 : 'a @@ global } [@@unboxed]
type 'a t2 : value mod aliased = { x2 : 'a @@ aliased } [@@unboxed]
type 'a t3 : value mod global = { x3 : 'a @@ global aliased } [@@unboxed]
[%%expect{|
type 'a t1 = { x1 : 'a @@ global; } [@@unboxed]
type 'a t2 = { x2 : 'a @@ aliased; } [@@unboxed]
type 'a t3 = { x3 : 'a @@ global; } [@@unboxed]
|}]

type 'a t4 : value mod global = { x4 : 'a @@ global unique } [@@unboxed]

[%%expect{|
File "_none_", line 1:
Error: The modality "global" can't be used together with "unique"
|}]

type t5 : value mod global unique = int

[%%expect{|
Line 1, characters 27-33:
1 | type t5 : value mod global unique = int
                               ^^^^^^
Error: The modifier "global" can't be used together with "unique"
|}]

(* Constructors. *)

let mk1 (x1 : 'a) : 'a t1 @ unique = { x1 }
let mk2 (x2 : 'a @ local) : 'a t2 @ local unique = { x2 }
let mk3 (x3 : 'a) : 'a t3 @ unique = { x3 }

[%%expect{|
val mk1 : 'a -> 'a t1 @ unique = <fun>
val mk2 : 'a @ local -> 'a t2 @ local unique = <fun>
val mk3 : 'a -> 'a t3 @ unique = <fun>
|}]

let fail1 (x1 : 'a @ local) : 'a t1 = { x1 }

[%%expect{|
Line 1, characters 40-42:
1 | let fail1 (x1 : 'a @ local) : 'a t1 = { x1 }
                                            ^^
Error: This value is "local" to the parent region but is expected to be "global".
|}]

let fail3 (x3 : 'a @ local) : 'a t3 = { x3 }

[%%expect{|
Line 1, characters 40-42:
1 | let fail3 (x3 : 'a @ local) : 'a t3 = { x3 }
                                            ^^
Error: This value is "local" to the parent region but is expected to be "global".
|}]


(* Destructors. *)

let unmk1 ({ x1 } : 'a t1 @ local) : 'a = x1
let unmk2 ({ x2 } : 'a t2 @ local) : 'a @ local = x2
let unmk3 ({ x3 } : 'a t3 @ local) : 'a = x3

[%%expect{|
val unmk1 : 'a t1 @ local -> 'a = <fun>
val unmk2 : 'a t2 @ local -> 'a @ local = <fun>
val unmk3 : 'a t3 @ local -> 'a = <fun>
|}]

let fail1 ({ x1 } : 'a t1 @ local unique) : 'a @ unique = x1

[%%expect{|
Line 1, characters 58-60:
1 | let fail1 ({ x1 } : 'a t1 @ local unique) : 'a @ unique = x1
                                                              ^^
Error: This value is "aliased" but is expected to be "unique".
|}]

let fail2 ({ x2 } : 'a t2 @ local unique) : 'a @ unique = x2

[%%expect{|
Line 1, characters 58-60:
1 | let fail2 ({ x2 } : 'a t2 @ local unique) : 'a @ unique = x2
                                                              ^^
Error: This value is "aliased" but is expected to be "unique".
|}]

let fail3 ({ x3 } : 'a t3 @ local unique) : 'a @ unique = x3

[%%expect{|
Line 1, characters 58-60:
1 | let fail3 ({ x3 } : 'a t3 @ local unique) : 'a @ unique = x3
                                                              ^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* Mutable fields. *)

type 'a mut1 = { mutable x1 : 'a }
type 'a mut2 = { mutable x2 : 'a @@ global }
type 'a mut3 = { mutable x3 : 'a @@ local }
type 'a mut4 = { mutable x4 : 'a @@ aliased }
type 'a mut5 = { mutable x5 : 'a @@ local unique }

[%%expect{|
type 'a mut1 = { mutable x1 : 'a; }
type 'a mut2 = { mutable x2 : 'a; }
type 'a mut3 = { mutable x3 : 'a @@ local; }
type 'a mut4 = { mutable x4 : 'a; }
type 'a mut5 = { mutable x5 : 'a @@ local unique; }
|}]

(* CR modes: better error for implicit [@@ global]? *)
type 'a mut6 = { mutable x6 : 'a @@ unique }

[%%expect{|
File "_none_", line 1:
Error: The modality "global" can't be used together with "unique"
|}]

(* [mod global] implies [mod aliased]. *)

type ('a : value mod global) u_global

type ('a : value mod aliased) u_aliased

type w_aliased : value mod aliased

type w_global : value mod global

[%%expect{|
type ('a : value mod global) u_global
type ('a : value mod aliased) u_aliased
type w_aliased : value mod aliased
type w_global : value mod global
|}]

type _fail = w_aliased u_global

[%%expect{|
Line 1, characters 13-22:
1 | type _fail = w_aliased u_global
                 ^^^^^^^^^
Error: This type "w_aliased" should be an instance of type
         "('a : value mod global)"
       The kind of w_aliased is value mod aliased
         because of the definition of w_aliased at line 5, characters 0-34.
       But the kind of w_aliased must be a subkind of value mod global
         because of the definition of u_global at line 1, characters 0-37.
|}]

type _z2 = w_global u_global

[%%expect{|
type _z2 = w_global u_global
|}]

type _z3 = w_aliased u_aliased

[%%expect{|
type _z3 = w_aliased u_aliased
|}]

type _z4 = w_global u_aliased

[%%expect{|
type _z4 = w_global u_aliased
|}]
