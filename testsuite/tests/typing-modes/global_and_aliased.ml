(* TEST
   expect;
*)

(* In this file, we test that [@@ global] always implies [@@ aliased]. *)

(* Types. *)

type 'a t1 : value mod global = { x1 : 'a @@ global } [@@unboxed]
type 'a t2 : value mod aliased = { x2 : 'a @@ aliased } [@@unboxed]
type 'a t3 : value mod global = { x3 : 'a @@ global aliased } [@@unboxed]
[%%expect{|
type 'a t1 = { global_ x1 : 'a; } [@@unboxed]
type 'a t2 = { x2 : 'a @@ aliased; } [@@unboxed]
type 'a t3 = { global_ x3 : 'a; } [@@unboxed]
|}]

type 'a t4 : value mod global = { x4 : 'a @@ global unique } [@@unboxed]

[%%expect{|
File "_none_", line 1:
Error: "global" modality can't be used together with "unique"
|}]

(* Constructors. *)

let mk1 (x1 : 'a) : 'a t1 @ unique = { x1 }
let mk2 (x2 : 'a @ local) : 'a t2 @ local unique = { x2 }
let mk3 (x3 : 'a) : 'a t3 @ unique = { x3 }

[%%expect{|
val mk1 : 'a -> 'a t1 @ unique = <fun>
val mk2 : local_ 'a -> 'a t2 @ local unique = <fun>
val mk3 : 'a -> 'a t3 @ unique = <fun>
|}]

let fail1 (x1 : 'a @ local) : 'a t1 = { x1 }

[%%expect{|
Line 1, characters 40-42:
1 | let fail1 (x1 : 'a @ local) : 'a t1 = { x1 }
                                            ^^
Error: This value is "local" but is expected to be "global".
|}]

let fail3 (x3 : 'a @ local) : 'a t3 = { x3 }

[%%expect{|
Line 1, characters 40-42:
1 | let fail3 (x3 : 'a @ local) : 'a t3 = { x3 }
                                            ^^
Error: This value is "local" but is expected to be "global".
|}]


(* Destructors. *)

let unmk1 ({ x1 } : 'a t1 @ local) : 'a = x1
let unmk2 ({ x2 } : 'a t2 @ local) : 'a @ local = x2
let unmk3 ({ x3 } : 'a t3 @ local) : 'a = x3

[%%expect{|
val unmk1 : local_ 'a t1 -> 'a = <fun>
val unmk2 : local_ 'a t2 -> local_ 'a = <fun>
val unmk3 : local_ 'a t3 -> 'a = <fun>
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
Error: "global" modality can't be used together with "unique"
|}]
