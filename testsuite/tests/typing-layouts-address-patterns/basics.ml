(* TEST
   expect;
*)

type 'a t1 : value_or_null & bits64 = 'a addr
type 'a t2 : value_or_null & bits64 = 'a addr_imm
[%%expect{|
type 'a t1 = 'a addr
type 'a t2 = 'a addr_imm
|}]

(* Variance *)

type ab = [ `A | `B ]
type a  = [ `A ];;
[%%expect{|
type ab = [ `A | `B ]
type a = [ `A ]
|}]

(* [addr] is invariant *)

let widen_addr (x : a addr) : ab addr = (x :> ab addr);;
[%%expect{|
Line 1, characters 40-54:
1 | let widen_addr (x : a addr) : ab addr = (x :> ab addr);;
                                            ^^^^^^^^^^^^^^
Error: Type "a addr" is not a subtype of "ab addr"
       The first variant type does not allow tag(s) "`B"
|}]

let narrow_addr (x : ab addr) : a addr = (x :> a addr);;
[%%expect{|
Line 1, characters 41-54:
1 | let narrow_addr (x : ab addr) : a addr = (x :> a addr);;
                                             ^^^^^^^^^^^^^
Error: Type "ab addr" is not a subtype of "a addr"
       The second variant type does not allow tag(s) "`B"
|}]

(* [addr_imm] is covariant *)

let widen_addr_imm (x : a addr_imm) : ab addr_imm = (x :> ab addr_imm);;
[%%expect{|
val widen_addr_imm : a addr_imm -> ab addr_imm = <fun>
|}]

let narrow_addr_imm (x : ab addr_imm) : a addr_imm = (x :> a addr_imm);;
[%%expect{|
Line 1, characters 53-70:
1 | let narrow_addr_imm (x : ab addr_imm) : a addr_imm = (x :> a addr_imm);;
                                                         ^^^^^^^^^^^^^^^^^
Error: Type "ab addr_imm" is not a subtype of "a addr_imm"
       Type "ab" = "[ `A | `B ]" is not a subtype of "a" = "[ `A ]"
       The second variant type does not allow tag(s) "`B"
|}]
