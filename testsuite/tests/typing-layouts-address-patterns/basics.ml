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

(******************************************)
(* Basic typechecking of address patterns *)

let f (addr_ x) = x
let f_imm (addr_imm_ x) = x
[%%expect{|
val f : 'a addr -> 'a = <fun>
val f_imm : 'a addr_imm -> 'a = <fun>
|}]

(* Address patterns can contain arbitrary patterns *)
let tuple (addr_ (x, y)) = x + y
let tuple_imm (addr_imm_ (x, y)) = x + y
let option = function addr_ (Some x) -> x | addr_ None -> 0
let option_imm = function addr_imm_ (Some x) -> x | addr_imm_ None -> 0
let alias (addr_ (_ as x)) = x
let alias_imm (addr_imm_ (_ as x)) = x
[%%expect{|
val tuple : (int * int) addr -> int = <fun>
val tuple_imm : (int * int) addr_imm -> int = <fun>
val option : int option addr -> int = <fun>
val option_imm : int option addr_imm -> int = <fun>
val alias : 'a addr -> 'a = <fun>
val alias_imm : 'a addr_imm -> 'a = <fun>
|}]

(* Address patterns can be nested *)
let nested (addr_ (addr_imm_ x)) = x
let nested_imm (addr_imm_ (addr_ x)) = x
[%%expect{|
val nested : 'a addr_imm addr -> 'a = <fun>
val nested_imm : 'a addr addr_imm -> 'a = <fun>
|}]

(* Or-patterns inside and around address patterns *)
let or_inside = function addr_ (`A x | `B x) -> x
let or_inside_imm = function addr_imm_ (`A x | `B x) -> x
let or_around = function addr_ (Some x) | addr_ (None as x) -> x
let or_around_imm = function addr_imm_ (Some x) | addr_imm_ (None as x) -> x
[%%expect{|
val or_inside : [< `A of 'a | `B of 'a ] addr -> 'a = <fun>
val or_inside_imm : [< `A of 'a | `B of 'a ] addr_imm -> 'a = <fun>
val or_around : 'a option option addr -> 'a option = <fun>
val or_around_imm : 'a option option addr_imm -> 'a option = <fun>
|}]

(* Type annotations inside and around address patterns *)
let annot_inside (addr_ (x : int)) = x
let annot_inside_imm (addr_imm_ (x : int)) = x
let annot_around (addr_ x : int addr) = x
let annot_around_imm (addr_imm_ x : int addr_imm) = x
[%%expect{|
val annot_inside : int addr -> int = <fun>
val annot_inside_imm : int addr_imm -> int = <fun>
val annot_around : int addr -> int = <fun>
val annot_around_imm : int addr_imm -> int = <fun>
|}]

(* Address patterns in let bindings *)
let let_bound a = let addr_ x = a in x
let let_bound_imm a = let addr_imm_ x = a in x
[%%expect{|
val let_bound : 'a addr -> 'a = <fun>
val let_bound_imm : 'a addr_imm -> 'a = <fun>
|}]

(* Address patterns under constructors, in records and in unboxed tuples *)
type 'a addr_variant = Mut of 'a addr | Imm of 'a addr_imm
type 'a addr_record = { mut : 'a addr; imm : 'a addr_imm }

let variant = function Mut addr_ x | Imm addr_imm_ x -> x
let record { mut = addr_ x; imm = addr_imm_ y } = x + y
let unboxed_tuple #(addr_ x, addr_imm_ y) = x + y
[%%expect{|
type 'a addr_variant = Mut of 'a addr | Imm of 'a addr_imm
type 'a addr_record = { mut : 'a addr; imm : 'a addr_imm; }
val variant : 'a addr_variant -> 'a = <fun>
val record : int addr_record -> int = <fun>
val unboxed_tuple : #(int addr * int addr_imm) -> int = <fun>
|}]

(******************************)
(* Constructor disambiguation *)

type t = A | B
type s = A | C
[%%expect{|
type t = A | B
type s = A | C
|}]

(* The type of the value at an address disambiguates constructors *)
let f (a : t addr) = match a with addr_ A -> 0 | addr_ B -> 1
let f_imm (a : t addr_imm) = match a with addr_imm_ A -> 0 | addr_imm_ B -> 1
[%%expect{|
val f : t addr -> int = <fun>
val f_imm : t addr_imm -> int = <fun>
|}]

(* Without an annotation, the last definition is used *)
let f = function addr_ A -> 0 | addr_ C -> 1
let f_imm = function addr_imm_ A -> 0 | addr_imm_ C -> 1
[%%expect{|
val f : s addr -> int = <fun>
val f_imm : s addr_imm -> int = <fun>
|}]

let bad (a : t addr) = match a with addr_ C -> 0 | _ -> 1
[%%expect{|
Line 1, characters 42-43:
1 | let bad (a : t addr) = match a with addr_ C -> 0 | _ -> 1
                                              ^
Error: This variant pattern is expected to have type "t"
       There is no constructor "C" within type "t"
|}]

(***************)
(* Type errors *)

(* [addr_] only matches mutable addresses and [addr_imm_] only matches immutable
   addresses *)
let bad (addr_ x : int addr_imm) = x
[%%expect{|
Line 1, characters 9-16:
1 | let bad (addr_ x : int addr_imm) = x
             ^^^^^^^
Error: This pattern matches values of type "'a addr"
       but a pattern was expected which matches values of type "int addr_imm"
|}]

let bad (addr_imm_ x : int addr) = x
[%%expect{|
Line 1, characters 9-20:
1 | let bad (addr_imm_ x : int addr) = x
             ^^^^^^^^^^^
Error: This pattern matches values of type "'a addr_imm"
       but a pattern was expected which matches values of type "int addr"
|}]

let bad = function addr_ x | addr_imm_ x -> x
[%%expect{|
Line 1, characters 29-40:
1 | let bad = function addr_ x | addr_imm_ x -> x
                                 ^^^^^^^^^^^
Error: This pattern matches values of type "'a addr_imm"
       but a pattern was expected which matches values of type "'b addr"
|}]

(* Address patterns only match addresses *)
let bad (addr_ x : int ref) = x
[%%expect{|
Line 1, characters 9-16:
1 | let bad (addr_ x : int ref) = x
             ^^^^^^^
Error: This pattern matches values of type "'a addr"
       but a pattern was expected which matches values of type "int ref"
|}]

let bad (addr_imm_ x : int ref) = x
[%%expect{|
Line 1, characters 9-20:
1 | let bad (addr_imm_ x : int ref) = x
             ^^^^^^^^^^^
Error: This pattern matches values of type "'a addr_imm"
       but a pattern was expected which matches values of type "int ref"
|}]

(* The type of the value at an address must match the inner pattern *)
let bad (addr_ (x : string) : int addr) = x
[%%expect{|
Line 1, characters 15-27:
1 | let bad (addr_ (x : string) : int addr) = x
                   ^^^^^^^^^^^^
Error: This pattern matches values of type "string"
       but a pattern was expected which matches values of type "int"
|}]

let bad (addr_imm_ (x : string) : int addr_imm) = x
[%%expect{|
Line 1, characters 19-31:
1 | let bad (addr_imm_ (x : string) : int addr_imm) = x
                       ^^^^^^^^^^^^
Error: This pattern matches values of type "string"
       but a pattern was expected which matches values of type "int"
|}]

(***********)
(* Layouts *)

(* The value at an address can have any representable layout *)
let float (addr_ (x : float#)) = x
let float_imm (addr_imm_ (x : float#)) = x
let bits64 (addr_ (x : int64_u)) = x
let bits64_imm (addr_imm_ (x : int64_u)) = x
let or_null (addr_ (x : int or_null)) = x
let or_null_imm (addr_imm_ (x : int or_null)) = x
let product (addr_ #(x, y)) = #(x + 1, y +. 1.)
let product_imm (addr_imm_ #(x, y)) = #(x + 1, y +. 1.)
[%%expect{|
val float : float# addr -> float# = <fun>
val float_imm : float# addr_imm -> float# = <fun>
val bits64 : int64_u addr -> int64_u = <fun>
val bits64_imm : int64_u addr_imm -> int64_u = <fun>
val or_null : int or_null addr -> int or_null = <fun>
val or_null_imm : int or_null addr_imm -> int or_null = <fun>
val product : #(int * float) addr -> #(int * float) = <fun>
val product_imm : #(int * float) addr_imm -> #(int * float) = <fun>
|}]

let poly : ('a : bits64). 'a addr -> 'a = fun (addr_ x) -> x
let poly_imm : ('a : bits64). 'a addr_imm -> 'a = fun (addr_imm_ x) -> x
let poly_product : ('a : value & float64). 'a addr -> 'a =
  fun (addr_ x) -> x
let poly_product_imm : ('a : value & float64). 'a addr_imm -> 'a =
  fun (addr_imm_ x) -> x
[%%expect{|
val poly : ('a : bits64). 'a addr -> 'a = <fun>
val poly_imm : ('a : bits64). 'a addr_imm -> 'a = <fun>
val poly_product : ('a : value & float64). 'a addr -> 'a = <fun>
val poly_product_imm : ('a : value & float64). 'a addr_imm -> 'a = <fun>
|}]

(* ...but not [any] *)
let bad : ('a : any). 'a addr -> unit = fun (addr_ _) -> ()
[%%expect{|
Line 1, characters 40-59:
1 | let bad : ('a : any). 'a addr -> unit = fun (addr_ _) -> ()
                                            ^^^^^^^^^^^^^^^^^^^
Error: This definition has type "'b addr -> unit" which is less general than
         "('a : any). 'a addr -> unit"
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of the value at an address
         in an address pattern.
|}]

let bad : ('a : any). 'a addr_imm -> unit = fun (addr_imm_ _) -> ()
[%%expect{|
Line 1, characters 44-67:
1 | let bad : ('a : any). 'a addr_imm -> unit = fun (addr_imm_ _) -> ()
                                                ^^^^^^^^^^^^^^^^^^^^^^^
Error: This definition has type "'b addr_imm -> unit"
       which is less general than "('a : any). 'a addr_imm -> unit"
       The layout of 'a is any
         because of the annotation on the universal variable 'a.
       But the layout of 'a must be representable
         because it's the type of the value at an address
         in an address pattern.
|}]

type t_any : any
[%%expect{|
type t_any : any
|}]

let bad (addr_ _ : t_any addr) = ()
[%%expect{|
Line 1, characters 9-16:
1 | let bad (addr_ _ : t_any addr) = ()
             ^^^^^^^
Error: This pattern matches values of type "'a addr"
       but a pattern was expected which matches values of type "t_any addr"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of the value at an address
         in an address pattern.
|}]

let bad (addr_imm_ _ : t_any addr_imm) = ()
[%%expect{|
Line 1, characters 9-20:
1 | let bad (addr_imm_ _ : t_any addr_imm) = ()
             ^^^^^^^^^^^
Error: This pattern matches values of type "'a addr_imm"
       but a pattern was expected which matches values of type "t_any addr_imm"
       The layout of t_any is any
         because of the definition of t_any at line 1, characters 0-16.
       But the layout of t_any must be representable
         because it's the type of the value at an address
         in an address pattern.
|}]

(* Sort variable inference *)
module M = struct
  let get a = match a with addr_ x -> x
  let f (a : int32_u addr) = get a
  let g (a : int64_u addr) = get a
end
[%%expect{|
Line 4, characters 33-34:
4 |   let g (a : int64_u addr) = get a
                                     ^
Error: The value "a" has type "int64_u addr"
       but an expression was expected of type "'a addr"
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a sublayout of bits32
         because of the definition of get at line 2, characters 10-39.
|}]

module M = struct
  let get a = match a with addr_imm_ x -> x
  let f (a : int32_u addr_imm) = get a
  let g (a : int64_u addr_imm) = get a
end
[%%expect{|
Line 4, characters 37-38:
4 |   let g (a : int64_u addr_imm) = get a
                                         ^
Error: The value "a" has type "int64_u addr_imm"
       but an expression was expected of type "'a addr_imm"
       The layout of int64_u is bits64
         because it is the primitive type int64_u.
       But the layout of int64_u must be a sublayout of bits32
         because of the definition of get at line 2, characters 10-43.
|}]

(***************************************)
(* Modes of immutable address patterns *)

let use_global : 'a -> unit = fun _ -> ()
let use_many : 'a -> unit = fun _ -> ()
let use_unique : 'a @ unique -> unit = fun _ -> ()
let use_portable : 'a @ portable -> unit = fun _ -> ()
let use_uncontended : 'a -> unit = fun _ -> ()
let use_unyielding : 'a @ unyielding -> unit = fun _ -> ()
[%%expect{|
val use_global : 'a -> unit = <fun>
val use_many : 'a -> unit = <fun>
val use_unique : 'a @ unique -> unit = <fun>
val use_portable : 'a @ portable -> unit = <fun>
val use_uncontended : 'a -> unit = <fun>
val use_unyielding : 'a -> unit = <fun>
|}]

(* The value at an immutable address has the mode of the address *)
let bad_local (a : string addr_imm @ local) =
  match a with
  | addr_imm_ x -> use_global x
[%%expect{|
Line 3, characters 30-31:
3 |   | addr_imm_ x -> use_global x
                                  ^
Error: This value is "local" to the parent region
         because it is the value at the address at line 3, characters 4-15
         which is "local" to the parent region.
       However, the highlighted expression is expected to be "global".
|}]

let ok_local (a : string addr_imm @ local) : string @ local =
  match a with
  | addr_imm_ x -> x
[%%expect{|
val ok_local : string addr_imm @ local -> string @ local = <fun>
|}]

let ok_portable (a : (unit -> unit) addr_imm @ portable) =
  match a with
  | addr_imm_ f -> use_portable f
[%%expect{|
val ok_portable : (unit -> unit) addr_imm @ portable -> unit = <fun>
|}]

let bad_nonportable (a : (unit -> unit) addr_imm @ nonportable) =
  match a with
  | addr_imm_ f -> use_portable f
[%%expect{|
Line 3, characters 32-33:
3 |   | addr_imm_ f -> use_portable f
                                    ^
Error: This value is "nonportable"
         because it is the value at the address at line 3, characters 4-15
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

let bad_shareable (a : (unit -> unit) addr_imm @ shareable) =
  match a with
  | addr_imm_ f -> use_portable f
[%%expect{|
Line 3, characters 32-33:
3 |   | addr_imm_ f -> use_portable f
                                    ^
Error: This value is "shareable"
         because it is the value at the address at line 3, characters 4-15
         which is "shareable".
       However, the highlighted expression is expected to be "portable".
|}]

let bad_corruptible (a : (unit -> unit) addr_imm @ corruptible) =
  match a with
  | addr_imm_ f -> use_portable f
[%%expect{|
Line 3, characters 32-33:
3 |   | addr_imm_ f -> use_portable f
                                    ^
Error: This value is "corruptible"
         because it is the value at the address at line 3, characters 4-15
         which is "corruptible".
       However, the highlighted expression is expected to be "portable".
|}]

let bad_contended (a : int ref addr_imm @ contended) =
  match a with
  | addr_imm_ r -> use_uncontended r
[%%expect{|
Line 3, characters 35-36:
3 |   | addr_imm_ r -> use_uncontended r
                                       ^
Error: This value is "contended"
         because it is the value at the address at line 3, characters 4-15
         which is "contended".
       However, the highlighted expression is expected to be "uncontended".
|}]

let bad_shared (a : int ref addr_imm @ shared) =
  match a with
  | addr_imm_ r -> use_uncontended r
[%%expect{|
Line 3, characters 35-36:
3 |   | addr_imm_ r -> use_uncontended r
                                       ^
Error: This value is "shared"
         because it is the value at the address at line 3, characters 4-15
         which is "shared".
       However, the highlighted expression is expected to be "uncontended".
|}]

let bad_corrupted (a : int ref addr_imm @ corrupted) =
  match a with
  | addr_imm_ r -> use_uncontended r
[%%expect{|
Line 3, characters 35-36:
3 |   | addr_imm_ r -> use_uncontended r
                                       ^
Error: This value is "corrupted"
         because it is the value at the address at line 3, characters 4-15
         which is "corrupted".
       However, the highlighted expression is expected to be "uncontended".
|}]

let bad_read (a : int ref addr_imm @ read) =
  match a with
  | addr_imm_ r -> r.contents <- 1
[%%expect{|
Line 3, characters 19-20:
3 |   | addr_imm_ r -> r.contents <- 1
                       ^
Error: This value is "read"
         because it is the value at the address at line 3, characters 4-15
         which is "read".
       However, the highlighted expression is expected to be "write" or "read_write"
         because its mutable field "contents" is being written.
|}]

let bad_immutable (a : int ref addr_imm @ immutable) =
  match a with
  | addr_imm_ r -> r.contents
[%%expect{|
Line 3, characters 19-20:
3 |   | addr_imm_ r -> r.contents
                       ^
Error: This value is "immutable"
         because it is the value at the address at line 3, characters 4-15
         which is "immutable".
       However, the highlighted expression is expected to be "read" or "read_write"
         because its mutable field "contents" is being read.
|}]

let bad_yielding (a : (unit -> unit) addr_imm @ yielding) =
  match a with
  | addr_imm_ f -> use_unyielding f
[%%expect{|
Line 3, characters 34-35:
3 |   | addr_imm_ f -> use_unyielding f
                                      ^
Error: This value is "yielding"
         because it is the value at the address at line 3, characters 4-15
         which is "yielding".
       However, the highlighted expression is expected to be "unyielding".
|}]

(* Once immutable addresses cannot be dereferenced *)
let bad_once (a : (unit -> unit) addr_imm @ once) =
  match a with
  | addr_imm_ f -> f ()
[%%expect{|
Line 3, characters 4-15:
3 |   | addr_imm_ f -> f ()
        ^^^^^^^^^^^
Error: This value is "once" but is expected to be "many".
|}]

let ok_many (a : (unit -> unit) addr_imm @ many) =
  match a with
  | addr_imm_ f -> use_many f
[%%expect{|
val ok_many : (unit -> unit) addr_imm -> unit = <fun>
|}]

(* The value at an immutable address is always aliased *)
let bad_unique (a : bytes addr_imm @ unique) =
  match a with
  | addr_imm_ x -> use_unique x
[%%expect{|
Line 3, characters 30-31:
3 |   | addr_imm_ x -> use_unique x
                                  ^
Error: This value is "aliased"
         because it is the value (with some modality) at the address at line 3, characters 4-15.
       However, the highlighted expression is expected to be "unique".
|}]

(* Dereferencing an immutable address does not otherwise restrict its mode *)
let ok_wildcard_local (a : int ref addr_imm @ local) =
  match a with
  | addr_imm_ _ -> ()
[%%expect{|
val ok_wildcard_local : int ref addr_imm @ local -> unit = <fun>
|}]

let ok_wildcard_contended (a : int ref addr_imm @ contended) =
  match a with
  | addr_imm_ _ -> ()
[%%expect{|
val ok_wildcard_contended : int ref addr_imm @ contended -> unit = <fun>
|}]

let ok_wildcard_immutable (a : int ref addr_imm @ immutable) =
  match a with
  | addr_imm_ _ -> ()
[%%expect{|
val ok_wildcard_immutable : int ref addr_imm @ immutable -> unit = <fun>
|}]

(*************************************)
(* Modes of mutable address patterns *)

(* Mutable addresses cannot be read at contended or corrupted modes *)
let bad_contended (a : int addr @ contended) =
  match a with
  | addr_ _ -> ()
[%%expect{|
Line 3, characters 4-11:
3 |   | addr_ _ -> ()
        ^^^^^^^
Error: This value is "contended"
       but is expected to be "shared" or "uncontended"
         because the value it points to is being read.
|}]

let bad_corrupted (a : int addr @ corrupted) =
  match a with
  | addr_ _ -> ()
[%%expect{|
Line 3, characters 4-11:
3 |   | addr_ _ -> ()
        ^^^^^^^
Error: This value is "corrupted"
       but is expected to be "shared" or "uncontended"
         because the value it points to is being read.
|}]

let ok_shared (a : int addr @ shared) =
  match a with
  | addr_ _ -> ()
[%%expect{|
val ok_shared : int addr @ shared -> unit = <fun>
|}]

(* Mutable addresses cannot be read at immutable or write visibility *)
let bad_immutable (a : int addr @ immutable) =
  match a with
  | addr_ _ -> ()
[%%expect{|
Line 3, characters 4-11:
3 |   | addr_ _ -> ()
        ^^^^^^^
Error: This value is "immutable"
       but is expected to be "read" or "read_write"
         because the value it points to is being read.
|}]

let bad_write (a : int addr @ write) =
  match a with
  | addr_ _ -> ()
[%%expect{|
Line 3, characters 4-11:
3 |   | addr_ _ -> ()
        ^^^^^^^
Error: This value is "write"
       but is expected to be "read" or "read_write"
         because the value it points to is being read.
|}]

let ok_read (a : int addr @ read) =
  match a with
  | addr_ _ -> ()
[%%expect{|
val ok_read : int addr @ read -> unit = <fun>
|}]

(* The value at a mutable address is global, many, aliased and unyielding *)
let ok_local (a : string addr @ local) =
  match a with
  | addr_ x -> use_global x
[%%expect{|
val ok_local : string addr @ local -> unit = <fun>
|}]

let ok_once (a : (unit -> unit) addr @ once) =
  match a with
  | addr_ f -> use_many f
[%%expect{|
val ok_once : (unit -> unit) addr @ once -> unit = <fun>
|}]

let bad_unique (a : bytes addr @ unique) =
  match a with
  | addr_ x -> use_unique x
[%%expect{|
Line 3, characters 26-27:
3 |   | addr_ x -> use_unique x
                              ^
Error: This value is "aliased"
         because it is the value (with some modality) at the address at line 3, characters 4-11.
       However, the highlighted expression is expected to be "unique".
|}]

let ok_yielding (a : (unit -> unit) addr @ yielding) =
  match a with
  | addr_ f -> use_unyielding f
[%%expect{|
val ok_yielding : (unit -> unit) addr @ yielding -> unit = <fun>
|}]

(* ...but otherwise has the mode of the address *)
let ok_portable (a : (unit -> unit) addr @ portable) =
  match a with
  | addr_ f -> use_portable f
[%%expect{|
val ok_portable : (unit -> unit) addr @ portable -> unit = <fun>
|}]

let bad_nonportable (a : (unit -> unit) addr @ nonportable) =
  match a with
  | addr_ f -> use_portable f
[%%expect{|
Line 3, characters 28-29:
3 |   | addr_ f -> use_portable f
                                ^
Error: This value is "nonportable"
         because it is the value at the address at line 3, characters 4-11
         which is "nonportable".
       However, the highlighted expression is expected to be "portable".
|}]

let bad_shared (a : int ref addr @ shared) =
  match a with
  | addr_ r -> use_uncontended r
[%%expect{|
Line 3, characters 31-32:
3 |   | addr_ r -> use_uncontended r
                                   ^
Error: This value is "shared"
         because it is the value at the address at line 3, characters 4-11
         which is "shared".
       However, the highlighted expression is expected to be "uncontended".
|}]

let bad_read (a : int ref addr @ read) =
  match a with
  | addr_ r -> r.contents <- 1
[%%expect{|
Line 3, characters 15-16:
3 |   | addr_ r -> r.contents <- 1
                   ^
Error: This value is "read"
         because it is the value at the address at line 3, characters 4-11
         which is "read".
       However, the highlighted expression is expected to be "write" or "read_write"
         because its mutable field "contents" is being written.
|}]

(**************)
(* Uniqueness *)

let use_unique_addr : 'a addr @ unique -> unit = fun _ -> ()
[%%expect{|
val use_unique_addr : ('a : any). 'a addr @ unique -> unit = <fun>
|}]

(* Matching on a mutable address uses the address as aliased *)
let unique_addr_after_match_mut (a : int addr @ unique) =
  match a with
  | addr_ 1 -> use_unique_addr a
  | _ -> ()
[%%expect{|
Line 3, characters 31-32:
3 |   | addr_ 1 -> use_unique_addr a
                                   ^
Error: This value is used here as unique,
       but it has already been used in an address pattern at:
Line 3, characters 4-11:
3 |   | addr_ 1 -> use_unique_addr a
        ^^^^^^^

|}]

(******************)
(* Exhaustiveness *)

let exhaustive = function addr_ true -> 0 | addr_ false -> 1
let exhaustive_imm = function addr_imm_ true -> 0 | addr_imm_ false -> 1
[%%expect{|
val exhaustive : bool addr -> int = <fun>
val exhaustive_imm : bool addr_imm -> int = <fun>
|}]

let non_exhaustive = function addr_ true -> 0
[%%expect{|
Line 1, characters 21-45:
1 | let non_exhaustive = function addr_ true -> 0
                         ^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "addr_ false"

val non_exhaustive : bool addr -> int = <fun>
|}]

let non_exhaustive_imm = function
  | addr_imm_ (Some true) -> 0
  | addr_imm_ None -> 1
[%%expect{|
Lines 1-3, characters 25-23:
1 | .........................function
2 |   | addr_imm_ (Some true) -> 0
3 |   | addr_imm_ None -> 1
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "addr_imm_ (Some false)"

val non_exhaustive_imm : bool option addr_imm -> int = <fun>
|}]

let non_exhaustive_nested = function addr_ (addr_imm_ true) -> 0
[%%expect{|
Line 1, characters 28-64:
1 | let non_exhaustive_nested = function addr_ (addr_imm_ true) -> 0
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched: "addr_ addr_imm_ false"

val non_exhaustive_nested : bool addr_imm addr -> int = <fun>
|}]

let unused = function addr_ _ -> 0 | addr_ true -> 1
[%%expect{|
Line 1, characters 37-47:
1 | let unused = function addr_ _ -> 0 | addr_ true -> 1
                                         ^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val unused : bool addr -> int = <fun>
|}]

let unused_imm = function addr_imm_ _ -> 0 | addr_imm_ true -> 1
[%%expect{|
Line 1, characters 45-59:
1 | let unused_imm = function addr_imm_ _ -> 0 | addr_imm_ true -> 1
                                                 ^^^^^^^^^^^^^^
Warning 11 [redundant-case]: this match case is unused.

val unused_imm : bool addr_imm -> int = <fun>
|}]

type _ gadt = Int : int gadt | Bool : bool gadt
[%%expect{|
type _ gadt = Int : int gadt | Bool : bool gadt
|}]

let gadt (a : int gadt addr) = match a with addr_ Int -> ()
let gadt_imm (a : int gadt addr_imm) = match a with addr_imm_ Int -> ()
[%%expect{|
val gadt : int gadt addr -> unit = <fun>
val gadt_imm : int gadt addr_imm -> unit = <fun>
|}]

(* Address patterns, unlike lazy patterns (see [typing-gadts/pr7421.ml]), are
    refutable *)
type (_, _) eq = Refl : ('a, 'a) eq
type empty = (int, unit) eq
[%%expect{|
type (_, _) eq = Refl : ('a, 'a) eq
type empty = (int, unit) eq
|}]

let refute (a : empty addr) = match a with addr_ _ -> .
let refute_imm (a : empty addr_imm) = match a with addr_imm_ _ -> .
[%%expect{|
val refute : empty addr -> 'a = <fun>
val refute_imm : empty addr_imm -> 'a = <fun>
|}]

let bad (a : empty Lazy.t) = match a with lazy _ -> .
[%%expect{|
Line 1, characters 42-48:
1 | let bad (a : empty Lazy.t) = match a with lazy _ -> .
                                              ^^^^^^
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: "lazy _"
|}]

(****************)
(* Principality *)

(* We get a principality warning when a constructor in an address pattern is
   disambiguated non-principally. *)
let f c a =
  if c then
    (match (a : t addr) with addr_ A -> 0 | _ -> 1)
  else
    (match a with addr_ A -> 0 | _ -> 1)
[%%expect{|
val f : bool -> t addr -> int = <fun>
|}, Principal{|
Line 5, characters 24-25:
5 |     (match a with addr_ A -> 0 | _ -> 1)
                            ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val f : bool -> t addr -> int = <fun>
|}]

let f_imm c a =
  if c then
    (match (a : t addr_imm) with addr_imm_ A -> 0 | _ -> 1)
  else
    (match a with addr_imm_ A -> 0 | _ -> 1)
[%%expect{|
val f_imm : bool -> t addr_imm -> int = <fun>
|}, Principal{|
Line 5, characters 28-29:
5 |     (match a with addr_imm_ A -> 0 | _ -> 1)
                                ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val f_imm : bool -> t addr_imm -> int = <fun>
|}]
