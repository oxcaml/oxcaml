(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

module M : sig
  type t
  val x : t
  val x_unique : unit -> t @ unique

  val save : 'a @ global -> unit
  val free : 'a @ unique -> unit
  val send : 'a @ portable -> unit
end = struct
  type t = string
  let x = "default"
  let x_unique = fun () -> "unique"

  let save _ = ()
  let free _ = ()
  let send _ = ()
end
[%%expect{|
module M :
  sig
    type t
    val x : t
    val x_unique : unit -> t @ unique
    val save : 'a -> unit
    val free : 'a @ unique -> unit
    val send : 'a @ portable -> unit
  end
|}]
#mark_toplevel_in_quotations;;

(** Splicing non-legacy expressions **)

(* We require the spliced expression to be at legacy.
   Thus, local/once/contended fail, as they are
   above legacy (global/many/uncontended).
   The other cases pass. *)

(* Local spliced expression -- should error! *)
let e = fun (x @ local) ->  <[ $x ]>
[%%expect{|
Line 9, characters 32-33:
9 | let e = fun (x @ local) ->  <[ $x ]>
                                    ^
Error: This value is "local" to the parent region
       but is expected to be "global" because it is spliced.
|}]

(* Unique spliced expression *)
let e = fun (x @ unique) -> <[ $x ]>
[%%expect{|
val e : ('a : any). 'a expr @ unique -> 'a expr = <fun>
|}]

(* Once spliced expression *)
let e = fun (x @ once) -> <[ $x ]>
[%%expect{|
val e : ('a : any). 'a expr @ once -> 'a expr = <fun>
|}]

(* Portable spliced expression *)
let e = fun (x @ portable) -> <[ $x ]>
[%%expect{|
val e : ('a : any). 'a expr @ portable -> 'a expr = <fun>
|}]

(* Contended spliced expression *)
let e = fun (x @ contended) -> <[ $x ]>
[%%expect{|
val e : ('a : any). 'a expr @ contended -> 'a expr = <fun>
|}]

(** Splicing expressions expecting a non-legacy result **)

(* The mode of what is in quotes should be a submode of what can be in quotes *)
let e = fun x -> <[ $x ]>
[%%expect{|
val e : ('a : any). 'a expr -> 'a expr = <fun>
|}]

(* Non-unique result of splicing *)
let e = fun x -> <[ M.free $x ]>
[%%expect{|
Line 1, characters 27-29:
1 | let e = fun x -> <[ M.free $x ]>
                               ^^
Error: This value is "aliased" but is expected to be "unique".
|}]

(* Non-portable result of splicing *)
let e = fun x -> <[ M.send $x ]>
[%%expect{|
Line 1, characters 27-29:
1 | let e = fun x -> <[ M.send $x ]>
                               ^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(** Quoting expressions with non-legacy results **)

(* The quoted expression's result needs to be at legacy.
   As before: if the mode is above legacy, the test fail,
   and otherwise we succeed. *)

(* Local result *)
let e = <[ let x @ local = M.x in x ]>
[%%expect {|
Line 8, characters 34-35:
8 | let e = <[ let x @ local = M.x in x ]>
                                      ^
Error: This value is "local" but is expected to be "global".
|}]

(* Unique result *)
(* top-level bindings must be [many], so we wrap the quote in a closure *)
let e () = <[ let x @ unique = M.x_unique () in x ]>
[%%expect {|
val e : unit -> <[M.t]> expr = <fun>
|}]

(* Once result *)
let e = <[ let x @ once = M.x in x ]>
[%%expect {|
Line 1, characters 33-34:
1 | let e = <[ let x @ once = M.x in x ]>
                                     ^
Error: This value is "once" but is expected to be "many".
|}]

(* Portable result *)
let e = <[ let x @ portable = M.x in x ]>
[%%expect {|
val e : <[M.t]> expr = <[let x = (M.x : _ @ portable) in x]>
|}]

(* Contended result *)
let e = <[ let x @ contended = M.x in x ]>
[%%expect {|
Line 1, characters 38-39:
1 | let e = <[ let x @ contended = M.x in x ]>
                                          ^
Error: This value is "contended" but is expected to be "uncontended".
|}]

(** Quoting expressions with non-legacy closures **)

(* All of the following examples should error.
   Quotes in them capture values at non-legacy modes in their closure,
   and should be given a mode accordingly. *)

(* CR metaprogramming jbachurski: For quote moding to be sound,
   the following should fail.
   Crossing a quotation-lock should be given a mode as a closure access
   (i.e. as if [<[ ... ]>] was [fun () -> ...]). *)

(* Local in closure *)
(* The quote <[f x]> is local, as it references x @ local -- should error! *)
let e = <[
  let x = stack_ (Some 42) in
  $(M.save <[let _ = x in ()]>; <[()]>)]>
[%%expect{|
val e : <[unit]> expr = <[let x = (stack_ (Some 42)) in ()]>
|}]

(* Unique in closure *)
(* The quote <[x]> is once, as it references x @ unique -- should error! *)
let e = <[
  let x @ unique = "abc" in
  $(let y = <[M.free x]> in
    <[$y; $y]>)]>
[%%expect{|
val e : <[unit]> expr = <[let x = ("abc" : _ @ unique) in M.free x; M.free x
  ]>
|}]

(* Once in closure *)
(* The quote <[x (); 2]> is once, as it references x @ once -- should error! *)
let e = <[
  let x @ once = fun () -> () in
  $(let y = <[x (); 2]> in
    <[$y + $y]>)]>
[%%expect{|
val e : <[int]> expr =
  <[let x = (fun () -> () : _ @ once) in (x (); 2) + (x (); 2)]>
|}]

(* Uncontended in closure *)
(* The quote <[x]> is nonportable, as it refs x @ uncontended -- should error! *)
let e = <[
  let x = ref 42 in
  $(M.send <[x := 0]>; <[!x]>)]>
[%%expect{|
Line 3, characters 11-21:
3 |   $(M.send <[x := 0]>; <[!x]>)]>
               ^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* Nonportable in closure *)
(* The quote <[f ()]> is nonportable, as it refs f @ nonportable -- should error! *)
let e = <[
  let x = ref 42 in
  let f = fun () -> x := 0 in
  $(M.send <[f ()]>; <[()]>)]>
[%%expect{|
Line 4, characters 11-19:
4 |   $(M.send <[f ()]>; <[()]>)]>
               ^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(** [expr] mode-crossing **)

(* [expr]s, like closures, do not cross comonadic modes: all of these error. *)
let cross_global (x : <[int]> expr @ local) : _ @ global = x
[%%expect{|
Line 4, characters 59-60:
4 | let cross_global (x : <[int]> expr @ local) : _ @ global = x
                                                               ^
Error: This value is "local" to the parent region but is expected to be "global".
|}]
let cross_many (x : <[int]> expr @ once) : _ @ many = x
[%%expect{|
Line 1, characters 54-55:
1 | let cross_many (x : <[int]> expr @ once) : _ @ many = x
                                                          ^
Error: This value is "once" but is expected to be "many".
|}]
let cross_portable (x : <[int]> expr @ nonportable) : _ @ portable = x
[%%expect{|
Line 1, characters 69-70:
1 | let cross_portable (x : <[int]> expr @ nonportable) : _ @ portable = x
                                                                         ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(** Quoting non-expansive expressions and using as [many] **)

let e = let x = <[42]> in <[$x + $x]>
[%%expect{|
val e : <[int]> expr = <[42 + 42]>
|}]
let e = let x = <[Some 42]> in <[Option.get $x + Option.get $x]>
[%%expect{|
val e : <[int]> expr =
  <[(Stdlib.Option.get (Some 42)) + (Stdlib.Option.get (Some 42))]>
|}]
let e = let x = <[fun () -> 2]> in <[$x () + $x ()]>
[%%expect{|
val e : <[int]> expr = <[((fun () -> 2) ()) + ((fun () -> 2) ())]>
|}]

(** Quoting expansive expressions and using as [many] **)

(* CR metaprogramming jbachurski: We'd like to give expansive expressions
   (i.e. not syntactic values) the mode [once], so these tests should fail.
   See internal ticket 5525. *)

(* Quoted expressions with side effects should be [once],
   so these tests fail. *)
let e = let x = <[ref 0]> in <[!($x) + !($x)]>
[%%expect{|
val e : <[int]> expr = <[(! (Stdlib.ref 0)) + (! (Stdlib.ref 0))]>
|}]
let e = let x = <[raise Not_found]> in <[$x + $x]>
[%%expect{|
val e : <[int]> expr = <[(Stdlib.raise Not_found) + (Stdlib.raise Not_found)
  ]>
|}]
let e = <[
  let r = ref 0 in $(
    let x = <[r := !r + 1; !r]> in
    <[ $x + $x ]> ) ]>
[%%expect{|
val e : <[int]> expr =
  <[
    let r = (Stdlib.ref 0) in
    (r := ((! r) + 1); ! r) + (r := ((! r) + 1); ! r)
  ]>
|}]

(* Quoting pure expansive expressions still fails (over-approximation). *)
let e = let x = <[1 + 1]> in <[$x + $x]>
[%%expect{|
val e : <[int]> expr = <[(1 + 1) + (1 + 1)]>
|}]

(* To use these expressions as [many], we re-build them explicitly.
   These tests should succeed. *)
let e = let x () = <[1 + 1]> in <[$(x ()) + $(x ())]>
[%%expect{|
val e : <[int]> expr = <[(1 + 1) + (1 + 1)]>
|}]
let e = <[
  let r = ref 0 in $(
    let x () = <[r := !r + 1; !r]> in
    <[ $(x ()) + $(x ()) ]> ) ]>
[%%expect{|
val e : <[int]> expr =
  <[
    let r = (Stdlib.ref 0) in
    (r := ((! r) + 1); ! r) + (r := ((! r) + 1); ! r)
  ]>
|}]
