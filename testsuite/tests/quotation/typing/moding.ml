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
fun (x @ local) -> <[ $x ]>
[%%expect{|
- : 'a expr @ local -> 'a expr @ local once = <fun>
|}];;

(* Unique spliced expression *)
fun (x @ unique) -> <[ $x ]>
[%%expect{|
- : 'a expr @ unique -> 'a expr @ once = <fun>
|}];;

(* Once spliced expression *)
fun (x @ once) -> <[ $x ]>
[%%expect{|
- : 'a expr @ once -> 'a expr @ once = <fun>
|}];;

(* Portable spliced expression *)
fun (x @ portable) -> <[ $x ]>
[%%expect{|
- : 'a expr @ portable -> 'a expr @ once = <fun>
|}];;

(* Contended spliced expression *)
fun (x @ contended) -> <[ $x ]>
[%%expect{|
- : 'a expr @ contended -> 'a expr @ once = <fun>
|}];;

(** Splicing expressions expecting a non-legacy result **)

(* The mode of what is in quotes should be a submode of what can be in quotes *)
fun x -> <[ $x ]>
[%%expect{|
- : 'a expr -> 'a expr @ once = <fun>
|}];;

(* Non-unique result of splicing *)
fun x -> <[ M.free $x ]>
[%%expect{|
Line 1, characters 19-21:
1 | fun x -> <[ M.free $x ]>
                       ^^
Error: This value is "aliased"
         because it is a quoted expression's result and thus always at the legacy modes.
       However, the highlighted expression is expected to be "unique".
|}];;

(* Non-portable result of splicing *)
fun x -> <[ M.send $x ]>
[%%expect{|
Line 1, characters 19-21:
1 | fun x -> <[ M.send $x ]>
                       ^^
Error: This value is "nonportable"
         because it is a quoted expression's result and thus always at the legacy modes.
       However, the highlighted expression is expected to be "portable".
|}];;

(** Quoting expressions with non-legacy results **)

(* The quoted expression's result needs to be at legacy.
   As before: if the mode is above legacy, the test fail,
   and otherwise we succeed. *)

(* Local result *)
<[ let x @ local = M.x in x ]>
[%%expect {|
Line 8, characters 26-27:
8 | <[ let x @ local = M.x in x ]>
                              ^
Error: This value is "local"
       but is expected to be "global"
         because it is a quoted expression's result and thus always at the legacy modes.
|}];;

(* Unique result *)
<[ let x @ unique = M.x_unique () in x ]>
[%%expect {|
- : <[M.t]> expr = <[let x = (M.x_unique () : _ @ unique) in x]>
|}];;

(* Once result *)
<[ let x @ once = M.x in x ]>
[%%expect {|
Line 1, characters 25-26:
1 | <[ let x @ once = M.x in x ]>
                             ^
Error: This value is "once"
       but is expected to be "many"
         because it is a quoted expression's result and thus always at the legacy modes.
|}];;

(* Portable result *)
<[ let x @ portable = M.x in x ]>
[%%expect {|
- : <[M.t]> expr = <[let x = (M.x : _ @ portable) in x]>
|}];;

(* Contended result *)
<[ let x @ contended = M.x in x ]>
[%%expect {|
Line 1, characters 30-31:
1 | <[ let x @ contended = M.x in x ]>
                                  ^
Error: This value is "contended"
       but is expected to be "uncontended"
         because it is a quoted expression's result and thus always at the legacy modes.
|}];;

(** Quoting expressions with non-legacy closures **)

(* All of the following examples should error.
   Quotes in them capture values at non-legacy modes in their closure,
   and should be given a mode accordingly. *)

(* Local in closure *)
(* The quote <[f x]> is local, as it references x @ local -- should error! *)
<[let x = stack_ (Some 42) in
  $(M.save <[let _ = x in ()]>; <[()]>)]>
[%%expect{|
Line 10, characters 21-22:
10 |   $(M.save <[let _ = x in ()]>; <[()]>)]>
                          ^
Error: The value "x" is "local" because it is "stack_"-allocated.
       However, the value "x" highlighted is expected to be "global"
         because it is used inside the quoted expression at line 10, characters 11-30
         which is expected to be "global".
|}];;

(* Unique in closure *)
(* The quote <[x]> is once, as it references x @ unique -- should error! *)
<[let x @ unique = "abc" in
  $(let y = <[M.free x]> in
    <[$y; $y]>)]>
[%%expect{|
Line 3, characters 11-12:
3 |     <[$y; $y]>)]>
               ^
Error: This value is used here,
       but it is defined as once and has already been used at:
Line 3, characters 7-8:
3 |     <[$y; $y]>)]>
           ^

|}];;

(* Once in closure *)
(* The quote <[x (); 2]> is once, as it references x @ once -- should error! *)
<[let x @ once = fun () -> () in
  $(let y = <[x (); 2]> in
    <[$y + $y]>)]>
[%%expect{|
Line 3, characters 12-13:
3 |     <[$y + $y]>)]>
                ^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 3, characters 7-8:
3 |     <[$y + $y]>)]>
           ^

|}];;

(* Uncontended in closure *)
(* The quote <[x]> is nonportable, as it refs x @ uncontended -- should error! *)
<[let x = ref 42 in
  $(M.send <[x := 0]>; <[!x]>)]>
[%%expect{|
Line 2, characters 13-14:
2 |   $(M.send <[x := 0]>; <[!x]>)]>
                 ^
Error: This value is "contended"
         because it is used inside the quoted expression at line 2, characters 11-21
         which is expected to be "portable".
       However, the highlighted expression is expected to be "uncontended".
|}];;

(* Nonportable in closure *)
(* The quote <[f ()]> is nonportable, as it refs f @ nonportable -- should error! *)
<[let x = ref 42 in
  let f = fun () -> x := 0 in
  $(M.send <[f ()]>; <[()]>)]>
[%%expect{|
Line 3, characters 13-14:
3 |   $(M.send <[f ()]>; <[()]>)]>
                 ^
Error: The value "f" is "nonportable"
         because it contains a usage (of the value "x" at line 2, characters 20-21)
         which is expected to be "uncontended".
       However, the value "f" highlighted is expected to be "portable"
         because it is used inside the quoted expression at line 3, characters 11-19
         which is expected to be "portable".
|}];;

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
|}];;

(** Quoting computations (non-values) and using as [many] **)

let x = <[42]> in <[$x + $x]>
[%%expect{|
- : <[int]> expr = <[42 + 42]>
|}];;
let x = <[Some 42]> in <[Option.get $x + Option.get $x]>
[%%expect{|
- : <[int]> expr =
<[(Stdlib.Option.get (Some 42)) + (Stdlib.Option.get (Some 42))]>
|}];;
let x = <[fun () -> 2]> in <[$x () + $x ()]>
[%%expect{|
- : <[int]> expr = <[((fun () -> 2) ()) + ((fun () -> 2) ())]>
|}];;

(** Quoting value expressions and using as [many] **)

(* Quoted expressions with side effects should be [once],
   so these tests fail. *)
let x = <[ref 0]> in <[!($x) + !($x)]>
[%%expect{|
Line 5, characters 34-35:
5 | let x = <[ref 0]> in <[!($x) + !($x)]>
                                      ^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 5, characters 26-27:
5 | let x = <[ref 0]> in <[!($x) + !($x)]>
                              ^

|}];;
let x = <[raise Not_found]> in <[$x + $x]>
[%%expect{|
Line 1, characters 39-40:
1 | let x = <[raise Not_found]> in <[$x + $x]>
                                           ^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 1, characters 34-35:
1 | let x = <[raise Not_found]> in <[$x + $x]>
                                      ^

|}];;
<[
  let r = ref 0 in $(
    let x = <[r := !r + 1; !r]> in
    <[ $x + $x ]> ) ]>
[%%expect{|
Line 4, characters 13-14:
4 |     <[ $x + $x ]> ) ]>
                 ^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 4, characters 8-9:
4 |     <[ $x + $x ]> ) ]>
            ^

|}];;
(<[ref 0]> : _ @ many)
[%%expect{|
Line 1, characters 1-10:
1 | (<[ref 0]> : _ @ many)
     ^^^^^^^^^
Error: This value is "once" because it is the quote of a computation.
       However, the highlighted expression is expected to be "many".
|}];;

(* Quoting pure computations still fails. *)
let x = <[1 + 1]> in <[$x + $x]>
[%%expect{|
Line 1, characters 29-30:
1 | let x = <[1 + 1]> in <[$x + $x]>
                                 ^
Error: This value is used here,
       but it is defined as once and is also being used at:
Line 1, characters 24-25:
1 | let x = <[1 + 1]> in <[$x + $x]>
                            ^

|}];;

(* To use these expressions as [many], we re-build them explicitly.
   These tests should succeed. *)
let x () = <[1 + 1]> in <[$(x ()) + $(x ())]>
[%%expect{|
- : <[int]> expr = <[(1 + 1) + (1 + 1)]>
|}];;
<[ let r = ref 0 in $(
    let x () = <[r := !r + 1; !r]> in
    <[ $(x ()) + $(x ()) ]> ) ]>
[%%expect{|
- : <[int]> expr =
<[let r = (Stdlib.ref 0) in (r := ((! r) + 1); ! r) + (r := ((! r) + 1); ! r)
]>
|}];;
