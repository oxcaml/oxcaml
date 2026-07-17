(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

module M : sig
  val free : 'a @ unique -> unit
  val of_local : 'a expr @ local -> <[unit]> expr
  val of_global : 'a expr @ global -> <[unit]> expr
end = struct
  let free _ = ()
  let of_local _ = <[()]>
  let of_global _ = <[()]>
end
#mark_toplevel_in_quotations
[%%expect{|
module M :
  sig
    val free : 'a @ unique -> unit
    val of_local : 'a expr @ local -> <[unit]> expr
    val of_global : 'a expr -> <[unit]> expr
  end
|}];;

(* We cannot normally quote an expression at local *)
<[ stack_ (Some 42) ]>
[%%expect{|
Line 1, characters 3-19:
1 | <[ stack_ (Some 42) ]>
       ^^^^^^^^^^^^^^^^
Error: This value is "local" because it is "stack_"-allocated.
       However, the highlighted expression is expected to be "global"
         because it is a quoted expression's result and thus always at the legacy modes.
|}];;
(* but we can delay the mode checks until generation *)
(<[ stack_ (Some 42) ]> [@magic_staged_modes])
[%%expect{|
- : <[int option]> expr = <[stack_ (Some 42)]>
|}];;

(* We cannot expect a spliced expression to be non-legacy *)
fun x -> <[ M.free $x ]>
[%%expect{|
Line 1, characters 19-21:
1 | fun x -> <[ M.free $x ]>
                       ^^
Error: This value is "aliased"
         because it is a quoted expression's result and thus always at the legacy modes.
       However, the highlighted expression is expected to be "unique".
|}];;
(* but we can delay the mode checks until generation *)
fun x -> <[ M.free ($x [@magic_staged_modes]) ]>
[%%expect{|
- : 'a expr -> <[unit]> expr @ once = <fun>
|}];;

(* We can capture a local, but not quote it as it is not legacy *)
<[ fun (x @ local) -> $(M.of_local <[ x ]>) ]>
[%%expect{|
Line 1, characters 38-39:
1 | <[ fun (x @ local) -> $(M.of_local <[ x ]>) ]>
                                          ^
Error: The value "x" is "quote_regional"
       but is expected to be "quote_global"
         because it is used inside the quoted expression at line 1, characters 35-42
         which is expected to be "quote_global".
|}];;
(* but we can delay the mode checks on [x] until generation *)
<[ fun (x @ local) -> $(M.of_local (<[ x ]> [@magic_staged_modes])) ]>
[%%expect{|
- : <[$('a) @ local -> unit]> expr = <[fun (x : _ @ local) -> ()]>
|}];;
(* including the mode checks on the capture of [x] *)
<[ fun (x @ local) -> $(M.of_global (<[ x ]> [@magic_staged_modes])) ]>
[%%expect{|
- : <[$('a) @ local -> unit]> expr = <[fun (x : _ @ local) -> ()]>
|}];;

(* The attribute is quoted appropriately *)
<[ fun x -> (<[ ($x [@magic_staged_modes]) ]> [@magic_staged_modes]) ]>
[%%expect{|
- : <[$('a) expr -> $('a) expr @ once]> expr =
<[fun x -> ((<[(($x) [@magic_staged_modes])]>) [@magic_staged_modes])]>
|}];;
