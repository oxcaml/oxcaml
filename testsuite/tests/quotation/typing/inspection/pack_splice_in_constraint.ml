(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

module type S = sig
  type t
  val x : t
  val i : int
end
#mark_toplevel_in_quotations;;

(* Obviously fine, nothing is spliced *)
let _ = <[ fun (module M : S with type t = int) -> M.x ]>
[%%expect {|
module type S = sig type t val x : t val i : int end
- : <[(module S with type t = int) -> int]> expr =
<[
  fun (((module M) : (module S with type t = int)) : (module
    S with type t = int)) -> M.x
]>
|}]

(* Not fine -- there's a wildcard, which is a type variable under the hood,
   and unpacking needs to have package constraints with closed types *)
let _ = <[ fun (module M : S with type t = _) -> M.x ]>
[%%expect {|
Line 1, characters 23-24:
1 | let _ = <[ fun (module M : S with type t = _) -> M.x ]>
                           ^
Error: The type of this packed module contains variables:
       "(module S with type t = 'a)"
|}]

(* Since we erase [$int] into [_] under quotes,
   the below should fail with an error like the above to be consistent. *)
(* CR metaprogramming jbachurski: This should fail gracefully.
   See ticket 7081. *)
let _ = <[ fun (module M : S with type t = $int) -> M.x ]>
[%%expect {|
>> Fatal error: Translquote [at line 1, characters 23-24]:
Type variables cannot be spliced in type annotations inserted in quotations
for higher-rank or package types.
Uncaught exception: Misc.Fatal_error

|}]

let _ = <[ <[ fun (module M : S with type t = $int) -> M.x ]> ]>
[%%expect {|
>> Fatal error: Translquote [at line 1, characters 26-27]:
Type variables cannot be spliced in type annotations inserted in quotations
for higher-rank or package types.
Uncaught exception: Misc.Fatal_error

|}]
