(* TEST
 flags = "-extension runtime_metaprogramming";
 expect;
*)

#syntax quotations on

type t
module type S = sig
  type t
  val x : t
  val i : int
end
#mark_toplevel_in_quotations;;
[%%expect {|
type t
module type S = sig type t val x : t val i : int end
|}]

(* Obviously fine, nothing is spliced *)
let _ = <[ fun (module M : S with type t = t) -> M.x ]>
[%%expect {|
Uncaught exception: Stdlib.Exit

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
let _ = <[ fun (module M : S with type t = $t) -> M.x ]>
[%%expect {|
>> Fatal error: Translquote [at line 1, characters 23-24]:
Splices cannot appear in type annotations inserted in quotations
for higher-rank or package types.
Uncaught exception: Misc.Fatal_error

|}]

let _ = <[ <[ fun (module M : S with type t = $t) -> M.x ]> ]>
[%%expect {|
>> Fatal error: Translquote [at line 1, characters 26-27]:
Splices cannot appear in type annotations inserted in quotations
for higher-rank or package types.
Uncaught exception: Misc.Fatal_error

|}]
