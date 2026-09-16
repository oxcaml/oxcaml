(* TEST
 {
   expect;
 }{
   flags = "-no-ikinds";
   expect;
 }
*)

(* Tests for the mode crossing of box kinds.

   [k box] crosses like [mutable_data with (type : k)]. *)

(**** [immediate box] crosses like [mutable_data] ****)

module M : sig
  type t : immediate box mod many forkable stateless unyielding
end = struct
  type t : immediate box
end
[%%expect{|
module M : sig type t : immediate box end
|}]

module M : sig
  type t : immediate box
end = struct
  type t : immediate box mod many forkable stateless unyielding
end
[%%expect{|
module M : sig type t : immediate box end
|}]

module Not_mod_contended : sig
  type t : immediate box mod contended
end = struct
  type t : immediate box
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : immediate box
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : immediate box end
       is not included in
         sig type t : immediate box mod contended end
       Type declarations do not match:
         type t : immediate box
       is not included in
         type t : immediate box mod contended
       The kind of the first is immediate box
         because of the definition of t at line 4, characters 2-24.
       But the kind of the first must be a subkind of
           immediate box mod contended
         because of the definition of t at line 2, characters 2-38.
|}]

module Not_mod_external : sig
  type t : immediate box mod external_
end = struct
  type t : immediate box
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t : immediate box
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : immediate box end
       is not included in
         sig type t : immediate box mod external_ end
       Type declarations do not match:
         type t : immediate box
       is not included in
         type t : immediate box mod external_
       The kind of the first is immediate box
         because of the definition of t at line 4, characters 2-24.
       But the kind of the first must be a subkind of
           immediate box mod external_
         because of the definition of t at line 2, characters 2-38.
|}]

(**** Mod bounds beneath a box ****)

module M : sig
  type t : value box mod portable
end = struct
  type t : (value mod portable) box
end
[%%expect{|
module M : sig type t : value box mod portable end
|}]

module M : sig
  type t : (value mod portable) box
end = struct
  type t : value box mod portable
end
[%%expect{|
module M : sig type t : value box mod portable end
|}]

module M : sig
  type t : bits64 box mod many forkable stateless unyielding
end = struct
  type t : (bits64 mod everything) box
end
[%%expect{|
module M : sig type t : (bits64 mod everything) box end
|}]

module M : sig
  type t : (bits64 mod everything) box
end = struct
  type t : bits64 box mod many forkable stateless unyielding
end
[%%expect{|
module M : sig type t : (bits64 mod everything) box end
|}]

(**** Values of box kinds cross accordingly ****)

type t : immediate box
let cross (x : t @ nonportable) : _ @ portable = x
[%%expect{|
type t : immediate box
val cross : t -> t @ portable = <fun>
|}]

type u : value box
let no_cross (x : u @ nonportable) : _ @ portable = x
[%%expect{|
type u : value box
Line 2, characters 52-53:
2 | let no_cross (x : u @ nonportable) : _ @ portable = x
                                                        ^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(**** With-bounds beneath a [box] ****)

type 'a t : (immutable_data with 'a) box
[%%expect{|
type 'a t : mutable_data box with 'a
|}]
