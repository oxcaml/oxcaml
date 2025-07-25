(* TEST
   expect;
*)

(* From jctis: <https://github.com/ocaml/ocaml/issues/10399> *)

module PR10399 : sig
  type t = < x : int >

  class c :
    object
      method x : int

      method y : bool
    end

  val o : t
end = struct
  type t = < x : int >

  class c =
    object
      method x = 3

      method y = true
    end

  let o = new c
end

[%%expect
{|
Lines 12-23, characters 6-3:
12 | ......struct
13 |   type t = < x : int >
14 |
15 |   class c =
16 |     object
...
20 |     end
21 |
22 |   let o = new c
23 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           type t = < x : int >
           class c : object method x : int method y : bool end
           val o : c
         end
       is not included in
         sig
           type t = < x : int >
           class c : object method x : int method y : bool end
           val o : t
         end
       Values do not match: val o : c is not included in val o : t
       The type "c" is not compatible with the type "t"
       The second object type has no method "y"
|}]
