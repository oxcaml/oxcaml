(* TEST
   expect;
*)

type t = ..

module M : sig
  type t += E | F
end = struct
  type t += E | F of int
end

[%%expect
{|
type t = ..
Lines 5-7, characters 6-3:
5 | ......struct
6 |   type t += E | F of int
7 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t += E | F of int  end
       is not included in
         sig type t += E | F  end
       Extension declarations do not match:
         type t += F of int
       is not included in
         type t += F
       Constructors do not match:
         "F of int"
       is not the same as:
         "F"
       They have different arities.
|}]

module M1 : sig
  type t += A
end = struct
  type t += private A
end

[%%expect
{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t += private A
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t += private A end
       is not included in
         sig type t += A end
       Extension declarations do not match:
         type t += private A
       is not included in
         type t += A
       Private extension constructor(s) would be revealed.
|}]

module M2 : sig
  type t += A
end = struct
  type t += private A | B
end

[%%expect
{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t += private A | B
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t += private A | B  end
       is not included in
         sig type t += A end
       Extension declarations do not match:
         type t += private A
       is not included in
         type t += A
       Private extension constructor(s) would be revealed.
|}]
