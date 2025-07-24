(* TEST
   flags = " -w +A -strict-sequence ";
   expect;
*)

module A : sig
  val f : fpclass -> fpclass
end = struct
  let f _ = FP_normal
end

[%%expect {|
module A : sig val f : fpclass -> fpclass end
|}]

type fpclass = A

[%%expect {|
type fpclass = A
|}]

module B : sig
  val f : fpclass -> fpclass
end = struct
  let f A = FP_normal
end

[%%expect
{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f A = FP_normal
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig val f : fpclass -> Stdlib.fpclass end
       is not included in
         sig val f : fpclass -> fpclass end
       Values do not match:
         val f : fpclass -> Stdlib.fpclass
       is not included in
         val f : fpclass -> fpclass
       The type "fpclass -> Stdlib.fpclass" is not compatible with the type
         "fpclass -> fpclass"
       Type "Stdlib.fpclass" is not compatible with type "fpclass"
|}]
