(* TEST
 flags = " -w +A -strict-sequence ";
 expect;
*)


module A: sig val f: fpclass -> fpclass end =
  struct
    let f _ = FP_normal
  end;;
[%%expect {|
module A : sig val f : fpclass -> fpclass end
|}]

type fpclass = A ;;
[%%expect {|
type fpclass = A
|}]

module B: sig val f: fpclass -> fpclass end =
  struct
    let f A = FP_normal
  end
    ;;
[%%expect {|
Lines 2-4, characters 2-5:
2 | ..struct
3 |     let f A = FP_normal
4 |   end
Error: Signature mismatch:
       The value "f" does not match its expected declaration
       The type "... -> Stdlib.fpclass" is not compatible with the type
         "... -> fpclass"
       Type "Stdlib.fpclass" is not compatible with type "fpclass"
|}]
