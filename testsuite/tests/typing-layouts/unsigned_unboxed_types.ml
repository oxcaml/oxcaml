(* TEST
 flags = "-extension small_numbers";
 expect;
*)

(* The unboxed unsigned integer types have the same kinds as their signed
   counterparts. *)

type t8 : bits8 mod everything = uint8_u
type t16 : bits16 mod everything = uint16_u
type t32 : bits32 mod everything = uint32_u
type t64 : bits64 mod everything = uint64_u
type tw : word mod everything = unativeint_u
[%%expect{|
type t8 = uint8_u
type t16 = uint16_u
type t32 = uint32_u
type t64 = uint64_u
type tw = unativeint_u
|}]

(* They are distinct from the signed types. *)

let f (x : int8#) : uint8_u = x
[%%expect{|
Line 1, characters 30-31:
1 | let f (x : int8#) : uint8_u = x
                                  ^
Error: The value "x" has type "int8#" but an expression was expected of type
         "uint8_u"
|}]

let f (x : int16#) : uint16_u = x
[%%expect{|
Line 1, characters 32-33:
1 | let f (x : int16#) : uint16_u = x
                                    ^
Error: The value "x" has type "int16#" but an expression was expected of type
         "uint16_u"
|}]

let f (x : int32#) : uint32_u = x
[%%expect{|
Line 1, characters 32-33:
1 | let f (x : int32#) : uint32_u = x
                                    ^
Error: The value "x" has type "int32#" but an expression was expected of type
         "uint32_u"
|}]

let f (x : int64#) : uint64_u = x
[%%expect{|
Line 1, characters 32-33:
1 | let f (x : int64#) : uint64_u = x
                                    ^
Error: The value "x" has type "int64#" but an expression was expected of type
         "uint64_u"
|}]

let f (x : nativeint#) : unativeint_u = x
[%%expect{|
Line 1, characters 40-41:
1 | let f (x : nativeint#) : unativeint_u = x
                                            ^
Error: The value "x" has type "nativeint#" but an expression was expected of type
         "unativeint_u"
|}]

(* They can be used as GADT indices and distinguished from the signed
   types. *)

type ('a : any) width =
  | I8 : int8# width
  | U8 : uint8_u width
  | I16 : int16# width
  | U16 : uint16_u width
  | I32 : int32# width
  | U32 : uint32_u width
  | I64 : int64# width
  | U64 : uint64_u width
  | IW : nativeint# width
  | UW : unativeint_u width
[%%expect{|
type ('a : any) width =
    I8 : int8# width
  | U8 : uint8_u width
  | I16 : int16# width
  | U16 : uint16_u width
  | I32 : int32# width
  | U32 : uint32_u width
  | I64 : int64# width
  | U64 : uint64_u width
  | IW : nativeint# width
  | UW : unativeint_u width
|}]

(* Specifying the index selects between the signed and unsigned
   constructors: the matching constructor alone is exhaustive. *)

let signed8 (x : int8# width) = match x with
  | I8 -> ()
[%%expect{|
val signed8 : int8# width -> unit = <fun>
|}]

let unsigned8 (x : uint8_u width) = match x with
  | U8 -> ()
[%%expect{|
val unsigned8 : uint8_u width -> unit = <fun>
|}]

let unsigned64 (x : uint64_u width) = match x with
  | U64 -> ()
[%%expect{|
val unsigned64 : uint64_u width -> unit = <fun>
|}]

let unsignedw (x : unativeint_u width) = match x with
  | UW -> ()
[%%expect{|
val unsignedw : unativeint_u width -> unit = <fun>
|}]

(* A constructor of the wrong signedness cannot match at a specified
   index. *)

let bad (x : uint8_u width) = match x with
  | I8 -> ()
[%%expect{|
Line 2, characters 4-6:
2 |   | I8 -> ()
        ^^
Error: This pattern matches values of type "int8# width"
       but a pattern was expected which matches values of type "uint8_u width"
       Type "int8#" is not compatible with type "uint8_u"
|}]

let bad (x : int16# width) = match x with
  | U16 -> ()
[%%expect{|
Line 2, characters 4-7:
2 |   | U16 -> ()
        ^^^
Error: This pattern matches values of type "uint16_u width"
       but a pattern was expected which matches values of type "int16# width"
       Type "uint16_u" is not compatible with type "int16#"
|}]

let bad (x : unativeint_u width) = match x with
  | IW -> ()
[%%expect{|
Line 2, characters 4-6:
2 |   | IW -> ()
        ^^
Error: This pattern matches values of type "nativeint# width"
       but a pattern was expected which matches values of type
         "unativeint_u width"
       Type "nativeint#" is not compatible with type "unativeint_u"
|}]
