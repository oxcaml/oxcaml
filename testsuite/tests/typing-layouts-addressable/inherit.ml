(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

type ('a : bits8) req8
type ('a : bits8 addressable) req8a
[%%expect{|
type ('a : bits8) req8
type ('a : bits8 addressable) req8a
|}]

(**** [inherit] opts a lone field out of being made addressable ****)

type t : bits8 = #{ inherit i : int8# }
[%%expect{|
type t = #{ inherit i : int8#; }
|}]

type t : bits8 addressable = #{ inherit i : int8# }
[%%expect{|
Line 1, characters 0-51:
1 | type t : bits8 addressable = #{ inherit i : int8# }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t" is bits8
         because it is an unboxed record.
       But the layout of type "t" must be a sublayout of bits8 addressable
         because of the annotation on the declaration of the type t.
|}]

type t : float64 = { inherit f : float# } [@@unboxed]
[%%expect{|
type t = { inherit f : float#; } [@@unboxed]
|}]

type t : bits32 = A of inherit int32_u [@@unboxed]
[%%expect{|
type t = A of inherit int32_u [@@unboxed]
|}]

type t : bits32 = A of { inherit i : int32_u } [@@unboxed]
[%%expect{|
type t = A of { inherit i : int32_u; } [@@unboxed]
|}]

type ('a : any) t = #{ inherit a : 'a }
type ok = int8# t req8
[%%expect{|
type ('a : any) t = #{ inherit a : 'a; }
type ok = int8# t req8
|}]

let f (x : 'b t) : 'b t req8 option = None
[%%expect{|
val f : ('b : bits8). 'b t -> 'b t req8 option = <fun>
|}]

(* [inherit] on an already-addressable field changes nothing *)
type t : bits64 = #{ inherit i : int64_u }
[%%expect{|
type t = #{ inherit i : int64_u; }
|}]

(* [inherit] is only supported where the field is alone in an unboxed record
   or an [@@unboxed] type *)
type t = { inherit i : int8# }
[%%expect{|
Line 1, characters 11-28:
1 | type t = { inherit i : int8# }
               ^^^^^^^^^^^^^^^^^
Error: inherit is only supported on the sole field or argument of
       an unboxed record or of an [@@unboxed] type
|}]

type t = #{ inherit i : int8#; j : int16# }
[%%expect{|
Line 1, characters 12-30:
1 | type t = #{ inherit i : int8#; j : int16# }
                ^^^^^^^^^^^^^^^^^^
Error: inherit is only supported on the sole field or argument of
       an unboxed record or of an [@@unboxed] type
|}]

type t = #{ i : int8#; inherit j : int16# }
[%%expect{|
Line 1, characters 23-41:
1 | type t = #{ i : int8#; inherit j : int16# }
                           ^^^^^^^^^^^^^^^^^^
Error: inherit is only supported on the sole field or argument of
       an unboxed record or of an [@@unboxed] type
|}]

type t = A of inherit int32_u
[%%expect{|
Line 1, characters 14-29:
1 | type t = A of inherit int32_u
                  ^^^^^^^^^^^^^^^
Error: inherit is only supported on the sole field or argument of
       an unboxed record or of an [@@unboxed] type
|}]

type t = A of inherit int32_u | B
[%%expect{|
Line 1, characters 14-29:
1 | type t = A of inherit int32_u | B
                  ^^^^^^^^^^^^^^^
Error: inherit is only supported on the sole field or argument of
       an unboxed record or of an [@@unboxed] type
|}]

type t = A of { inherit i : int32_u }
[%%expect{|
Line 1, characters 16-35:
1 | type t = A of { inherit i : int32_u }
                    ^^^^^^^^^^^^^^^^^^^
Error: inherit is only supported on the sole field or argument of
       an unboxed record or of an [@@unboxed] type
|}]

type t = A of inherit int32_u * string [@@unboxed]
[%%expect{|
Line 1, characters 0-50:
1 | type t = A of inherit int32_u * string [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       its constructor has more than one argument.
|}]

(* Printing *)
module M = struct
  type t = #{ inherit i : int8# }
  type u = { inherit f : float# } [@@unboxed]
  type v = A of inherit int32_u [@@unboxed]
  type w = A of { inherit i : int32_u } [@@unboxed]
end
[%%expect{|
module M :
  sig
    type t = #{ inherit i : int8#; }
    type u = { inherit f : float#; } [@@unboxed]
    type v = A of inherit int32_u [@@unboxed]
    type w = A of { inherit i : int32_u; } [@@unboxed]
  end
|}]

(* Inclusion requires [inherit] to match *)
module M : sig
  type t = #{ inherit i : int8# }
end = struct
  type t = #{ i : int8# }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ i : int8# }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ i : int8#; } end
       is not included in
         sig type t = #{ inherit i : int8#; } end
       Type declarations do not match:
         type t = #{ i : int8#; }
       is not included in
         type t = #{ inherit i : int8#; }
       Fields do not match:
         "i : int8#;"
       is not the same as:
         "inherit i : int8#;"
       The second is inherit and the first is not.
|}]

module M : sig
  type t = #{ i : int8# }
end = struct
  type t = #{ inherit i : int8# }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = #{ inherit i : int8# }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = #{ inherit i : int8#; } end
       is not included in
         sig type t = #{ i : int8#; } end
       Type declarations do not match:
         type t = #{ inherit i : int8#; }
       is not included in
         type t = #{ i : int8#; }
       Fields do not match:
         "inherit i : int8#;"
       is not the same as:
         "i : int8#;"
       The first is inherit and the second is not.
|}]

module M : sig
  type t = A of inherit int32_u [@@unboxed]
end = struct
  type t = A of int32_u [@@unboxed]
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of int32_u [@@unboxed]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of int32_u [@@unboxed] end
       is not included in
         sig type t = A of inherit int32_u [@@unboxed] end
       Type declarations do not match:
         type t = A of int32_u [@@unboxed]
       is not included in
         type t = A of inherit int32_u [@@unboxed]
       Constructors do not match:
         "A of int32_u"
       is not the same as:
         "A of inherit int32_u"
       Argument 1 of the second is inherit and that of the first is not.
|}]

module M : sig
  type t = A of int32_u [@@unboxed]
end = struct
  type t = A of inherit int32_u [@@unboxed]
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = A of inherit int32_u [@@unboxed]
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = A of inherit int32_u [@@unboxed] end
       is not included in
         sig type t = A of int32_u [@@unboxed] end
       Type declarations do not match:
         type t = A of inherit int32_u [@@unboxed]
       is not included in
         type t = A of int32_u [@@unboxed]
       Constructors do not match:
         "A of inherit int32_u"
       is not the same as:
         "A of int32_u"
       Argument 1 of the first is inherit and that of the second is not.
|}]
