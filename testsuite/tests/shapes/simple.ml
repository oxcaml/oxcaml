(* TEST
 flags = "-dshape";
 expect;
*)

let x = ()
[%%expect{|
{
 "x"[value] -> <.5>;
 }
val x : unit = ()
|}]

external y : int -> int = "%identity"
[%%expect{|
{
 "y"[value] -> <.7>;
 }
external y : int -> int = "%identity"
|}]

type t = A of foo
and foo = Bar
[%%expect{|
{
 "foo"[type] ->
   (Mutrec t/291 := Variant A<.10> of (Constr foo/292  );
           foo/292 := Variant Bar<.11>;).foo/292;
 "t"[type] ->
   (Mutrec t/291 := Variant A<.10> of (Constr foo/292  );
           foo/292 := Variant Bar<.11>;).t/291;
 }
type t = A of foo
and foo = Bar
|}]

module type S = sig
  type t
end
[%%expect{|
{
 "S"[module type] -> <.14>;
 }
module type S = sig type t end
|}]

exception E
[%%expect{|
{
 "E"[extension constructor] -> <.15>;
 }
exception E
|}]

type ext = ..
[%%expect{|
{
 "ext"[type] -> <.16>;
 }
type ext = ..
|}]

type ext += A | B
[%%expect{|
{
 "A"[extension constructor] -> <.17>;
 "B"[extension constructor] -> <.18>;
 }
type ext += A | B
|}]

module M = struct
  type ext += C
end
[%%expect{|
{
 "M"[module] -> {<.20>
                 "C"[extension constructor] -> <.19>;
                 };
 }
module M : sig type ext += C end
|}]

module _ = struct
  type t = Should_not_appear_in_shape
end
[%%expect{|
{}
|}]

module rec M1 : sig
  type t = C of M2.t
end = struct
  type t = C of M2.t
end

and M2 : sig
  type t
  val x : t
end = struct
  type t = T
  let x = T
end
[%%expect{|
{
 "M1"[module] -> {
                  "t"[type] -> Variant C<.37> of (M2<.25> . "t"[type] );
                  };
 "M2"[module] -> {
                  "t"[type] -> Variant T<.40>;
                  "x"[value] -> <.41>;
                  };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}]

class c = object end
[%%expect{|
{
 "c"[type] -> <.43>;
 "c"[class] -> <.43>;
 "c"[class type] -> <.43>;
 }
class c : object  end
|}]

class type c = object end
[%%expect{|
{
 "c"[type] -> <.46>;
 "c"[class type] -> <.46>;
 }
class type c = object  end
|}]

type u = t
[%%expect{|
{
 "u"[type] ->
   (Mutrec t/291 := Variant A<.10> of (Constr foo/292  );
           foo/292 := Variant Bar<.11>;).t/291;
 }
type u = t
|}]
