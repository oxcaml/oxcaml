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
 "y"[value] -> <.6>;
 }
external y : int -> int = "%identity"
|}]

type t = A of foo
and foo = Bar
[%%expect{|
{
 "foo"[type] ->
   (Mutrec t/298 := Variant A<.9> of (foo/299  );foo/299 := Variant Bar<.10>; ).foo/299;
 "t"[type] ->
   (Mutrec t/298 := Variant A<.9> of (foo/299  );foo/299 := Variant Bar<.10>; ).t/298;
 }
type t = A of foo
and foo = Bar
|}]

module type S = sig
  type t
end
[%%expect{|
{
 "S"[module type] -> <.13>;
 }
module type S = sig type t end
|}]

exception E
[%%expect{|
{
 "E"[extension constructor] -> <.14>;
 }
exception E
|}]

type ext = ..
[%%expect{|
{
 "ext"[type] -> <.15>;
 }
type ext = ..
|}]

type ext += A | B
[%%expect{|
{
 "A"[extension constructor] -> <.16>;
 "B"[extension constructor] -> <.17>;
 }
type ext += A | B
|}]

module M = struct
  type ext += C
end
[%%expect{|
{
 "M"[module] -> {<.19>
                 "C"[extension constructor] -> <.18>;
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
                  "t"[type] -> Variant C<.36> of (M2<.24> . "t"[type] );
                  };
 "M2"[module] -> {
                  "t"[type] -> Variant T<.39>;
                  "x"[value] -> <.40>;
                  };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}]

class c = object end
[%%expect{|
{
 "c"[type] -> <.41>;
 "c"[class] -> <.41>;
 "c"[class type] -> <.41>;
 }
class c : object  end
|}]

class type c = object end
[%%expect{|
{
 "c"[type] -> <.44>;
 "c"[class type] -> <.44>;
 }
class type c = object  end
|}]

type u = t
[%%expect{|
{
 "u"[type] ->
   (Mutrec t/298 := Variant A<.9> of (foo/299  );foo/299 := Variant Bar<.10>; ).t/298;
 }
type u = t
|}]
