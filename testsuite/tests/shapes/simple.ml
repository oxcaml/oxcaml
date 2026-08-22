(* TEST
 flags = "-dshape";
 expect;
*)

let x = ()
[%%expect{|
{
 "x"[value] -> <.0>;
 }
val x : unit = ()
|}]

external y : int -> int = "%identity"
[%%expect{|
{
 "y"[value] -> <.1>;
 }
external y : int -> int = "%identity"
|}]

type t = A of foo
and foo = Bar
[%%expect{|
{
 "foo"[type] ->
   (Mutrec t/344 := Variant<.2> A<.4> of (foo/345  );
           foo/345 := Variant<.3> Bar<.5>;
    ).foo/345;
 "t"[type] ->
   (Mutrec t/344 := Variant<.2> A<.4> of (foo/345  );
           foo/345 := Variant<.3> Bar<.5>;
    ).t/344;
 }
type t = A of foo
and foo = Bar
|}]

module type S = sig
  type t
end
[%%expect{|
{
 "S"[module type] -> <.8>;
 }
module type S = sig type t end
|}]

exception E
[%%expect{|
{
 "E"[extension constructor] -> <.9>;
 }
exception E
|}]

type ext = ..
[%%expect{|
{
 "ext"[type] -> ((? ) : value)<.10>;
 }
type ext = ..
|}]

type ext += A | B
[%%expect{|
{
 "A"[extension constructor] -> <.11>;
 "B"[extension constructor] -> <.12>;
 }
type ext += A | B
|}]

module M = struct
  type ext += C
end
[%%expect{|
{
 "M"[module] -> {<.14>
                 "C"[extension constructor] -> <.13>;
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
 "M1"[module] ->
   {
    "t"[type] -> Variant<.32> C<.33> of (M2<.19> . "t"[type] );
    };
 "M2"[module] -> {
                  "t"[type] -> Variant<.34> T<.35>;
                  "x"[value] -> <.36>;
                  };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}]

class c = object end
[%%expect{|
{
 "c"[type] -> <.37>;
 "c"[class] -> <.37>;
 "c"[class type] -> <.37>;
 }
class c : object  end
|}]

class type c = object end
[%%expect{|
{
 "c"[type] -> <.40>;
 "c"[class type] -> <.40>;
 }
class type c = object  end
|}]

type u = t
[%%expect{|
{
 "u"[type] ->
   ((Mutrec t/344 := Variant<.2> A<.4> of (foo/345  );
            foo/345 := Variant<.3> Bar<.5>;
     ).t/344)<.41>;
 }
type u = t
|}]
