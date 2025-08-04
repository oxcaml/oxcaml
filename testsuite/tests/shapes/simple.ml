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
 "y"[value] -> <.2>;
 }
external y : int -> int = "%identity"
|}]

type t = A of foo
and foo = Bar
[%%expect{|
{
 "foo"[type] -> {<.4>
                 "Bar"[constructor] -> {<.6>};
                 };
 "t"[type] -> {<.3>
               "A"[constructor] -> {<.5>};
               };
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
 "E"[extension constructor] -> {<.9>};
 }
exception E
|}]

type ext = ..
[%%expect{|
{
 "ext"[type] -> <.10>;
 }
type ext = ..
|}]

type ext += A | B
[%%expect{|
{
 "A"[extension constructor] -> {<.11>};
 "B"[extension constructor] -> {<.12>};
 }
type ext += A | B
|}]

module M = struct
  type ext += C
end
[%%expect{|
{
 "M"[module] -> {<.14>
                 "C"[extension constructor] -> {<.13>};
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
                  "t"[type] -> {<.28>
                                "C"[constructor] -> {<.29>};
                                };
                  };
 "M2"[module] ->
   {
    "t"[type] -> {<.30>
                  "T"[constructor] -> {<.31>};
                  };
    "x"[value] -> <.32>;
    };
 }
module rec M1 : sig type t = C of M2.t end
and M2 : sig type t val x : t end
|}]

class c = object end
[%%expect{|
{
 "c"[type] -> <.34>;
 "c"[class] -> <.34>;
 "c"[class type] -> <.34>;
 }
class c : object  end
|}]

class type c = object end
[%%expect{|
{
 "c"[type] -> <.37>;
 "c"[class type] -> <.37>;
 }
class type c = object  end
|}]

type u = t
[%%expect{|
{
 "u"[type] -> <.38>;
 }
type u = t
|}]
