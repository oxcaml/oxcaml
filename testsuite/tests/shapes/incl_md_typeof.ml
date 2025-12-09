(* TEST
 flags = "-dshape";
 expect;
*)

module Foo : sig
  module Bar : sig
  end
end = struct
  module Bar = struct
  end
end
;;
[%%expect{|
{
 "Foo"[module] -> {<.8>
                   "Bar"[module] -> {<.6>};
                   };
 }
module Foo : sig module Bar : sig end end
|}]

module type Extended = sig
  include module type of struct include Foo end
  module Bar : sig
    include module type of struct include Bar end
  end
end
;;
[%%expect{|
{
 "Extended"[module type] -> <.10>;
 }
module type Extended = sig module Bar : sig end end
|}]

module E : Extended = struct
  module Bar = struct end
end

[%%expect{|
{
 "E"[module] -> {<.12>
                 "Bar"[module] -> {<.11>};
                 };
 }
module E : Extended
|}]
