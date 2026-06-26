(* TEST
 flags = "-dshape";
 expect;
*)

(* Everything that couldn't go anywhere else. *)

open struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{}
module M : sig type t = A end
|}]

include M
[%%expect{|
{
 "t"[type] -> Variant A<.7>;
 }
type t = M.t = A
|}]

module N = M
[%%expect{|
{
 "N"[module] -> Alias(<.9>
                      {<.8>
                       "t"[type] -> Variant A<.7>;
                       });
 }
module N = M
|}]

(* Not open structs, but the code handling the following is currently very
   similar to the one for open struct (i.e. calls [Env.enter_signature]), and
   so we are likely to encounter the same bugs, if any. *)

include struct
  module M' = struct
    type t = A
  end
end
[%%expect{|
{
 "M'"[module] -> {<.12>
                  "t"[type] -> Variant A<.11>;
                  };
 }
module M' : sig type t = A end
|}]

module N' = M'
[%%expect{|
{
 "N'"[module] -> Alias(<.13>
                       {<.12>
                        "t"[type] -> Variant A<.11>;
                        });
 }
module N' = M'
|}]

module Test = struct
  module M = struct
    type t = A
  end
end
[%%expect{|
{
 "Test"[module] ->
   {<.17>
    "M"[module] -> {<.16>
                    "t"[type] -> Variant A<.15>;
                    };
    };
 }
module Test : sig module M : sig type t = A end end
|}]

include Test
[%%expect{|
{
 "M"[module] -> {<.16>
                 "t"[type] -> Variant A<.15>;
                 };
 }
module M = Test.M
|}]

module N = M
[%%expect{|
{
 "N"[module] -> Alias(<.18>
                      {<.16>
                       "t"[type] -> Variant A<.15>;
                       });
 }
module N = M
|}]
