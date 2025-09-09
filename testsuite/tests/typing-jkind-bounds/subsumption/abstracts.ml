(* TEST
    flags = "-extension layouts_alpha -ikinds";
    expect;
*)

module type S = sig
  type t : value mod portable
end

module M : S = struct
  type t = (int * int)
end
[%%expect{|
module type S = sig type t : value mod portable end
module M : S
|}]

type t1 : value

module type S = sig
  type t : immutable_data with t1
end

module M : S = struct
  type t = Foo of (int * t1)
end
[%%expect{|
type t1
module type S = sig type t : immutable_data with t1 end
module M : S
|}]


type t1 : value

module type S = sig
  type t : value mod portable with t1
end

module M : S = struct
  type t = Foo of (int * t1)
end
[%%expect {|
type t1
module type S = sig type t : value mod portable with t1 end
module M : S
|}]

type t1 : value

module type S = sig
  type t : value with t1
end

module M : S = struct
  type t = (int * t1)
end
[%%expect{|
type t1
module type S = sig type t end
module M : S
|}]


type t1 : value mod portable

module type S = sig
  type t : value mod portable with t1
end

module M : S = struct
  type t = t1
end
[%%expect{|
type t1 : value mod portable
module type S = sig type t : value mod portable with t1 end
module M : S
|}]

type t1 : value mod portable

module type S = sig
  type t : value with t1
end

module M : S = struct
  type t = t1
end
[%%expect{|
type t1 : value mod portable
module type S = sig type t end
module M : S
|}]

type t1 : value mod portable

module type S = sig
  type t : value mod portable with t1
end

module M : S = struct
  type t = (t1 * int)
end
[%%expect{|
type t1 : value mod portable
module type S = sig type t : value mod portable with t1 end
module M : S
|}]


module type S = sig
  type t : value mod portable
end

module M : S = struct
  type t = (int * int)
end
[%%expect{|
module type S = sig type t : value mod portable end
module M : S
|}]

module type S = sig
  type t : value mod portable
end

module M : S = struct
  type t : value mod portable = (int * int)
end
[%%expect{|
module type S = sig type t : value mod portable end
module M : S
|}]

module type S = sig
  type t : value mod portable
end

module M : S = struct
  type t : value mod portable = Foo of (int * int)
end
[%%expect{|
module type S = sig type t : value mod portable end
module M : S
|}]

module type S = sig
  type t : immutable_data
end

module M : S = struct
  type t = (int * int)
end
[%%expect{|
module type S = sig type t : immutable_data end
module M : S
|}]

module type S = sig
  type t : value mod portable
end

module M : S = struct
  type t = int
end
[%%expect{|
module type S = sig type t : value mod portable end
module M : S
|}]

type t : value mod portable = (int * int)
[%%expect{|
type t = int * int
|}]
