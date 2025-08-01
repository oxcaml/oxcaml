(* TEST
flags = "-extension-universe alpha ";

 expect;
*)


module type T = sig
  val fst : (?x): (int * string) or_null -> unit -> int
  val snd : (?x): (int * string) or_null -> unit -> string
end

module M : T = struct
  let fst (?x:(((y, _) : int * string) = 5, "six") : _ or_null) () = y
  let snd (?x:(((y, z) : int * string) = 5, "six") : _ or_null) () = z
end

[%%expect {|
module type T =
  sig
    val fst : (?x):(int * string) or_null -> unit -> int
    val snd : (?x):(int * string) or_null -> unit -> string
  end
module M : T @@ stateless
|}]

let v = M.fst ()

[%%expect{|
val v : int = 5
|}]


let v = M.snd ()

[%%expect{|
val v : string = "six"
|}]
