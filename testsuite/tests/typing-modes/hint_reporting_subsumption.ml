(* TEST
   expect;
*)

(* Tests that the reported modes are the cause of a signature module mismatch, even
   when that mode is implied by another (e.g. local implies yielding). *)

module M : sig
  val with_ : f:('a -> 'a) @ local -> unit

end = struct
  let with_ ~f =
    let (use_f @ local unyielding) x =
      f x
    in
    let _ = use_f in
    ()
end
[%%expect{|
Lines 4-11, characters 6-3:
 4 | ......struct
 5 |   let with_ ~f =
 6 |     let (use_f @ local unyielding) x =
 7 |       f x
 8 |     in
 9 |     let _ = use_f in
10 |     ()
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig val with_ : f:('a -> 'b) -> unit end
       is not included in
         sig val with_ : f:('a -> 'a) @ local -> unit end
       Values do not match:
         val with_ : f:('a -> 'b) -> unit
       is not included in
         val with_ : f:('a -> 'a) @ local -> unit
       The type "f:('a -> 'a) -> unit" is not compatible with the type
         "f:('a -> 'a) @ local -> unit"
       The mode of argument f in the interface is "yielding" (line 2, characters 29-34)
       but the implementation expects it to be "unyielding".
|}]

(* Putting an unyielding annotation in the signature fixes the inclusion error *)
module M : sig
  val with_ : f:('a -> 'a) @ local unyielding -> unit

end = struct
  let with_ ~f =
    let (use_f @ local unyielding) x =
      f x
    in
    let _ = use_f in
    ()
end
[%%expect{|
module M : sig val with_ : f:('a -> 'a) @ local unyielding -> unit end
|}]

module M : sig
  val with_ : ('a -> 'a) @ local -> unit

end = struct
  let with_ f =
    let (use_f @ local unyielding) x =
      f x
    in
    let _ = use_f in
    ()
end
[%%expect{|
Lines 4-11, characters 6-3:
 4 | ......struct
 5 |   let with_ f =
 6 |     let (use_f @ local unyielding) x =
 7 |       f x
 8 |     in
 9 |     let _ = use_f in
10 |     ()
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig val with_ : ('a -> 'b) -> unit end
       is not included in
         sig val with_ : ('a -> 'a) @ local -> unit end
       Values do not match:
         val with_ : ('a -> 'b) -> unit
       is not included in
         val with_ : ('a -> 'a) @ local -> unit
       The type "('a -> 'a) -> unit" is not compatible with the type
         "('a -> 'a) @ local -> unit"
       The mode of the argument in the interface is "yielding" (line 2, characters 27-32)
       but the implementation expects it to be "unyielding".
|}]

module M : sig
  val local_ret : 'a @ local -> 'a @ local unyielding

end = struct
  let local_ret (x @ local) = exclave_ x
end
[%%expect{|
Lines 4-6, characters 6-3:
4 | ......struct
5 |   let local_ret (x @ local) = exclave_ x
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig val local_ret : 'a @ local -> 'a @ local end
       is not included in
         sig val local_ret : 'a @ local -> 'a @ local unyielding end
       Values do not match:
         val local_ret : 'a @ local -> 'a @ local
       is not included in
         val local_ret : 'a @ local -> 'a @ local unyielding
       The type "'a @ local -> 'a @ local" is not compatible with the type
         "'a @ local -> 'a @ local unyielding"
       The mode of the return in the implementation is "yielding"
       but the interface expects it to be "unyielding".
|}]
