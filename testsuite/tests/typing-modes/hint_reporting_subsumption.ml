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
         sig val with_ : f:('a -> 'b) @ local unyielding -> unit end
       is not included in
         sig val with_ : f:('a -> 'a) @ local -> unit end
       Values do not match:
         val with_ : f:('a -> 'b) @ local unyielding -> unit
       is not included in
         val with_ : f:('a -> 'a) @ local -> unit
       The type "f:('a -> 'a) @ local unyielding -> unit"
       is not compatible with the type "f:('a -> 'a) @ local -> unit"
       The argument mode was expected to be "unyielding"
       because it is used inside the function at lines 6-7, characters 35-9
       which is expected to be "unyielding" but is "yielding"
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
         sig val with_ : ('a -> 'b) @ local unyielding -> unit end
       is not included in
         sig val with_ : ('a -> 'a) @ local -> unit end
       Values do not match:
         val with_ : ('a -> 'b) @ local unyielding -> unit
       is not included in
         val with_ : ('a -> 'a) @ local -> unit
       The type "('a -> 'a) @ local unyielding -> unit"
       is not compatible with the type "('a -> 'a) @ local -> unit"
       The argument mode was expected to be "unyielding"
       because it is used inside the function at lines 6-7, characters 35-9
       which is expected to be "unyielding" but is "yielding"
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
       The return mode was expected to be "unyielding" but is "yielding"
|}]

(* The hint chain goes through the conversion between the function's return
   mode and the mode of its body, and between the parameter's mode and its mode
   inside the function. *)
module M : sig
  val id : int ref @ local -> int ref
end = struct
  let id (x @ local) = x
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let id (x @ local) = x
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           val id :
             'a @ local forkable unyielding -> 'a @ local forkable unyielding
         end
       is not included in
         sig val id : int ref @ local -> int ref end
       Values do not match:
         val id :
           'a @ local forkable unyielding -> 'a @ local forkable unyielding
       is not included in
         val id : int ref @ local -> int ref
       The type
         "int ref @ local forkable unyielding -> int ref @ local forkable
         unyielding"
       is not compatible with the type "int ref @ local -> int ref"
       The return mode was expected to be "global" but is "local"
       because it is the result of the expression at line 4, characters 23-24
       which is "local" to the parent region
       because it is the parameter at line 4, characters 9-20 which is "local"
|}]
