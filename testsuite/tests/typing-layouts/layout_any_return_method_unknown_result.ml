(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

class runner =
  object
    method run : type (a : any). (unit -> a) -> unit -> a =
      fun callback () -> callback ()
  end
[%%expect{|
class runner : object method run : ('a : any). (unit -> 'a) -> unit -> 'a end
|}]

class self_runner =
  object (self)
    inherit runner
    method run_self : type (a : any). (unit -> a) -> unit -> a =
      fun callback () -> self#run callback ()
  end
[%%expect{|
Line 5, characters 25-45:
5 |       fun callback () -> self#run callback ()
                             ^^^^^^^^^^^^^^^^^^^^
Error: This expression is in return position, so its layout must be
       representable. Only tail calls, whose result is returned directly,
       and expressions that never return normally, such as raise, are exempt.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let[@inline never] forward_method
    : type (a : any). runner -> (unit -> a) -> unit -> a =
  fun runner callback () -> runner#run callback ()
[%%expect{|
Line 3, characters 28-50:
3 |   fun runner callback () -> runner#run callback ()
                                ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is in return position, so its layout must be
       representable. Only tail calls, whose result is returned directly,
       and expressions that never return normally, such as raise, are exempt.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

let[@inline never] forward_fresh : type (a : any). (unit -> a) -> unit -> a =
  fun callback () -> (new runner)#run callback ()
[%%expect{|
Line 2, characters 21-49:
2 |   fun callback () -> (new runner)#run callback ()
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression is in return position, so its layout must be
       representable. Only tail calls, whose result is returned directly,
       and expressions that never return normally, such as raise, are exempt.
       The layout of a is any
         because of the annotation on the abstract type declaration for a.
       But the layout of a must be representable
         because we must know concretely how to return a function result.
|}]

class concrete_runner =
  object (self)
    inherit runner
    method run_self (callback : unit -> int) () : int =
      self#run callback ()
  end
[%%expect{|
class concrete_runner :
  object
    method run : ('a : any). (unit -> 'a) -> unit -> 'a
    method run_self : (unit -> int) -> unit -> int
  end
|}]

let[@inline never] forward_concrete (runner : runner) callback () : int =
  runner#run callback ()
[%%expect{|
val forward_concrete : runner -> (unit -> int) -> unit -> int = <fun>
|}]

let () =
  assert (forward_concrete (new runner) (fun () -> 42) () = 42);
  assert ((new concrete_runner)#run_self (fun () -> 43) () = 43)
[%%expect{|
|}]
