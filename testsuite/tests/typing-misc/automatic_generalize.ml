(* TEST
 expect;
*)

(* #13688 *)
type 'e opt = 'e option constraint 'e = [> `A ]
let f: unit -> [> `A] opt = fun () -> None
let x = f ()
[%%expect{|
type 'a opt = 'a option constraint 'a = [> `A ]
val f : unit -> [> `A ] opt = <fun>
val x : [> `A ] opt = None
|}]

(* A phantom result stored in a shared cell must not prevent generalization
   of the argument type. The functor signature hides its phantom variance. *)
module type Data = sig type 'a t end
module type Make = sig
  module Make (D : Data) : sig
    type 'a t = 'a D.t
    val make : 'a -> 'a t
  end
end
[%%expect{|
module type Data = sig type 'a t end
module type Make =
  sig
    module Make :
      functor (D : Data) -> sig type 'a t = 'a D.t val make : 'a -> 'a t end
  end
|}]

module Phantom (F : Make) = struct
  module M = F.Make (struct type 'a t = unit end)
  let save_both () =
    let result = ref None in
    let save x = result := Some (M.make x) in
    save 1;
    save true;
    !result
end
[%%expect{|
Line 7, characters 9-13:
7 |     save true;
             ^^^^
Error: The constructor "true" has type "bool"
       but an expression was expected of type "int"
|}]

(* Expansion must also discover phantom parameters through named aliases. *)
module Named_phantom (F : Make) = struct
  module D = struct type 'a t = unit end
  module M = F.Make (D)
  type 'a alias = 'a M.t option
  let save_both () =
    let result = ref None in
    let save x = result := Some (Some (M.make x) : _ alias) in
    save 1;
    save true;
    !result
end
[%%expect{|
module Named_phantom :
  functor (F : Make) ->
    sig
      module D : sig type 'a t = unit end
      module M : sig type 'a t = 'a D.t val make : 'a -> 'a t end
      type 'a alias = 'a M.t option
      val save_both : unit -> 'a alias option
    end
|}]

(* Noninjective parameters of an abstract type still occur in the result. *)
module Abstract : sig type 'a t val make : unit -> 'a t end = struct
  type 'a t = unit
  let make () = ()
end
type 'a alias = 'a Abstract.t
let make_alias () =
  let result = ref None in
  result := Some (Abstract.make () : _ alias);
  result
[%%expect{|
module Abstract : sig type 'a t val make : unit -> 'a t end
type 'a alias = 'a Abstract.t
val make_alias : unit -> 'a alias option ref = <fun>
|}]

(* When the result depends on the argument, the shared cell prevents
   generalization. *)
module Dependent (F : Make) = struct
  module M = F.Make (struct type 'a t = 'a end)
  let save_both () =
    let result = ref None in
    let save x = result := Some (M.make x) in
    save 1;
    save true;
    !result
end
[%%expect{|
Line 7, characters 9-13:
7 |     save true;
             ^^^^
Error: The constructor "true" has type "bool"
       but an expression was expected of type "int"
|}]
