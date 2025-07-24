(* TEST_BELOW
   Filler_text_added_
   to_preserve_locations_while_tran
   slating_from_old_syntax__Filler_
   text_added_to_pre
   serve_locations_while_translati
*)
(* Expressions *)
let () =
  let%foo[@foo] x = 3 and[@foo] y = 4 in
  [%foo
    (let module M = M in
    ())
    [@foo]];
  [%foo
    (let open M in
    ()) [@foo]];
  [%foo fun [@foo] x -> ()];
  [%foo function[@foo] x -> ()];
  [%foo try[@foo] () with _ -> ()];
  [%foo if [@foo] () then () else ()];
  [%foo
    while () do
      ()
    done
    [@foo]];
  [%foo
    for x = () to () do
      ()
    done
    [@foo]];
  ();%foo
  ();
  [%foo assert true [@foo]];
  [%foo lazy x [@foo]];
  [%foo object end [@foo]];
  [%foo
    begin
      3
    end
    [@foo]];
  [%foo new x [@foo]];
  [%foo
    match[@foo] () with
    | [%foo? (* Pattern expressions *)
        ((lazy x) [@foo])] -> ()
    | [%foo? ((exception x) [@foo])] -> ()]

(* Class expressions *)
class x =
  fun [@foo] x ->
    let[@foo] x = 3 in
    object
      inherit x [@@foo]

      val x = 3 [@@foo]

      val virtual x : t [@@foo]

      val! mutable x = 3 [@@foo]

      method x = 3 [@@foo]

      method virtual x : t [@@foo]

      method! private x = 3 [@@foo]

      initializer x [@@foo]
    end
    [@foo]

(* Class type expressions *)
class type t =
  object
    inherit t [@@foo]

    val x : t [@@foo]

    val mutable x : t [@@foo]

    method x : t [@@foo]

    method private x : t [@@foo]

    constraint t = t' [@@foo]
  end[@foo]

(* Type expressions *)
type t = [%foo: ((module M)[@foo])]

(* Module expressions *)
module M = (functor [@foo] (M : S) -> (val x) [@foo] (struct end [@foo]))

(* Module type expression *)
module type S = functor [@foo]
  (M : S)
  -> functor
  (_ : (module type of M) [@foo])
  -> sig end [@foo]

(* Structure items *)
let%foo[@foo] x = 4

and[@foo] y = x

type%foo t = int [@@foo]

and t = int [@@foo]

type%foo t += T [@@foo]

class%foo x = x [@@foo]

class type%foo x = x [@@foo]

external%foo x : _ = "" [@@foo]

exception%foo X [@foo]

module%foo M = M [@@foo]

module%foo rec M : S = M [@@foo]
and M : S = M [@@foo]

module type%foo S = S [@@foo]

include%foo M [@@foo]

open%foo M [@@foo]

(* Signature items *)
module type S = sig
  val%foo x : t [@@foo]

  external%foo x : t = "" [@@foo]

  type%foo t = int [@@foo]

  and t' = int [@@foo]

  type%foo t += T [@@foo]

  exception%foo X [@foo]

  module%foo M : S [@@foo]

  module%foo rec M : S [@@foo]
  and M : S [@@foo]

  module%foo M = M [@@foo]

  module type%foo S = S [@@foo]

  include%foo M [@@foo]

  open%foo M [@@foo]

  class%foo x : t [@@foo]

  class type%foo x = x [@@foo]
end

(* TEST
   flags = "-dparsetree";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   check-ocamlc.byte-output;
*)
