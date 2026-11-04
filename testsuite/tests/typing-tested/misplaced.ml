(* TEST
 flags = "-w +A-32-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

module type S = sig
  [@@@testing_required] (* accepted *)
  [@@@testing_not_required] (* accepted *)
  val x : int [@@testing_required] (* accepted *)
  external y : int -> int = "%identity" [@@testing_not_required] (* accepted *)
  module M : sig end [@@testing_required] (* accepted *)
  module rec R : sig end [@@testing_not_required] (* accepted *)
  include sig end [@@testing_required] (* accepted *)

  type t [@@testing_required] (* rejected *)
  module type T = sig end [@@testing_not_required] (* rejected *)
  exception E [@@testing_required] (* rejected *)
  [@@@covers_required_tests_of M] (* rejected *)
end

module M = struct
  [@@@testing_required] (* rejected *)
  let x = 1 [@@testing_not_required] (* rejected *)
  module N = struct end [@@testing_required] (* rejected *)
  module O : sig val v : int [@@testing_required] (* accepted *) end = struct
    let v = 1
  end
  module _ = struct
    [@@@covers_required_tests_of O] (* accepted *)
    let v = O.v
  end
end
