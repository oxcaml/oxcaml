(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-extension laws";
 module = "gen_extension_enabled.ml";
 ocamlopt.opt;
 script = "${ocamlrun} ${ocamlsrcdir}/tools/ocamllaws extract \
   -format laws-file -o gen_extension_enabled_laws.ml \
   gen_extension_enabled.cmx";
 script;
 module = "gen_extension_enabled_laws.ml";
 ocamlopt.opt;
*)

(* With the laws extension, a unit records a laws file. This test extracts the
   file and checks that it compiles.  *)

let double x = 2 * x
let scale ~by x = by * x

law_? trivial : 1 = 1
law_? labelled (x : int) : scale ~by:2 x = double x
law_? two_assumptions (x : int) (y : int) :
  x > 0 implies_ y > 0 implies_ x + y > 0

module M = struct
  type t = int list
  let empty : t = []
  law_? in_module (xs : t) : List.length xs >= List.length empty
  module N = struct
    let size (xs : t) = List.length xs
    law_? nested (xs : t) : size xs = List.length xs
  end
end

law_? qualified (xs : M.t) : M.N.size xs >= 0
law_? from_stdlib (o : int option) (r : (int, string) result) :
  Option.is_some o = Option.is_some o && Result.is_ok r = Result.is_ok r

module C : sig
  val one : int
end = struct
  let one = 1
  law_? constrained : one = 1
end

include struct
  let two = 2
  law_? included : two = 2
end

module rec R : sig
  val three : int
end = struct
  let three = 3
  law_? recursive : R.three = 3
end

open M
law_? opened (xs : t) : N.size xs >= List.length empty
