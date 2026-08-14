(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "link_intf_impl.mli link_intf_impl.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -uid-deps link_intf_impl.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)

let x (* 0 *) = 42

type t (* 1 *) = int

module type S (* 4 *) = sig
  val y (* 2 *) : t
end

module M (* 6 *) : S = struct
  let y (* 5 *) = 36
end

module N (* 10 *) : sig
  val y (* 8 *) : int
end = struct
  let y (* 7 *) = 2
end

let _ = (module N : S)

module P (* 12 *)= struct
  let y (* 11 *) = 12
end

module F (* 14 *) (X (* 13 *) : S) = X

module G (* 15 *) = F(P)

module type Initial (* 20 *) = sig
  module type Nested (* 18 *) = sig
    type t (* 16 *)
  end
end

module MF (* 32 *) : sig
   module F (* 30 *) (X (* 27 *) : sig val x (* 25 *) : int end) : sig end
end = struct
  module F (* 24 *) (X (* 23 *) : sig val x (* 21 *) : int end) = struct end
end

module FMT (* 39 *) (X (* 37 *) : sig
  module type MT (* 35 *) = sig val x (* 33 *) : int end
end) : sig end = struct end
