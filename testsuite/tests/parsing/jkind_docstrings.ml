(* TEST
 flags += " -dsource ";
 expect;
*)

(* Doc comments should attach to abstract-kind declarations the same way
   they attach to type/value declarations. *)

(* Doc comment before, no manifest. *)
module type S1 = sig
  (** Doc for k *)
  kind_ k
end;;

[%%expect {|

module type S1  = sig kind_ k[@@ocaml.doc " Doc for k "] end;;
module type S1 = sig kind_ k end
|}]

(* Doc comment before, with manifest. *)
module type S2 = sig
  (** Doc for k *)
  kind_ k = value
end;;

[%%expect {|

module type S2  = sig kind_ k = value[@@ocaml.doc " Doc for k "] end;;
module type S2 = sig kind_ k = value end
|}]

(* Doc comment after, no manifest. *)
module type S3 = sig
  kind_ k
  (** Doc for k *)
end;;

[%%expect {|

module type S3  = sig kind_ k[@@ocaml.doc " Doc for k "] end;;
module type S3 = sig kind_ k end
|}]

(* Doc comment after, with manifest. *)
module type S4 = sig
  kind_ k = value
  (** Doc for k *)
end;;

[%%expect {|

module type S4  = sig kind_ k = value[@@ocaml.doc " Doc for k "] end;;
module type S4 = sig kind_ k = value end
|}]

(* Doc comment + non-doc attribute, no manifest.  The doc should still
   appear; today only the non-doc attribute survives. *)
module type S5 = sig
  (** Doc for k *)
  kind_ k [@@deprecated "use k' instead"]
end;;

[%%expect {|

module type S5  =
  sig kind_ k[@@ocaml.doc " Doc for k "][@@deprecated "use k' instead"] end;;
module type S5 = sig kind_ k end
|}]

(* Doc comment + non-doc attribute, with manifest. *)
module type S6 = sig
  (** Doc for k *)
  kind_ k = value [@@deprecated "use k' instead"]
end;;

[%%expect {|

module type S6  =
  sig
    kind_ k = value[@@ocaml.doc " Doc for k "][@@deprecated "use k' instead"]
  end;;
module type S6 = sig kind_ k = value end
|}]
