(* TEST
 flags += " -dsource ";
 expect;
*)

(* Doc comments should attach to abstract-kind declarations the same way
   they attach to type/value declarations. *)

module type S1 = sig
  (** Doc for k *)
  kind_ k
end;;

[%%expect {|

module type S1  = sig kind_ k[@@ocaml.doc " Doc for k "] end;;
module type S1 = sig kind_ k end
|}]

module type S2 = sig
  (** Doc for k *)
  kind_ k = value
end;;

[%%expect {|

module type S2  = sig kind_ k = value[@@ocaml.doc " Doc for k "] end;;
module type S2 = sig kind_ k = value end
|}]

module type S3 = sig
  kind_ k
  (** Doc for k *)
end;;

[%%expect {|

module type S3  = sig kind_ k[@@ocaml.doc " Doc for k "] end;;
module type S3 = sig kind_ k end
|}]

module type S4 = sig
  kind_ k = value
  (** Doc for k *)
end;;

[%%expect {|

module type S4  = sig kind_ k = value[@@ocaml.doc " Doc for k "] end;;
module type S4 = sig kind_ k = value end
|}]

module type S5 = sig
  (** Doc for k *)
  kind_ k [@@deprecated "use k' instead"]
end;;

[%%expect {|

module type S5  =
  sig kind_ k[@@ocaml.doc " Doc for k "][@@deprecated "use k' instead"] end;;
module type S5 = sig kind_ k end
|}]

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

module type S7 = sig
  kind_ k [@@deprecated "use k' instead"]
  (** Doc for k *)
end;;

[%%expect {|

module type S7  =
  sig kind_ k[@@deprecated "use k' instead"][@@ocaml.doc " Doc for k "] end;;
module type S7 = sig kind_ k end
|}]

module type S8 = sig
  kind_ k = value [@@deprecated "use k' instead"]
  (** Doc for k *)
end;;

[%%expect {|

module type S8  =
  sig
    kind_ k = value[@@deprecated "use k' instead"][@@ocaml.doc " Doc for k "]
  end;;
module type S8 = sig kind_ k = value end
|}]

module type S9 = sig
  (** Doc for k *)
  kind_ [@deprecated "use k' instead"] k
end;;

[%%expect {|

module type S9  =
  sig kind_ k[@@ocaml.doc " Doc for k "][@@deprecated "use k' instead"] end;;
module type S9 = sig kind_ k end
|}]

module type S10 = sig
  (** Doc for k *)
  kind_[@deprecated "use k' instead"] k = value
end;;

[%%expect {|

module type S10  =
  sig
    kind_ k = value[@@ocaml.doc " Doc for k "][@@deprecated "use k' instead"]
  end;;
module type S10 = sig kind_ k = value end
|}]

module type S11 = sig
  kind_[@deprecated "use k' instead"] k
  (** Doc for k *)
end;;

[%%expect {|

module type S11  =
  sig kind_ k[@@deprecated "use k' instead"][@@ocaml.doc " Doc for k "] end;;
module type S11 = sig kind_ k end
|}]

module type S12 = sig
  kind_[@deprecated "use k' instead"] k = value
  (** Doc for k *)
end;;

[%%expect {|

module type S12  =
  sig
    kind_ k = value[@@deprecated "use k' instead"][@@ocaml.doc " Doc for k "]
  end;;
module type S12 = sig kind_ k = value end
|}]
