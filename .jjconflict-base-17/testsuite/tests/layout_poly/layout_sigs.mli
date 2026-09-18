(* flags = "-extension layout_poly_alpha";
*)

(** This purpose of this file is to be used as part of a round-trip test: are
    CMIs created and read from consistently?
    The compiler codebase contains several assertions concerning the sign of
    sort variable IDs.
    The correct behaviour means that the compiler never asserts false.
*)

module type Sig_with_repr = sig
  val foo : (repr_ 'a). 'a -> 'a
end

module type Sig_with_layout_1 = sig
  val bar : layout_ l. ('a : l). 'a -> 'a
end

module type Sig_with_layout_2 = sig
  val bam : layout_ l1 l2. ('a : l1) ('b : l2). 'a -> 'b -> #('a * 'b)
end

val f : layout_ l. ('a : l). 'a -> 'a
