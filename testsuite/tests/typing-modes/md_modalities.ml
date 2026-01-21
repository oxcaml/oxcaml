(* TEST
    flags += "-extension mode_alpha";
    expect;
*)

(* the semantics of pmd_modalities *)
module type S = sig @@ portable
    module M : sig
        val foo : 'a -> 'a
        module N : sig
            val bar : 'a -> 'a
        end
    end
end
[%%expect{|
module type S =
  sig
    module M :
      sig
        val foo : 'a -> 'a @@ portable
        module N : sig val bar : 'a -> 'a @@ portable end
      end
  end
|}]

module type S = sig
    module M : sig
        val foo : 'a -> 'a
        module N : sig
            val bar : 'a -> 'a
        end
    end @@ portable
end
[%%expect{|
module type S =
  sig
    module M :
      sig
        val foo : 'a -> 'a @@ portable
        module N : sig val bar : 'a -> 'a @@ portable end
      end
  end
|}]


module type S = sig @@ portable
    module M : sig
        val foo : 'a -> 'a
        module N : sig
            val bar : 'a -> 'a
        end
    end @@ nonportable
end
[%%expect{|
module type S =
  sig
    module M :
      sig val foo : 'a -> 'a module N : sig val bar : 'a -> 'a end end
  end
|}]

(* works for recursive modules as well *)
module type S = sig @@ portable
    module rec M : sig
        val foo : 'a -> 'a
        (* illegal to refer to N's type, so the fact that we are using a
        workaround semantics is invisible. *)
    end @@ nonportable
    and N : sig
        val bar : 'a -> 'a
    end
end
[%%expect{|
module type S =
  sig
    module rec M : sig val foo : 'a -> 'a end
    and N : sig val bar : 'a -> 'a @@ portable end
  end
|}]

(* works on Mty_ident as well *)
module type T = sig
  val foo : 'a -> 'a
end

module type S = sig @@ portable
  module M : T
end
[%%expect{|
module type T = sig val foo : 'a -> 'a end
module type S = sig module M : sig val foo : 'a -> 'a @@ portable end end
|}]

(* works for Mty_functor *)
module type S = sig @@ portable
  module M : (sig val foo : 'a -> 'a end) -> (sig val bar : 'a -> 'a end)
end
[%%expect{|
module type S =
  sig module M : sig val foo : 'a -> 'a end -> sig val bar : 'a -> 'a end end
|}]

module M : T = struct
  let (foo @ nonportable) x = x
end


(* works for Mty_alias *)
module type S = sig @@ portable
  module M' = M
end
[%%expect{|
module M : T @@ stateless nonportable
module type S = sig module M' = M end
|}]

(* works for Mty_strenthen, and type check keeps working *)
module type S = sig @@ portable
  module M' : T with M
end
[%%expect{|
module type S = sig module M' : sig val foo : 'a -> 'a @@ portable end end
|}]

module M : S = struct
  module M' = M
end
[%%expect{|
Lines 1-3, characters 15-3:
1 | ...............struct
2 |   module M' = M
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig module M' = M @@ stateless nonportable end
       is not included in
         S
       In module "M'":
       Modules do not match:
         sig val foo : 'a -> 'a end @ nonportable
       is not included in
         sig val foo : 'a -> 'a @@ portable end @ nonportable
       In module "M'":
       Values do not match:
         val foo : 'a -> 'a (* in a structure at nonportable *)
       is not included in
         val foo : 'a -> 'a @@ portable (* in a structure at nonportable *)
       The left-hand side is "nonportable"
       but the right-hand side is "portable".
|}]

(* nested default modalities on different axes *)
module type S = sig @@ portable
    val bar : 'a -> 'a
    module M : sig @@ contended
        val foo : 'a -> 'a
    end
end
[%%expect{|
module type S =
  sig
    val bar : 'a -> 'a @@ portable
    module M : sig val foo : 'a -> 'a @@ portable contended end
  end
|}]

(* nested default modalities on the same axis *)
module type S = sig @@ portable
    val bar : 'a -> 'a
    module M : sig @@ nonportable
        val foo : 'a -> 'a
    end
end
[%%expect{|
module type S =
  sig
    val bar : 'a -> 'a @@ portable
    module M : sig val foo : 'a -> 'a @@ portable end
  end
|}]

(* CR modes: fix nested signatures overriding. Internal ticket 6301. *)
(* two nested signatures with defaults + val overriding outer default *)
module type S = sig @@ portable
    val outer : 'a -> 'a
    module M : sig @@ contended
        val inner : 'a -> 'a
        val inner_override : 'a -> 'a @@ nonportable
    end
end
[%%expect{|
module type S =
  sig
    val outer : 'a -> 'a @@ portable
    module M :
      sig
        val inner : 'a -> 'a @@ portable contended
        val inner_override : 'a -> 'a @@ portable contended
      end
  end
|}]

(* two nested signatures with defaults + val overriding inner default *)
module type S = sig @@ portable
    val outer : 'a -> 'a
    module M : sig @@ contended
        val inner : 'a -> 'a
        val inner_override : 'a -> 'a @@ uncontended
    end
end
[%%expect{|
module type S =
  sig
    val outer : 'a -> 'a @@ portable
    module M :
      sig
        val inner : 'a -> 'a @@ portable contended
        val inner_override : 'a -> 'a @@ portable
      end
  end
|}]

(* default modality on one axis + explicit modality on val on another axis *)
module type S = sig @@ portable
    val foo : 'a -> 'a @@ contended
    val bar : 'a -> 'a
end
[%%expect{|
module type S =
  sig
    val foo : 'a -> 'a @@ portable contended
    val bar : 'a -> 'a @@ portable
  end
|}]

(* default modality on one axis + explicit modality on val on the same axis *)
module type S = sig @@ portable
    val foo : 'a -> 'a @@ nonportable
    val bar : 'a -> 'a
end
[%%expect{|
module type S = sig val foo : 'a -> 'a val bar : 'a -> 'a @@ portable end
|}]

(* Testing implications. *)

(* default global, val local *)
module type S = sig @@ global
    val foo : 'a -> 'a @@ local
    val bar : 'a -> 'a
end
[%%expect{|
module type S =
  sig val foo : 'a -> 'a @@ aliased val bar : 'a -> 'a @@ global end
|}]

(* default global, val unforkable *)
module type S = sig @@ global
    val foo : 'a -> 'a @@ unforkable
    val bar : 'a -> 'a
end
[%%expect{|
module type S =
  sig
    val foo : 'a -> 'a @@ global unforkable
    val bar : 'a -> 'a @@ global
  end
|}]

(* default global unforkable, val global *)
module type S = sig @@ global unforkable
    val foo : 'a -> 'a @@ global
    val bar : 'a -> 'a
end
[%%expect{|
module type S =
  sig
    val foo : 'a -> 'a @@ global
    val bar : 'a -> 'a @@ global unforkable
  end
|}]

(* default global unforkable, val local *)
module type S = sig @@ global unforkable
    val foo : 'a -> 'a @@ local
    val bar : 'a -> 'a
end
[%%expect{|
module type S =
  sig
    val foo : 'a -> 'a @@ aliased
    val bar : 'a -> 'a @@ global unforkable
  end
|}]

(* default global unforkable, val forkable *)
module type S = sig @@ global unforkable
    val foo : 'a -> 'a @@ forkable
    val bar : 'a -> 'a
end
[%%expect{|
module type S =
  sig
    val foo : 'a -> 'a @@ global
    val bar : 'a -> 'a @@ global unforkable
  end
|}]

(* default local, val global *)
module type S = sig @@ local
    val foo : 'a -> 'a @@ global
    val bar : 'a -> 'a
end
[%%expect{|
module type S = sig val foo : 'a -> 'a @@ global val bar : 'a -> 'a end
|}]

(* default unforkable, val global *)
module type S = sig @@ unforkable
    val foo : 'a -> 'a @@ global
    val bar : 'a -> 'a
end
[%%expect{|
module type S = sig val foo : 'a -> 'a @@ global val bar : 'a -> 'a end
|}]

(* default forkable, val global *)
module type S = sig @@ forkable
    val foo : 'a -> 'a @@ global
    val bar : 'a -> 'a
end
[%%expect{|
module type S =
  sig val foo : 'a -> 'a @@ global val bar : 'a -> 'a @@ forkable end
|}]

(* default forkable, val global unforkable *)
module type S = sig @@ forkable
    val foo : 'a -> 'a @@ global unforkable
    val bar : 'a -> 'a
end
[%%expect{|
module type S =
  sig
    val foo : 'a -> 'a @@ global unforkable
    val bar : 'a -> 'a @@ forkable
  end
|}]
