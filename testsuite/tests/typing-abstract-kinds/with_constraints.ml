(* TEST
   expect;
*)

(***************************************************)
(* Test: Basic non-destructive constraint behavior *)
module type S = sig
  kind_ k
  type t : k
end

module M = struct
  kind_ k = value
  type t = int
end

module M1 : S = M

(* You can use a signature with a substituted kind when the module matches. *)
module M2 : S with kind_ k = value = M
[%%expect{|
module type S = sig kind_ k type t : k end
module M : sig kind_ k = value type t = int end
module M1 : S
module M2 : sig kind_ k = value type t end
|}]

(* But not when it doesn't. *)
module M3 : S with kind_ k = bits64 = M
[%%expect{|
Line 1, characters 38-39:
1 | module M3 : S with kind_ k = bits64 = M
                                          ^
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = value type t = int end
       is not included in
         sig kind_ k = bits64 type t : bits64 end
       Kind declarations do not match:
         kind_ k = value
       is not included in
         kind_ k = bits64
       Their definitions are not equal.
|}]

(* You can see that the update from the substitution has happened in uses. *)
module F(X : S with kind_ k = float64) = struct
  type ('a : float64) t
  type s = X.t t
end
[%%expect{|
module F :
  functor (X : sig kind_ k = float64 type t : float64 end) ->
    sig type ('a : float64) t type s = X.t t end
|}]

module F(X : S with kind_ k = float64) = struct
  type ('a : bits64) t
  type s = X.t t
end
[%%expect{|
Line 3, characters 11-14:
3 |   type s = X.t t
               ^^^
Error: This type "X.t" should be an instance of type "('a : bits64)"
       The layout of X.t is float64
         because of the definition of t at line 3, characters 2-12.
       But the layout of X.t must be a sublayout of bits64
         because of the definition of t at line 2, characters 2-22.
|}]

(***********************************************)
(* Test: Basic destructive constraint behavior *)
module type S = sig
  kind_ k
  type t : k
end

module M = struct
  kind_ k = value
  type t = int
end

module M1 : S = M

(* You can use a signature with a substituted kind when the module matches. *)
module M2 : S with kind_ k := value = M
[%%expect{|
module type S = sig kind_ k type t : k end
module M : sig kind_ k = value type t = int end
module M1 : S
module M2 : sig type t end
|}]

module M2' : S with kind_ k := value = struct
  type t
end
[%%expect{|
module M2' : sig type t end
|}]

(* But not when it doesn't. *)
module M3 : S with kind_ k := bits64 = M
[%%expect{|
Line 1, characters 39-40:
1 | module M3 : S with kind_ k := bits64 = M
                                           ^
Error: Signature mismatch:
       Modules do not match:
         sig kind_ k = value type t = int end
       is not included in
         sig type t : bits64 end
       Type declarations do not match:
         type t = int
       is not included in
         type t : bits64
       The layout of the first is value non_pointer
         because it is the primitive type int.
       But the layout of the first must be a sublayout of bits64
         because of the definition of t at line 3, characters 2-12.
       Note: The layout of immediate is value non_pointer.
|}]

module M3' : S with kind_ k := value = struct
  type t : bits64
end
[%%expect{|
Lines 1-3, characters 39-3:
1 | .......................................struct
2 |   type t : bits64
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t : bits64 end
       is not included in
         sig type t end
       Type declarations do not match:
         type t : bits64
       is not included in
         type t
       The layout of the first is bits64
         because of the definition of t at line 2, characters 2-17.
       But the layout of the first must be a value layout
         because of the definition of t at line 3, characters 2-12.
|}]

(* You can see that the update from the substitution has happened in uses. *)
module F(X : S with kind_ k := float64) = struct
  type ('a : float64) t
  type s = X.t t
end
[%%expect{|
module F :
  functor (X : sig type t : float64 end) ->
    sig type ('a : float64) t type s = X.t t end
|}]

module F(X : S with kind_ k := float64) = struct
  type ('a : bits64) t
  type s = X.t t
end
[%%expect{|
Line 3, characters 11-14:
3 |   type s = X.t t
               ^^^
Error: This type "X.t" should be an instance of type "('a : bits64)"
       The layout of X.t is float64
         because of the definition of t at line 3, characters 2-12.
       But the layout of X.t must be a sublayout of bits64
         because of the definition of t at line 2, characters 2-22.
|}]

(*************************************************************************)
(* Test: Destructive substitutions update other places kinds can appear. *)

module type S = sig
  kind_ k

  type ('a : k) t

  val f : ('a : k) . 'a -> 'a
end

module type S' = S with kind_ k := float64 & value
[%%expect{|
module type S = sig kind_ k type ('a : k) t val f : ('a : k). 'a -> 'a end
module type S' =
  sig
    type ('a : float64 & value) t
    val f : ('a : float64 & value). 'a -> 'a
  end
|}]


(*******************************************************)
(* Test: You can't change the manifest, if one exists. *)
module type S = sig
  kind_ k = value
end

module type S' = S with kind_ k = value
[%%expect{|
module type S = sig kind_ k = value end
module type S' = sig kind_ k = value end
|}]

module type S' = S with kind_ k = immediate
[%%expect{|
Line 1, characters 17-43:
1 | module type S' = S with kind_ k = immediate
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "k"
       does not match its original definition in the constrained signature:
       Kind declarations do not match:
         kind_ k = immediate
       is not included in
         kind_ k = value
       Their definitions are not equal.
|}]

module type S' = S with kind_ k = any
[%%expect{|
Line 1, characters 17-37:
1 | module type S' = S with kind_ k = any
                     ^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "k"
       does not match its original definition in the constrained signature:
       Kind declarations do not match:
         kind_ k = any
       is not included in
         kind_ k = value
       Their definitions are not equal.
|}]

module type S' = S with kind_ k := value
[%%expect{|
module type S' = sig end
|}]

module type S' = S with kind_ k := immediate
[%%expect{|
Line 1, characters 17-44:
1 | module type S' = S with kind_ k := immediate
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "k"
       does not match its original definition in the constrained signature:
       Kind declarations do not match:
         kind_ k = immediate
       is not included in
         kind_ k = value
       Their definitions are not equal.
|}]

module type S' = S with kind_ k := any
[%%expect{|
Line 1, characters 17-38:
1 | module type S' = S with kind_ k := any
                     ^^^^^^^^^^^^^^^^^^^^^
Error: In this "with" constraint, the new definition of "k"
       does not match its original definition in the constrained signature:
       Kind declarations do not match:
         kind_ k = any
       is not included in
         kind_ k = value
       Their definitions are not equal.
|}]

(******************************)
(* Test: Nested substitution. *)

module type S = sig
  module M : sig
    kind_ k
    type t1 : k
  end

  kind_ k' = M.k
  type t2 : M.k

  module A : sig
    module M : sig
      kind_ k
      type t3 : M.k
      type t4 : k
    end
  end
end
[%%expect{|
module type S =
  sig
    module M : sig kind_ k type t1 : k end
    kind_ k' = M.k
    type t2 : M.k
    module A : sig module M : sig kind_ k type t3 : M.k type t4 : k end end
  end
|}]

(* we can substitute M.k, and we don't accidentally change A.M.k *)
module type S' = S with kind_ M.k = float64

module F(X : S') = struct
  type ('a : float64) require_float64
  type s1 = X.M.t1 require_float64
  type s2 = X.t2 require_float64
  type s3 = X.A.M.t3 require_float64
end
[%%expect{|
module type S' =
  sig
    module M : sig kind_ k = float64 type t1 : float64 end
    kind_ k' = float64
    type t2 : float64
    module A :
      sig module M : sig kind_ k type t3 : float64 type t4 : k end end
  end
module F :
  functor (X : S') ->
    sig
      type ('a : float64) require_float64
      type s1 = X.M.t1 require_float64
      type s2 = X.t2 require_float64
      type s3 = X.A.M.t3 require_float64
    end
|}]

module F'(X : S') = struct
  type ('a : float64) require_float64
  type s1 = X.A.M.t4 require_float64
end
[%%expect{|
Line 3, characters 12-20:
3 |   type s1 = X.A.M.t4 require_float64
                ^^^^^^^^
Error: This type "X.A.M.t4" should be an instance of type "('a : float64)"
       The kind of X.A.M.t4 is X.A.M.k
         because of the definition of t4 at line 14, characters 6-17.
       But the kind of X.A.M.t4 must be a subkind of float64
         because of the definition of require_float64 at line 2, characters 2-37.
|}]

(* Similarly for a destructive update. *)
module type S' = S with kind_ M.k := float64

module F(X : S') = struct
  type ('a : float64) require_float64
  type s1 = X.M.t1 require_float64
  type s2 = X.t2 require_float64
  type s3 = X.A.M.t3 require_float64
end
[%%expect{|
module type S' =
  sig
    module M : sig type t1 : float64 end
    kind_ k' = float64
    type t2 : float64
    module A :
      sig module M : sig kind_ k type t3 : float64 type t4 : k end end
  end
module F :
  functor (X : S') ->
    sig
      type ('a : float64) require_float64
      type s1 = X.M.t1 require_float64
      type s2 = X.t2 require_float64
      type s3 = X.A.M.t3 require_float64
    end
|}]

module F'(X : S') = struct
  type ('a : float64) require_float64
  type s1 = X.A.M.t4 require_float64
end
[%%expect{|
Line 3, characters 12-20:
3 |   type s1 = X.A.M.t4 require_float64
                ^^^^^^^^
Error: This type "X.A.M.t4" should be an instance of type "('a : float64)"
       The kind of X.A.M.t4 is X.A.M.k
         because of the definition of t4 at line 14, characters 6-17.
       But the kind of X.A.M.t4 must be a subkind of float64
         because of the definition of require_float64 at line 2, characters 2-37.
|}]

(*************************************************************)
(* Test: Only destructive update can save illegal shadowing. *)
module type S1 = sig
  kind_ k
  type t1 : k
end
module type S2 = sig
  kind_ k
  type t2 : k
end
module type Combined = sig
  include S1
  include S2 with kind_ k = k
end

[%%expect{|
module type S1 = sig kind_ k type t1 : k end
module type S2 = sig kind_ k type t2 : k end
Line 11, characters 2-29:
11 |   include S2 with kind_ k = k
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Illegal shadowing of included kind "k/2" by "k/1".
Line 10, characters 2-12:
10 |   include S1
       ^^^^^^^^^^
  Kind "k/2" came from this include.
Line 3, characters 2-13:
3 |   type t1 : k
      ^^^^^^^^^^^
  The type "t1" has no valid kind if "k/2" is shadowed.
|}]

module type S1 = sig
  kind_ k
  type t1 : k
end
module type S2 = sig
  kind_ k
  type t2 : k
end
module type Combined = sig
  include S1
  include S2 with kind_ k := k
end

[%%expect{|
module type S1 = sig kind_ k type t1 : k end
module type S2 = sig kind_ k type t2 : k end
module type Combined = sig kind_ k type t1 : k type t2 : k end
|}]

(*********************)
(* Recursive modules *)

(* A legal substitution that looks through an alias. *)
module type S = sig
  module rec M1 : sig
    kind_ k = M2.k
  end
  and M2 : sig
    kind_ k = value
  end
end with kind_ M1.k = value
[%%expect {|
module type S =
  sig
    module rec M1 : sig kind_ k = value end
    and M2 : sig kind_ k = value end
  end
|}]

(* The above needs the correct kind. *)
module type S = sig
  module rec M1 : sig
    kind_ k = M2.k
  end
  and M2 : sig
    kind_ k = value
  end
end with kind_ M1.k = float64
[%%expect {|
Lines 1-8, characters 16-29:
1 | ................sig
2 |   module rec M1 : sig
3 |     kind_ k = M2.k
4 |   end
5 |   and M2 : sig
6 |     kind_ k = value
7 |   end
8 | end with kind_ M1.k = float64
Error: In this "with" constraint, the new definition of "M1.k"
       does not match its original definition in the constrained signature:
       Kind declarations do not match:
         kind_ k = float64
       is not included in
         kind_ k = value
       Their definitions are not equal.
|}]

(* Loops are caught. *)
module type S = sig
  kind_ k1
  kind_ k2 = k1
end

module rec M : S with kind_ k1 = M.k2 = M
[%%expect {|
module type S = sig kind_ k1 kind_ k2 = k1 end
Line 6, characters 0-41:
6 | module rec M : S with kind_ k1 = M.k2 = M
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind "M.k1" is cyclic:
         "M.k1" = "M.k2",
         "M.k2" = "M.k1"
|}]

module type S = sig kind_ k end

module rec M : S with kind_ k = N.k = M
and N : S with kind_ k = M.k = N
[%%expect {|
module type S = sig kind_ k end
Line 3, characters 0-39:
3 | module rec M : S with kind_ k = N.k = M
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind "M.k" is cyclic:
         "M.k" = "N.k",
         "N.k" = "M.k"
|}]

module type S = sig
  module M : sig kind_ k1 end
  kind_ k2 = M.k1
end

module rec N : S with kind_ M.k1 = N.k2 = N
[%%expect {|
module type S = sig module M : sig kind_ k1 end kind_ k2 = M.k1 end
Line 6, characters 0-43:
6 | module rec N : S with kind_ M.k1 = N.k2 = N
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind "N.M.k1" is cyclic:
         "N.M.k1" = "N.k2",
         "N.k2" = "N.M.k1"
|}]

(* This next loopy example adapted from a comment on [check_well_founded] in
   typedecl. *)
module type K = sig kind_ k end
module Fix(F:(K -> K)) = struct
  module rec Fixed : K with kind_ k = F(Fixed).k = F(Fixed)
end
[%%expect {|
module type K = sig kind_ k end
module Fix :
  functor (F : K -> K) ->
    sig module rec Fixed : sig kind_ k = F(Fixed).k end end
|}]

module M = Fix(functor (M : K) -> struct kind_ k = M.k end)
[%%expect {|
Line 1, characters 11-59:
1 | module M = Fix(functor (M : K) -> struct kind_ k = M.k end)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the signature of this functor application:
       The kind "Fixed.k" is cyclic:
         "Fixed.k" = "F(Fixed).k",
         "F(Fixed).k" = "Fixed.k"
|}]

(* Test adapted from a similar one for types in [typing-sigsubst/sigsubst.ml]:
   In the presence of recursive modules, the use of a module can come before its
   definition (in the typed tree). *)
module Id(X : sig kind_ k end) = struct kind_ k = X.k end
module type S3 = sig
  module rec M : sig kind_ k = Id(M2).k end
  and M2 : sig kind_ k end
end with kind_ M2.k := float64
[%%expect {|
module Id : functor (X : sig kind_ k end) -> sig kind_ k = X.k end
Lines 2-5, characters 17-30:
2 | .................sig
3 |   module rec M : sig kind_ k = Id(M2).k end
4 |   and M2 : sig kind_ k end
5 | end with kind_ M2.k := float64
Error: This "with" constraint on "M2.k" makes the applicative functor
       type "Id(M2).k" ill-typed in the constrained signature:
       Modules do not match: sig end is not included in sig kind_ k end
       The kind "k" is required but not provided
|}]

(*************************************************)
(* Substituting a module containing kinds works. *)

module type S = sig
  module K : sig
    kind_ k
  end
  type t : K.k
end

module K' = struct
  kind_ k = value
end

module M1 : S with module K = K' = struct
  module K = K'
  type t = int
end

module M2 : S with module K := K' = struct
  type t = string
end
[%%expect {|
module type S = sig module K : sig kind_ k end type t : K.k end
module K' : sig kind_ k = value end
module M1 : sig module K : sig kind_ k = value end type t end
module M2 : sig type t end
|}]

module M1 : S with module K = K' = struct
  module K = K'
  type t = float#
end
[%%expect {|
Lines 1-4, characters 35-3:
1 | ...................................struct
2 |   module K = K'
3 |   type t = float#
4 | end
Error: Signature mismatch:
       Modules do not match:
         sig module K = K' type t = float# end
       is not included in
         sig module K : sig kind_ k = value end type t end
       Type declarations do not match:
         type t = float#
       is not included in
         type t
       The layout of the first is float64
         because it is the unboxed version of the primitive type float.
       But the layout of the first must be a value layout
         because of the definition of t at line 5, characters 2-14.
|}]

module M2 : S with module K := K' = struct
  type t = int64#
end
[%%expect {|
Lines 1-3, characters 36-3:
1 | ....................................struct
2 |   type t = int64#
3 | end
Error: Signature mismatch:
       Modules do not match:
         sig type t = int64# end
       is not included in
         sig type t end
       Type declarations do not match:
         type t = int64#
       is not included in
         type t
       The layout of the first is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of the first must be a value layout
         because of the definition of t at line 5, characters 2-14.
|}]

(***************************)
(* Multiple substitutions. *)

module type S = sig
  module type S1 = sig
    kind_ k1
    type t1 : k1
  end
  module M1 : S1
  kind_ k2 = M1.k1
  type t2 : k2
  module M2 : S1
  kind_ k3 = M2.k1
  type t3 : k3
end

module type S' =
  S with kind_ M1.k1 = value & float64
            and type M1.t1 = #(int * float#)
            and type t2 = #(string * float#)
     and kind_ M2.k1 := bits64
            and type t3 = int64#
[%%expect {|
module type S =
  sig
    module type S1 = sig kind_ k1 type t1 : k1 end
    module M1 : S1
    kind_ k2 = M1.k1
    type t2 : M1.k1
    module M2 : S1
    kind_ k3 = M2.k1
    type t3 : M2.k1
  end
module type S' =
  sig
    module type S1 = sig kind_ k1 type t1 : k1 end
    module M1 : sig kind_ k1 = value & float64 type t1 = #(int * float#) end
    kind_ k2 = M1.k1
    type t2 = #(string * float#)
    module M2 : sig type t1 : bits64 end
    kind_ k3 = bits64
    type t3 = int64#
  end
|}]

(******************)
(* Module aliases *)

(* Test adapted from a similar one for types in [typing-sigsubst/sigsubst.ml] *)
module type S = sig
  module M : sig kind_ k end
  module A = M
end with kind_ M.k = float64
[%%expect {|
module type S = sig module M : sig kind_ k = float64 end module A = M end
|}]

module type S = sig
  module M : sig kind_ k end
  module A = M
end with kind_ M.k := float64
[%%expect {|
Lines 1-4, characters 16-29:
1 | ................sig
2 |   module M : sig kind_ k end
3 |   module A = M
4 | end with kind_ M.k := float64
Error: This "with" constraint on "M.k" changes "M", which is aliased
       in the constrained signature (as "A").
|}]

(**********************************************)
(* Destructive substitution breaking functors *)

(* Test adapted from a similar one for types in [typing-sigsubst/sigsubst.ml] *)
module type S = sig
  module M : sig kind_ k1 kind_ k2 end
  module F(X : sig kind_ k1 end) : sig kind_ k1 end
  kind_ k = F(M).k1
end
[%%expect {|
module type S =
  sig
    module M : sig kind_ k1 kind_ k2 end
    module F : functor (X : sig kind_ k1 end) -> sig kind_ k1 end
    kind_ k = F(M).k1
  end
|}]

(* This one doesn't work because we're removing a kind needed by the functor. *)
module type S2 = S with kind_ M.k1 := float64
[%%expect {|
Line 1, characters 17-45:
1 | module type S2 = S with kind_ M.k1 := float64
                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This "with" constraint on "M.k1" makes the applicative functor
       type "F(M).k1" ill-typed in the constrained signature:
       Modules do not match:
         sig kind_ k2 = M.k2 end
       is not included in
         sig kind_ k1 end
       The kind "k1" is required but not provided
|}]

(* But this one is fine *)
module type S2 = S with kind_ M.k2 := float64
[%%expect {|
module type S2 =
  sig
    module M : sig kind_ k1 end
    module F : functor (X : sig kind_ k1 end) -> sig kind_ k1 end
    kind_ k = F(M).k1
  end
|}]

(**********************************************)
(* Test: substitutions that affect with kinds *)

module type S = sig
  kind_ k
  type t : k
  type s : value mod portable with t
end
[%%expect {|
module type S = sig kind_ k type t : k type s : value mod portable with t end
|}]

module F1(X : S with kind_ k = immediate) = struct
  type r : value mod portable = X.s
end

module F2(X : S with kind_ k := immediate) = struct
  type r : value mod portable = X.s
end

[%%expect {|
module F1 :
  functor
    (X : sig
           kind_ k = immediate
           type t : immediate
           type s : value mod portable with t
         end)
    -> sig type r = X.s end
module F2 :
  functor (X : sig type t : immediate type s : value mod portable with t end)
    -> sig type r = X.s end
|}]

module F1(X : S with kind_ k = value) = struct
  type r : value mod portable = X.s
end
[%%expect {|
Line 2, characters 2-35:
2 |   type r : value mod portable = X.s
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "X.s" is value mod portable with X.t
         because of the definition of s at line 4, characters 2-36.
       But the kind of type "X.s" must be a subkind of value mod portable
         because of the definition of r at line 2, characters 2-35.
|}]

module F2(X : S with kind_ k := value) = struct
  type r : value mod portable = X.s
end

[%%expect {|
Line 2, characters 2-35:
2 |   type r : value mod portable = X.s
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "X/2.s" is value mod portable with X/2.t
         because of the definition of s at line 4, characters 2-36.
       But the kind of type "X/2.s" must be a subkind of value mod portable
         because of the definition of r at line 2, characters 2-35.
|}]

(**************************************************)
(* Test: substitution into subsequent functor arg *)

module type S = sig
  kind_ k
  type t : k
end

module F (X : S) (Y : S with kind_ k = X.k) : sig
  type s : X.k
end = struct
  type s = Y.t
end
[%%expect {|
module type S = sig kind_ k type t : k end
module F :
  functor (X : S) (Y : sig kind_ k = X.k type t : k end) ->
    sig type s : X.k end
|}]

module F (X : S) (Y : S with kind_ k := X.k) : sig
  type s : X.k
end = struct
  type s = Y.t
end
[%%expect {|
module F : functor (X : S) (Y : sig type t : X.k end) -> sig type s : X.k end
|}]

(************************)
(* Test: scannable axes *)

module type S = sig
  kind_ k
  type t : k
end

module F1(X : S with kind_ k = value non_pointer) = struct
  type s : any non_pointer = X.t
end

module F2(X : S with kind_ k := value non_pointer) = struct
  type s : any non_pointer = X.t
end
[%%expect {|
module type S = sig kind_ k type t : k end
module F1 :
  functor
    (X : sig kind_ k = value non_pointer type t : value non_pointer end) ->
    sig type s = X.t end
module F2 :
  functor (X : sig type t : value non_pointer end) -> sig type s = X.t end
|}]

module F1(X : S with kind_ k = value) = struct
  type s : any non_pointer = X.t
end
[%%expect {|
Line 2, characters 2-32:
2 |   type s : any non_pointer = X.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "X.t" is value
         because of the definition of t at line 3, characters 2-12.
       But the layout of type "X.t" must be a sublayout of any non_pointer
         because of the definition of s at line 2, characters 2-32.
|}]

module F2(X : S with kind_ k := value) = struct
  type s : any non_pointer = X.t
end
[%%expect {|
Line 2, characters 2-32:
2 |   type s : any non_pointer = X.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "X/2.t" is value
         because of the definition of t at line 3, characters 2-12.
       But the layout of type "X/2.t" must be a sublayout of any non_pointer
         because of the definition of s at line 2, characters 2-32.
|}]

(****************************)
(* Test: motivating example *)

module type Box = sig
  kind_ k
  type ('a : k) t
  val make : ('a : k). 'a -> 'a t
  val get : ('a : k). 'a t -> 'a
end

module M1 : Box with kind_ k = float32 = struct
  kind_ k = float32
  type ('a : float32) t = { x : 'a }
  let make x = { x }
  let get x = x.x
end

module M2 : Box with kind_ k := value & bits64 = struct
  type ('a : value & bits64) t = { x : 'a }
  let make x = { x }
  let get x = x.x
end

[%%expect{|
module type Box =
  sig
    kind_ k
    type ('a : k) t
    val make : ('a : k). 'a -> 'a t
    val get : ('a : k). 'a t -> 'a
  end
module M1 :
  sig
    kind_ k = float32
    type ('a : float32) t
    val make : ('a : float32). 'a -> 'a t
    val get : ('a : float32). 'a t -> 'a
  end
module M2 :
  sig
    type ('a : value & bits64) t
    val make : ('a : value & bits64). 'a -> 'a t
    val get : ('a : value & bits64). 'a t -> 'a
  end
|}]
