(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type 'witness wrap : value mod portable with 'witness =
  { dummy : int }
[@@unsafe_allow_any_mode_crossing]

module type Derived1 = sig
  type 'cmp_a comparator_witness : value mod portable with 'cmp_a

  val comparator : 'cmp_a wrap -> 'cmp_a comparator_witness wrap
end

module T1 : Derived1 = struct
  type 'cmp_a comparator_witness : value mod portable with 'cmp_a

  let comparator a = { dummy = a.dummy }
end

module type S_portable = sig
  type comparator_witness : value mod portable

  val comparator : comparator_witness wrap
end

module type S = sig
  type comparator_witness

  val comparator : comparator_witness wrap
end

module Make_nonportable (A : S) = struct
  type comparator_witness = A.comparator_witness T1.comparator_witness

  let comparator = T1.comparator A.comparator
end

module type Result = sig
  type comparator_witness : value mod portable

  val comparator : comparator_witness wrap @@ portable
end

module Test (A : S_portable) : Result = struct
  include Make_nonportable (A)
end
[%%expect{|
type 'witness wrap : value mod portable with 'witness = { dummy : int; }
[@@unsafe_allow_any_mode_crossing]
module type Derived1 =
  sig
    type 'cmp_a comparator_witness : value mod portable with 'cmp_a
    val comparator : 'cmp_a wrap -> 'cmp_a comparator_witness wrap
  end
module T1 : Derived1
module type S_portable =
  sig
    type comparator_witness : value mod portable
    val comparator : comparator_witness wrap
  end
module type S =
  sig type comparator_witness val comparator : comparator_witness wrap end
module Make_nonportable :
  functor (A : S) ->
    sig
      type comparator_witness = A.comparator_witness T1.comparator_witness
      val comparator : A.comparator_witness T1.comparator_witness wrap
    end
module type Result =
  sig
    type comparator_witness : value mod portable
    val comparator : comparator_witness wrap @@ portable
  end
module Test : functor (A : S_portable) -> Result
|}]
