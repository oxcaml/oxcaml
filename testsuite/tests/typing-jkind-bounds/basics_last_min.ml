(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type 'witness wrap : value mod portable with 'witness = { dummy : int }
[@@unsafe_allow_any_mode_crossing]

module type S_portable = sig
  type comparator_witness : value mod portable

  val comparator : comparator_witness wrap
end

module type Result = sig
  type comparator_witness : value mod portable

  val comparator : comparator_witness wrap @@ portable
end

module Test (A : S_portable) : Result = struct
  type comparator_witness = A.comparator_witness

  let comparator = A.comparator
end
[%%expect{|
type 'witness wrap : value mod portable with 'witness = { dummy : int; }
[@@unsafe_allow_any_mode_crossing]
module type S_portable =
  sig
    type comparator_witness : value mod portable
    val comparator : comparator_witness wrap
  end
module type Result =
  sig
    type comparator_witness : value mod portable
    val comparator : comparator_witness wrap @@ portable
  end
module Test : functor (A : S_portable) -> Result
|}]
