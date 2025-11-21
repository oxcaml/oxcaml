(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type 'witness wrap : value mod portable with 'witness = 
  { dummy : 'witness }

module Make_nonportable (A : sig
  type comparator_witness

  val comparator : comparator_witness wrap
end) = struct
  type comparator_witness = A.comparator_witness

  let comparator = A.comparator
end

module A = struct
  type comparator_witness : value mod portable
  let comparator : comparator_witness wrap = Obj.magic ()
end

module B : sig
  type comparator_witness : value mod portable

  val comparator : comparator_witness wrap @@ portable
end = struct
  include Make_nonportable (A)
end

[%%expect{|
type 'witness wrap = { dummy : 'witness; }
module Make_nonportable :
  functor
    (A : sig
           type comparator_witness
           val comparator : comparator_witness wrap
         end)
    ->
    sig
      type comparator_witness = A.comparator_witness
      val comparator : A.comparator_witness wrap
    end
module A :
  sig
    type comparator_witness : value mod portable
    val comparator : comparator_witness wrap
  end
module B :
  sig
    type comparator_witness : value mod portable
    val comparator : comparator_witness wrap @@ portable
  end
|}]
