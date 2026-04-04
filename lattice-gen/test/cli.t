  $ ../bin/lattice_gen.exe example.lattice --ml actual.ml --mli actual.mli --test-ml actual_test.ml
  $ sed -n '1,36p' actual.mli
  module Locality : sig
    module Const : sig
      type t = private int
  
      val global : t
      val local : t
  
      val bottom : t
      val top : t
  
      val leq : t -> t -> bool
      val equal : t -> t -> bool
      val join : t -> t -> t
      val meet : t -> t -> t
      val sub : t -> t -> t
      val imply : t -> t -> t
  
      val name : t -> string
      val of_name : string -> t option
      val pp : Format.formatter -> t -> unit
      val show : t -> string
  
      module Repr : sig
        val bits : int
        val mask : int
        val to_int : t -> int
        val of_int_exn : int -> t
      end
  
      val min : t
      val max : t
      val le : t -> t -> bool
      val print : Format.formatter -> t -> unit
      val legacy : t
    end
    type const = Const.t
  $ sed -n '1,20p' actual_test.ml
  open Actual
  
  [@@@warning "-32"]
  
  let exhaustive_threshold = 256
  
  let repr_exhaustive_threshold = 4096
  
  let sample_count = 200
  
  let rng = Random.State.make [| 0x51ed; 0x1234; 0x2026 |]
  
  let fail fmt = Printf.ksprintf failwith fmt
  
  let ensure cond fmt =
    Printf.ksprintf (fun message -> if not cond then fail "%s" message) fmt
  
  let safe_product limit a b =
    if a = 0 || b = 0
    then 0
