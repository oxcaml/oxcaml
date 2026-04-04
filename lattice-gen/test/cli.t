  $ ../bin/lattice_gen.exe example.lattice --ml actual.ml --mli actual.mli --test-ml actual_test.ml
  $ sed -n '1,28p' actual.mli
  module Locality : sig
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
  end
  $ sed -n '1,20p' actual_test.ml
  open Actual
  
  let exhaustive_threshold = 256
  
  let sample_count = 200
  
  let rng = Random.State.make [| 0x51ed; 0x1234; 0x2026 |]
  
  let fail fmt = Printf.ksprintf failwith fmt
  
  let ensure cond fmt =
    Printf.ksprintf (fun message -> if not cond then fail "%s" message) fmt
  
  let safe_product limit a b =
    if a = 0 || b = 0
    then 0
    else if a > limit / b
    then limit + 1
    else a * b
  
  $ ocamlc -c actual.mli
  $ ocamlc -c actual.ml
  $ ocamlc -o actual_test actual.cmo actual_test.ml
  $ ./actual_test
