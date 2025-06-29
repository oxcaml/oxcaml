open Ocaml_ir_fiber

module Raw_or_file : sig
  type 'a t =
    | Raw of 'a
    | File of string
end

type t

val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t
val create : ?mappings:Ir_map.raw Raw_or_file.t -> string Raw_or_file.t -> t
val ir : t -> string Fiber.t
val mappings : t -> Ir_map.raw option Fiber.t
