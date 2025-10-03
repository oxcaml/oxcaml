open Format
open Cmx_format
open Compilenv

type emit =
  Unit_info.file_prefix -> progname:string -> ppf_dump:Format.formatter -> unit

module type File_extensions = sig
  (** File extensions include exactly one dot, so they can be added with regular string
      append, and removed by Filename.strip_extension *)

  val ext_obj : string

  val ext_lib : string

  val ext_flambda_obj : string

  val ext_flambda_lib : string

  (** Name of executable produced by linking if none is given with -o,
      e.g. [a.out] under Unix. *)
  val default_executable_name : string
end

module type Backend = sig
  val backend : Compile_common.opt_backend

  val link_shared :
    string list ->
    string ->
    genfns:Generic_fns.Tbl.t ->
    units_tolink:Linkenv.unit_link_info list ->
    ppf_dump:Format.formatter ->
    unit

  val link :
    string list ->
    string ->
    cached_genfns_imports:Generic_fns.Partition.Set.t ->
    genfns:Generic_fns.Tbl.t ->
    units_tolink:Linkenv.unit_link_info list ->
    ppf_dump:Format.formatter ->
    unit

  val link_partial : string -> string list -> unit

  val create_archive : string -> string list -> unit

  val compile_implementation :
    keep_symbol_tables:bool ->
    sourcefile:string option ->
    prefixname:string ->
    ppf_dump:Format.formatter ->
    Lambda.program ->
    unit

  val emit : emit option

  include File_extensions
end
