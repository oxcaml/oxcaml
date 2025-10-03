type unit_link_info = {
  name: Compilation_unit.t;
  defines: Compilation_unit.t list;
  file_name: string;
  crc: Digest.t;
  (* for shared libs *)
  dynunit : Cmxs_format.dynunit option;
}

module type File_extensions = sig
  (** File extensions include exactly one dot, so they can be added with regular string
      append, and removed by Filename.strip_extension *)

  val ext_obj : string
  val ext_lib : string
  val ext_flambda_obj : string
  val ext_flambda_lib : string
end

type emit =
  Unit_info.file_prefix
  -> progname:string
  -> Compile_common.info
  -> ppf_dump:Format.formatter
  -> unit

module type S = sig
  val link_shared
    :  string list
    -> string
    -> genfns:Generic_fns.Tbl.t
    -> units_to_link:unit_link_info list
    -> ppf_dump:Format.formatter
    -> unit

  val link
    :  string list
    -> string
    -> cached_genfns_imports:Generic_fns.Partition.Set.t
    -> genfns:Generic_fns.Tbl.t
    -> units_to_link:unit_link_info list
    -> ppf_dump:Format.formatter
    -> unit

  val link_partial :  string -> string list -> unit
  val create_archive : string -> string list -> unit

  val compile_implementation
    : keep_symbol_tables:bool
    -> sourcefile:string option
    -> prefixname:string
    -> ppf_dump:Format.formatter
    -> Lambda.program
    -> unit

  val emit : emit option

  include File_extensions
end
