(** Findlib package lookups, performed by running the [ocamlfind] executable
    found on [PATH]. Successful [ocamlfind] invocations are memoized. *)

module Config : sig
  (** Which findlib installation to query. *)
  type t =
    { conf : string option;  (** Path to [findlib.conf]. *)
      path : string list;
          (** Package search path. When non-empty it replaces the [OCAMLPATH]
            environment variable. *)
      toolchain : string option
    }

  val default : t
end

type 'a result := ('a, string) Stdlib.result

module Package : sig
  type t =
    { name : string;
      directory : string;
      ppx : string option;  (** The [ppx] META property, unresolved. *)
      ppxopt : (string * string list) list
          (** The [ppxopt] META property, parsed into (ppx package, options)
            pairs. Options are unresolved. *)
    }
end

(** [query config packages] returns the direct and indirect dependencies of
    [packages] (including [packages] themselves), deepest dependency first.
    [ocamlfind] is not run when [packages] is empty. *)
val query : config:Config.t -> string list -> Package.t list result

(** [None] when the package is not installed. *)
val package_directory : config:Config.t -> string -> string option result

(** Resolves findlib's path notations, like [+] and [@]. *)
val resolve_path : config:Config.t -> base:string -> string -> string result
