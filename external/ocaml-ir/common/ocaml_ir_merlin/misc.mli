val canonicalize_filename : ?cwd:string -> string -> string
val expand_glob : ?filter:(string -> bool) -> string -> string list -> string list
val expand_directory : string -> string -> string
val rev_split_words : string -> string list
