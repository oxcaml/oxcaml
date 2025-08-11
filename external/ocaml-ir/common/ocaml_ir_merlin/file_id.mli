type t
[@@ocaml.doc
  " An instance of [t] represents the identity of the contents of a file path. Use this to\n\
  \    quickly detect if a file has changed. (Detection is done by checking some fields \
   from\n\
  \    stat syscall, it can be tricked but should behave well in regular cases). FIXME:\n\
  \    precision of mtime is still the second?! "]

val check : t -> t -> bool
  [@@ocaml.doc
    " Returns true iff the heuristic determines that the file contents has not changed. "]

val get : string -> t
  [@@ocaml.doc
    " [file_id filename] computes an id for the current contents of [filename] "]

val with_cache : (unit -> 'a) -> 'a
