(* A first-class module avoids a link-time dependency on [Unix], which would
   cause ocamloptcomp to depend on [Unix], thus breaking compilerlibs clients.
   The signature is written out explicitly rather than using "module type of"
   on the [Unix] module so that the dune file doesn't need to have [unix] in the
   [libraries] stanza.  This ensures accidental uses of [Unix] will be
   caught. *)
module type S = sig
  type file_descr

  type open_flag =
      O_RDONLY
    | O_WRONLY
    | O_RDWR
    | O_NONBLOCK
    | O_APPEND
    | O_CREAT
    | O_TRUNC
    | O_EXCL
    | O_NOCTTY
    | O_DSYNC
    | O_SYNC
    | O_RSYNC
    | O_SHARE_DELETE
    | O_CLOEXEC
    | O_KEEPEXEC

  type file_perm = int

  val openfile : string -> open_flag list -> file_perm -> file_descr

  val close : file_descr -> unit

  type seek_command =
      SEEK_SET
    | SEEK_CUR
    | SEEK_END

  val lseek : file_descr -> int -> seek_command -> int

  val map_file :
    file_descr ->
    ?pos:int64 ->
    ('a, 'b) Stdlib.Bigarray.kind ->
    'c Stdlib.Bigarray.layout -> bool -> int array ->
    ('a, 'b, 'c) Stdlib.Bigarray.Genarray.t

  val getpid : unit -> int

  val stdin : file_descr

  val stdout : file_descr

  val stderr : file_descr

  val create_process :
    string -> string array -> file_descr -> file_descr -> file_descr -> int

  type wait_flag =
      WNOHANG
    | WUNTRACED

  type process_status =
      WEXITED of int
    | WSIGNALED of int
    | WSTOPPED of int

  val waitpid : wait_flag list -> int -> int * process_status

  val kill : int -> int -> unit
end
