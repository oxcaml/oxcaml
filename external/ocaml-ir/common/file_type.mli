type t =
  | Ml 
  | Mli 
val from_extension : string -> t Result.t
val check_language_supported : t -> Language.t -> unit Result.t
