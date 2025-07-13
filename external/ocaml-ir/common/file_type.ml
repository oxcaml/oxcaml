type t =
  | Ml
  | Mli

let from_extension = function
  | ".mlt" | ".ml" -> Result.return Ml
  | ".mli" -> Result.return Mli
  | _ -> Result.unsupported_source_file_extension
;;

let check_language_supported t language =
  match t with
  | Ml -> Result.return ()
  | Mli ->
    (match language with
     | Language.Ppx | Parse_tree | Typed_tree -> Result.return ()
     | _ ->
       Result.unsupported_language
         (Printf.sprintf
            "Language %s is not supported for *.mli files"
            (Language.string_of_t language)))
;;
