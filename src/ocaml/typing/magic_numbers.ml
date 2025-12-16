open Std

module Cmi = struct
  type error =
    | Not_an_interface of string
    | Wrong_version_interface of string * string
    | Corrupted_interface of string

  exception Error of error

  let to_version_opt = function
    | "Caml1999I017" -> Some "4.02"
    | "Caml1999I020" -> Some "4.03"
    | "Caml1999I021" -> Some "4.04 or 4.05"
    | "Caml1999I022" -> Some "4.06"
    | "Caml1999I023" -> Some "4.07.0"
    | "Caml1999I024" -> Some "4.07.1"
    | "Caml1999I025" -> Some "4.08"
    | "Caml1999I026" -> Some "4.09"
    | "Caml1999I027" -> Some "4.10"
    | "Caml1999I028" -> Some "4.11"
    | "Caml1999I029" | "Caml1999I500" -> Some "4.12"
    | "Caml1999I030" -> Some "4.13"
    | "Caml1999I031" | "Caml1999I501" -> Some "4.14"
    | "Caml1999I502" -> Some "4.14.1-5"
    | "Caml1999I503" -> Some "4.14.1-6"
    | "Caml1999I504" -> Some "4.14.1-7"
    | "Caml1999I505" -> Some "4.14.1-8"
    | "Caml1999I506" -> Some "4.14.1-10"
    | "Caml1999I507" -> Some "4.14.1-12"
    | "Caml1999I508" -> Some "4.14.1-13"
    | "Caml1999I509" -> Some "4.14.1-15"
    | "Caml1999I510" -> Some "4.14.1-16"
    | "Caml1999I511" -> Some "4.14.1-18"
    | "Caml1999I512" -> Some "4.14.1-19"
    | "Caml1999I513" -> Some "4.14.1-22"
    | "Caml1999I514" -> Some "4.14.1-24"
    | "Caml1999I032" -> Some "5.0"
    | "Caml1999I033" -> Some "5.1"
    | "Caml1999I520" -> Some "5.1.1minus"
    | "Caml1999I521" -> Some "5.1.1minus-4"
    | "Caml1999I522" -> Some "5.1.1minus-8"
    | "Caml1999I523" -> Some "5.1.1minus-9"
    | "Caml1999I524" -> Some "5.1.1minus-10"
    | "Caml1999I526" -> Some "5.1.1minus-11"
    | "Caml1999I527" -> Some "5.1.1minus-12"
    | "Caml1999I528" -> Some "5.1.1minus-13"
    | "Caml1999I529" -> Some "5.1.1minus-14"
    | "Caml1999I530" -> Some "5.1.1minus-16"
    | "Caml1999I531" -> Some "5.1.1minus-17"
    | "Caml1999I532" -> Some "5.1.1minus-18"
    | "Caml1999I533" -> Some "5.1.1minus-19"
    | "Caml1999I534" -> Some "5.1.1minus-20"
    | "Caml1999I535" -> Some "5.1.1minus-21"
    | "Caml1999I536" -> Some "5.1.1minus-23"
    | "Caml1999I537" -> Some "5.1.1minus-24"
    | "Caml1999I034" -> Some "5.2"
    | "Caml1999I550" -> Some "5.2.0minus-0"
    | "Caml1999I551" -> Some "5.2.0minus-1"
    | "Caml1999I552" -> Some "5.2.0minus-2"
    | "Caml1999I553" -> Some "5.2.0minus-3"
    | "Caml1999I554" -> Some "5.2.0minus-4"
    | "Caml1999I555" -> Some "5.2.0minus-5"
    | "Caml1999I556" -> Some "5.2.0minus-6"
    | "Caml1999I557" -> Some "5.2.0minus-7"
    | "Caml1999I558" -> Some "5.2.0minus-8"
    | "Caml1999I559" -> Some "5.2.0minus-9"
    | "Caml1999I560" -> Some "5.2.0minus-10"
    | "Caml1999I561" -> Some "5.2.0minus-11"
    | "Caml1999I562" -> Some "5.2.0minus-12"
    | "Caml1999I563" -> Some "5.2.0minus-13"
    | "Caml1999I564" -> Some "5.2.0minus-14"
    | "Caml1999I565" -> Some "5.2.0minus-16"
    | "Caml1999I566" -> Some "5.2.0minus-17"
    | "Caml1999I567" -> Some "5.2.0minus-18"
    | "Caml1999I568" -> Some "5.2.0minus-19"
    | "Caml1999I569" -> Some "5.2.0minus-22"
    | "Caml1999I570" -> Some "5.2.0minus-24"
    | "Caml1999I571" -> Some "5.2.0minus-25"
    | _ -> None

  let () = assert (to_version_opt Config.cmi_magic_number <> None)

  open Format
  module Style = Misc.Style

  let report_error ppf = function
    | Not_an_interface filename ->
        fprintf ppf "%a@ is not a compiled interface"
        (Style.as_inline_code Location.print_filename) filename
    | Wrong_version_interface (filename, compiler_magic) ->
      let merlin_ocaml_version =
        let ocaml_version =
          match to_version_opt Config.cmi_magic_number with
          | Some version -> "OCaml " ^ version
          | None -> "an unknown version of OCaml"
        in
        sprintf "%s (with magic number %s)" ocaml_version Config.cmi_magic_number
      in
      begin match to_version_opt compiler_magic with
      | None ->
        fprintf ppf
          "%a@ seems to be compiled with a version of OCaml (with magic number \
           %s) that is not supported by Merlin.@.\
          This instance of Merlin handles %s."
          Location.print_filename filename
          compiler_magic
          merlin_ocaml_version
      | Some version ->
        fprintf ppf
          "%a@ seems to be compiled with OCaml %s.@.\
           But this instance of Merlin handles %s."
          Location.print_filename filename
          version
          merlin_ocaml_version
      end
    | Corrupted_interface filename ->
        fprintf ppf "Corrupted compiled interface@ %a"
        (Style.as_inline_code Location.print_filename) filename

  let () =
    Location.register_error_of_exn
      (function
        | Error err -> Some (Location.error_of_printer_file report_error err)
        | _ -> None
      )
end
