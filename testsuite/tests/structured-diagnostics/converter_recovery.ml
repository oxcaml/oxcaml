(* TEST
 flags = "-I ${ocamlsrcdir}/driver -I ${ocamlsrcdir}/parsing";
 include ocamlcommon;
 expect;
*)

exception Converter_raises

exception Raised_by_converter

let () =
  Location.register_error_of_exn (function
    | Raised_by_converter -> Some (Location.error "reported after recovery")
    | _ -> None);
  Location.register_error_of_exn (function
    | Converter_raises -> raise Raised_by_converter
    | _ -> None)

let contains text sub =
  let length = String.length sub in
  let rec loop start =
    start + length <= String.length text
    && (String.sub text start length = sub || loop (start + 1))
  in
  loop 0

let reported exn =
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  match Structured_diagnostic_reporting.report_exception ppf exn with
  | () ->
    Format.pp_print_flush ppf ();
    `Reported (Buffer.contents buffer)
  | exception raised -> `Escaped (Printexc.to_string raised)

let show exn =
  match reported exn with
  | `Escaped raised -> Format.printf "escaped: %s@." raised
  | `Reported output ->
    let lines =
      List.filter (fun line -> line <> "") (String.split_on_char '\n' output)
    in
    Format.printf "lines: %d@." (List.length lines);
    List.iter
      (fun line ->
        Format.printf "object: %b@."
          (String.length line > 1
          && line.[0] = '{'
          && line.[String.length line - 1] = '}');
        Format.printf "reports the recovered error: %b@."
          (contains line "reported after recovery"))
      lines

let () = Structured_diagnostic_reporting.setup
    [| Structured_diagnostic_reporting.flag |]

let () = show Raised_by_converter

[%%expect {|
exception Converter_raises
exception Raised_by_converter
val contains : String.t -> String.t -> bool = <fun>
val reported : exn -> [> `Escaped of string | `Reported of string ] = <fun>
val show : exn -> unit = <fun>
lines: 1
object: true
reports the recovered error: true
|}]

let () = show Converter_raises

[%%expect {|
lines: 1
object: true
reports the recovered error: true
|}]
