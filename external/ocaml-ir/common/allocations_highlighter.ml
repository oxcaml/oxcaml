open Std

let clear = "\027[0m"
let highlight_red = "\027[0;30;41m"
let underline_red = "\027[4;31m"
let highlight_green = "\027[0;30;42m"
let underline_green = "\027[4;32m"
let underline = "\027[4m"
let italic = "\027[03m"
let dark = "\027[30m"
let highlight_yellow = "\027[4;33m"

include Highlighter.Make (Allocations.Item)

let highlight
  ?(clear_marker = clear)
  ~marker
  ~start_newline
  ~filename
  ~content
  fmt
  allocations
  =
  let basename = Filename.basename filename in
  Allocations.to_list allocations
  |> List.filter ~f:(fun t ->
       match Allocations.Item.loc t with
       | Some l ->
         Location.Inferred.extract_filename l
         |> Filename.basename
         |> String.equal basename
       | _ -> false)
  |> highlight ~stop:clear_marker ~marker ~start_newline ~content fmt
;;
