open Std

module Annotation_location = struct
  type t =
    { lnum : int
    ; col : int
    }

  let of_allocation_location = function
    | None ->
      raise
        (Invalid_argument
           "Cannot construct [Annotation_location] from unknown allocation location")
    | Some loc ->
      let Location.Simple.{ loc_start = { pos_lnum; pos_bol; pos_cnum; _ }; _ } =
        Location.Inferred.extract loc
      in
      { lnum = pos_lnum - 1; col = pos_cnum - pos_bol }
  ;;

  let equal { lnum; col } { lnum = lnum'; col = col' } = lnum = lnum' && col = col'
end

let alloc_comment = "(* ALLOC *)"

module Allocation_event_in_file = Event_in_file.Make (Allocations.Item)

let sort_allocations_in_events ~basename allocations =
  Allocations.to_list allocations
  |> List.filter ~f:(fun t ->
       match Allocations.Item.loc t with
       | Some l ->
         Location.Inferred.extract_filename l
         |> Filename.basename
         |> String.equal basename
       | _ -> false)
  |> Allocation_event_in_file.sort
;;

let get_annotation_locations ~allocations ~basename =
  let sorted = sort_allocations_in_events ~basename allocations in
  List.filter_map sorted ~f:(fun Allocation_event_in_file.(type_, { data = item; _ }) ->
    let location = Allocations.Item.loc item in
    match type_ with
    | Allocation_event_in_file.Start ->
      Some (Annotation_location.of_allocation_location location)
    | End -> None)
  |> List.remove_consecutive_duplicates ~equal:Annotation_location.equal
;;

let make_alloc_allowed_filter ~content =
  let lines = String.split_lines content in
  let pragma_lines =
    (List.map lines ~f:String.trim
    |> List.filter_mapi ~f:(fun i line ->
         match line with
         | "(* ALLOC allow *)" -> Some (i, true)
         | "(* ALLOC disallow *)" -> Some (i, false)
         | _ -> None)
    |> List.remove_consecutive_duplicates ~equal:(fun (_, is_allowed) (_, is_allowed') ->
         Bool.equal is_allowed is_allowed'))
    @ [ List.length lines, true ]
    |> Array.of_list
  in
  fun lnum ->
    Array.Binary_search.last_less_than_or_equal_to
      pragma_lines
      ~compare:(fun key elt -> Int.compare key (fst elt))
      ~key:lnum
    |> Option.map ~f:(fun index -> pragma_lines.(index) |> snd)
    |> Option.value ~default:false
;;

let validate_alloc_pragmas ~filename ~content =
  let lines = String.split_lines content |> Array.of_list in
  let lnum_and_col_from_offset offset =
    let line_offsets =
      Array.fold_left lines ~init:(0, []) ~f:(fun (sum, prefix_sum) line ->
        sum + String.length line + 1, sum :: prefix_sum)
      |> snd
      |> List.rev
      |> Array.of_list
    in
    let lnum =
      Array.Binary_search.last_less_than_or_equal_to
        line_offsets
        ~compare:Int.compare
        ~key:offset
      |> Option.value_exn
    in
    let col = offset - line_offsets.(lnum) in
    lnum, col
  in
  let format_error_msg ~lnum ~col ~msg =
    let line = lines.(lnum) in
    let pointer = String.make col ' ' ^ "^" in
    Printf.sprintf
      "File \"%s\", line %d, character %d:\n%s\n%s\n\nError: %s"
      filename
      (lnum + 1)
      col
      line
      pointer
      msg
  in
  let regex = Str.regexp "(\\*[\n\r ]*ALLOC[\n\r ]*.*[\n\r ]*\\*)" in
  let error_msg =
    Seq.unfold
      (fun (start, prev_alloc_pragma) ->
        match Str.search_forward regex content start with
        | exception Not_found -> None
        | index ->
          let alloc_pragma = Str.matched_string content in
          Some ((index, prev_alloc_pragma, alloc_pragma), (index + 1, alloc_pragma)))
      (0, "")
    |> List.of_seq
    |> List.filter_map ~f:(fun (index, prev_alloc_pragma, alloc_pragma) ->
         let lnum, col = lnum_and_col_from_offset index in
         let line = lines.(lnum) in
         let some_format_error_msg ~msg = Some (format_error_msg ~lnum ~col ~msg) in
         if not (String.equal (String.trim line) alloc_pragma)
         then some_format_error_msg ~msg:"Alloc comments should be on a line by itself."
         else (
           match alloc_pragma with
           | "(* ALLOC allow *)" | "(* ALLOC disallow *)" ->
             if String.equal prev_alloc_pragma alloc_pragma
             then
               some_format_error_msg
                 ~msg:
                   "This alloc comment is redundant as there is an identical alloc \
                    comment before it. Consecutive alloc comments should not be \
                    identical. Please either remove this one, or check that you have not \
                    left a stray alloc comment."
             else None
           | _ ->
             some_format_error_msg
               ~msg:
                 "Invalid or badly formatted alloc comment. Use either (* ALLOC allow *) \
                  or (* ALLOC disallow *) with the exact format displayed here."))
    |> String.concat ~sep:"\n\n"
  in
  if String.length error_msg > 0
  then (
    Printf.eprintf "%s\n\n" error_msg;
    Stdlib.exit 1)
;;

let annotate ~allocations ~filename ~content =
  validate_alloc_pragmas ~filename ~content;
  let lines = String.split_lines content |> Array.of_list in
  let basename = Filename.basename filename in
  let alloc_allowed_filter = make_alloc_allowed_filter ~content in
  get_annotation_locations ~allocations ~basename
  |> List.filter_map ~f:(fun Annotation_location.{ lnum; col } ->
       if not (alloc_allowed_filter lnum) then Some (lnum, col) else None)
  |> Int.Map.of_alist_multi
  |> Int.Map.to_alist
  |> List.iter ~f:(fun (lnum, cols) ->
       lines.(lnum)
       |> String.split_at_indices ~indices:cols
       |> List.intersperse ~sep:(" " ^ alloc_comment ^ " ")
       |> String.concat ~sep:""
       |> Array.set lines lnum);
  lines |> Array.to_list |> String.concat ~sep:"\n"
;;
