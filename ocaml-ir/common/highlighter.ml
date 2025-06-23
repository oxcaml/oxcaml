open Std
module Make(M:sig
                type t[@@deriving sexp_of]
                include
                  sig
                    [@@@ocaml.warning "-32"]
                    val sexp_of_t : t -> Sexplib0.Sexp.t
                  end[@@ocaml.doc "@inline"][@@merlin.hide ]
                val compare : t -> t -> int
                val byte_offset_within_file : t -> (int * int) option
              end) =
  struct
    module Being_highlighted =
      struct
        let empty = []
        let none t = match t with | [] -> true | _ -> false
        let end_ ~data t =
          List.filter ~f:(fun a -> (M.compare a data) <> 0) t
        let start ~data t = data :: t
        let current t = match t with | [] -> None | hd::_ -> Some hd
      end
    module Event_in_file = (Event_in_file.Make)(M)
    let highlight ~marker ~stop ~start_newline ~content ppf items =
      let line = ref 1 in
      let fprintf fmt = Format.fprintf ppf fmt in
      let print_substring ~start ~end_ stack =
        let substring = String.sub ~pos:start ~len:(end_ - start) content in
        let is_only_whitespace_since_start_line = ref false in
        String.iter
          ~f:(function
              | '\n' ->
                  (incr line;
                   if not (Being_highlighted.none stack)
                   then fprintf "%s" stop;
                   fprintf "\n%s" (start_newline (!line));
                   is_only_whitespace_since_start_line := true)
              | ' ' | '\t' as c when !is_only_whitespace_since_start_line ->
                  fprintf "%c" c
              | char ->
                  (if !is_only_whitespace_since_start_line
                   then
                     (Option.iter (fun a -> fprintf "%s" (marker a))
                        (Being_highlighted.current stack);
                      is_only_whitespace_since_start_line := false);
                   fprintf "%c" char)) substring in
      let highlighting_events = Event_in_file.sort items in
      fprintf "%s" (start_newline 1);
      (let len_content = String.length content in
       let (stack, cursor) =
         List.fold_left ~init:(Being_highlighted.empty, 0)
           ~f:(fun (stack, cursor) (type_, Event_in_file.{ pos; data } ) ->
                 print_substring ~start:cursor ~end_:pos stack;
                 (match type_ with
                  | Event_in_file.Start ->
                      let stack = Being_highlighted.start ~data stack in
                      (fprintf "%s" (marker data); (stack, pos))
                  | End ->
                      let stack = Being_highlighted.end_ ~data stack in
                      (fprintf "%s" stop;
                       (Being_highlighted.current stack) |>
                         (Option.iter (fun a -> fprintf "%s" (marker a)));
                       (stack, pos)))) highlighting_events in
       print_substring ~start:cursor ~end_:len_content stack)
  end
