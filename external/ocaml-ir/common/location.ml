open Sexplib.Std
open Std

module Position = struct
  type t =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving sexp]

  include struct
    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let error_source__002_ = "location.ml.Position.t" in
       fun x__003_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__002_
           ~fields:
             (Field
                { name = "pos_fname"
                ; kind = Required
                ; conv = string_of_sexp
                ; rest =
                    Field
                      { name = "pos_lnum"
                      ; kind = Required
                      ; conv = int_of_sexp
                      ; rest =
                          Field
                            { name = "pos_bol"
                            ; kind = Required
                            ; conv = int_of_sexp
                            ; rest =
                                Field
                                  { name = "pos_cnum"
                                  ; kind = Required
                                  ; conv = int_of_sexp
                                  ; rest = Empty
                                  }
                            }
                      }
                })
           ~index_of_field:
             (function
              | "pos_fname" -> 0
              | "pos_lnum" -> 1
              | "pos_bol" -> 2
              | "pos_cnum" -> 3
              | _ -> -1)
           ~allow_extra_fields:false
           ~create:(fun (pos_fname, (pos_lnum, (pos_bol, (pos_cnum, ())))) : t ->
             { pos_fname; pos_lnum; pos_bol; pos_cnum })
           x__003_
        : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun { pos_fname = pos_fname__005_
           ; pos_lnum = pos_lnum__007_
           ; pos_bol = pos_bol__009_
           ; pos_cnum = pos_cnum__011_
           } ->
         let bnds__004_ = ([] : _ Stdlib.List.t) in
         let bnds__004_ =
           let arg__012_ = sexp_of_int pos_cnum__011_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pos_cnum"; arg__012_ ] :: bnds__004_
             : _ Stdlib.List.t)
         in
         let bnds__004_ =
           let arg__010_ = sexp_of_int pos_bol__009_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pos_bol"; arg__010_ ] :: bnds__004_
             : _ Stdlib.List.t)
         in
         let bnds__004_ =
           let arg__008_ = sexp_of_int pos_lnum__007_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pos_lnum"; arg__008_ ] :: bnds__004_
             : _ Stdlib.List.t)
         in
         let bnds__004_ =
           let arg__006_ = sexp_of_string pos_fname__005_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "pos_fname"; arg__006_ ] :: bnds__004_
             : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__004_
        : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let compare a b =
    let c = String.compare a.pos_fname b.pos_fname in
    if c <> 0
    then c
    else (
      let c = Int.compare a.pos_cnum b.pos_cnum in
      if c <> 0
      then c
      else (
        let c = Int.compare a.pos_bol b.pos_bol in
        if c <> 0 then c else Int.compare a.pos_lnum b.pos_lnum))
  ;;

  let keep_file_basenames t = { t with pos_fname = Filename.basename t.pos_fname }

  let of_lexing_position ({ pos_fname; pos_cnum; pos_bol; pos_lnum } : Lexing.position) =
    { pos_fname; pos_cnum; pos_bol; pos_lnum }
  ;;
end

module Simple = struct
  module T = struct
    type t =
      { loc_start : Position.t
      ; loc_end : Position.t
      }
    [@@deriving sexp]

    include struct
      let _ = fun (_ : t) -> ()

      let t_of_sexp =
        (let error_source__014_ = "location.ml.Simple.T.t" in
         fun x__015_ ->
           Sexplib0.Sexp_conv_record.record_of_sexp
             ~caller:error_source__014_
             ~fields:
               (Field
                  { name = "loc_start"
                  ; kind = Required
                  ; conv = Position.t_of_sexp
                  ; rest =
                      Field
                        { name = "loc_end"
                        ; kind = Required
                        ; conv = Position.t_of_sexp
                        ; rest = Empty
                        }
                  })
             ~index_of_field:
               (function
                | "loc_start" -> 0
                | "loc_end" -> 1
                | _ -> -1)
             ~allow_extra_fields:false
             ~create:(fun (loc_start, (loc_end, ())) : t -> { loc_start; loc_end })
             x__015_
          : Sexplib0.Sexp.t -> t)
      ;;

      let _ = t_of_sexp

      let sexp_of_t =
        (fun { loc_start = loc_start__017_; loc_end = loc_end__019_ } ->
           let bnds__016_ = ([] : _ Stdlib.List.t) in
           let bnds__016_ =
             let arg__020_ = Position.sexp_of_t loc_end__019_ in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc_end"; arg__020_ ] :: bnds__016_
               : _ Stdlib.List.t)
           in
           let bnds__016_ =
             let arg__018_ = Position.sexp_of_t loc_start__017_ in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc_start"; arg__018_ ]
              :: bnds__016_
               : _ Stdlib.List.t)
           in
           Sexplib0.Sexp.List bnds__016_
          : t -> Sexplib0.Sexp.t)
      ;;

      let _ = sexp_of_t
    end [@@ocaml.doc "@inline"] [@@merlin.hide]

    let fname { loc_start; _ } = loc_start.pos_fname

    let keep_file_basenames { loc_start; loc_end } =
      { loc_start = Position.keep_file_basenames loc_start
      ; loc_end = Position.keep_file_basenames loc_end
      }
    ;;

    let compare a b =
      let c = Position.compare a.loc_start b.loc_start in
      if c <> 0 then c else Position.compare a.loc_end b.loc_end
    ;;

    let non_empty { loc_start; loc_end; _ } = Position.compare loc_start loc_end < 0

    let of_warnings_loc (range : Warnings.loc) =
      let loc_start = Position.of_lexing_position range.loc_start in
      let loc_end = Position.of_lexing_position range.loc_end in
      { loc_start; loc_end }
    ;;

    let of_debuginfo dbg = Debuginfo.to_location dbg |> of_warnings_loc

    let print ppf ({ loc_start; loc_end } as t) =
      let loc_fname = fname t in
      Format.fprintf ppf "%s:" loc_fname;
      let start_col = loc_start.pos_cnum - loc_start.pos_bol in
      let end_col = loc_end.pos_cnum - loc_end.pos_bol in
      if loc_start.pos_lnum = loc_end.pos_lnum
      then
        Format.fprintf ppf "line %d, columns %d-%d" loc_start.pos_lnum start_col end_col
      else
        Format.fprintf
          ppf
          ": lines %d-%d, columns %d-%d"
          loc_start.pos_lnum
          loc_end.pos_lnum
          start_col
          end_col
    ;;
  end

  include T

  module Map = struct
    module M = Map.Make (T)
    include M
    include Make_map_sexp (M)

    let t_of_sexp data_of_sexp m = map_of_sexp t_of_sexp data_of_sexp m
    let sexp_of_t sexp_of_data m = sexp_of_map sexp_of_t sexp_of_data m
  end
end

module With_inlined_frames = struct
  type t = Simple.t list [@@deriving sexp]

  include struct
    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (fun x__022_ -> list_of_sexp Simple.t_of_sexp x__022_ : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun x__023_ -> sexp_of_list Simple.sexp_of_t x__023_ : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let compare a b = List.compare ~cmp:Simple.compare a b
  let in_file t = List.hd t
  let of_simple s = [ s ]
  let is_inlined t = List.length t > 1

  let update_in_file ~f t =
    match t with
    | [] -> []
    | x :: tl ->
      let n = f x in
      if n = x then t else n :: tl
  ;;

  let get_first_declaration t =
    let rec aux l =
      match l with
      | x :: [] -> x
      | _ :: tl -> aux tl
      | [] -> assert false
    in
    aux t
  ;;

  let is_inlined_within_file t =
    is_inlined t
    && String.equal (Simple.fname (get_first_declaration t)) (Simple.fname (in_file t))
  ;;

  let for_testing t = t

  let of_debuginfo' dbg =
    List.map
      ~f:
        (fun Debuginfo.
               { dinfo_file
               ; dinfo_line
               ; dinfo_char_start
               ; dinfo_char_end
               ; dinfo_start_bol
               ; dinfo_end_bol
               ; dinfo_end_line
               ; _
               } ->
        let loc_start =
          let open Position in
          { pos_cnum = dinfo_char_start + dinfo_start_bol
          ; pos_bol = dinfo_start_bol
          ; pos_lnum = dinfo_line
          ; pos_fname = dinfo_file
          }
        in
        let loc_end =
          let open Position in
          { pos_cnum = dinfo_char_end + dinfo_start_bol
          ; pos_bol = dinfo_end_bol
          ; pos_lnum = dinfo_end_line
          ; pos_fname = dinfo_file
          }
        in
        let open Simple in
        { loc_start; loc_end })
      dbg
  ;;

  let of_debuginfo dbg = of_debuginfo' (Debuginfo.Dbg.to_list (Debuginfo.get_dbg dbg))

  let print ppf t =
    List.iteri
      ~f:(fun i loc ->
        if i > 0 then Format.fprintf ppf " > ";
        Format.fprintf ppf "[%a]" Simple.print loc)
      t
  ;;

  let to_string t =
    ignore (Format.flush_str_formatter () : string);
    print Format.str_formatter t;
    Format.flush_str_formatter ()
  ;;
end

module Pattern = struct
  type atom =
    { path : string
    ; start_line : int option
    ; end_line : int option
    }

  type t = atom list

  let parse_atom s =
    let parts = String.split_on_char ~sep:':' s in
    match parts with
    | path :: [] -> Some { path; start_line = None; end_line = None }
    | [ path; lines ] ->
      let parts = String.split_on_char ~sep:'-' lines in
      let parts = List.map ~f:int_of_string_opt parts in
      (match parts with
       | (Some _ as start_line) :: [] -> Some { path; start_line; end_line = None }
       | [ (Some _ as start_line); (Some _ as end_line) ] ->
         Some { path; start_line; end_line }
       | _ -> None)
    | _ -> None
  ;;

  let of_string s =
    s
    |> String.split_on_char ~sep:';'
    |> List.map ~f:parse_atom
    |> List.fold_left ~init:(Some []) ~f:(fun acc x ->
         match acc, x with
         | Some a, Some b -> Some (b :: a)
         | _ -> None)
    |> Option.map ~f:List.rev
  ;;

  let atom_matches (loc : Simple.t) (atom : atom) =
    let line_matches =
      match atom.start_line, atom.end_line with
      | Some start_line, Some end_line ->
        start_line <= loc.loc_start.pos_lnum && loc.loc_end.pos_lnum <= end_line
      | Some start_line, None ->
        start_line = loc.loc_start.pos_lnum && loc.loc_end.pos_lnum = start_line
      | None, None -> true
      | None, Some _ -> false
    in
    String.is_substring ~substring:atom.path (Simple.fname loc) && line_matches
  ;;

  let matches loc_with_inlining pattern =
    let rec aux locs atoms =
      match locs, atoms with
      | _, [] -> true
      | [], _ -> false
      | loc :: tl_locs, atom :: tl_atoms when atom_matches loc atom ->
        aux tl_locs tl_atoms
      | _ :: tl_locs, atoms -> aux tl_locs atoms
    in
    aux loc_with_inlining pattern
  ;;

  let print ppf t =
    List.map t ~f:(function
      | { path; start_line = Some s; end_line = Some e } ->
        path ^ ":" ^ string_of_int s ^ "-" ^ string_of_int e
      | { path; start_line = Some s; end_line = None } -> path ^ ":" ^ string_of_int s
      | { path; _ } -> path)
    |> String.concat ~sep:";"
    |> Format.fprintf ppf "%s"
  ;;
end

module Inferred = struct
  type precision =
    | Exact
    | Approximate
  [@@deriving sexp, compare]

  include struct
    let _ = fun (_ : precision) -> ()

    let precision_of_sexp =
      (let error_source__026_ = "location.ml.Inferred.precision" in
       function
       | Sexplib0.Sexp.Atom ("exact" | "Exact") -> Exact
       | Sexplib0.Sexp.Atom ("approximate" | "Approximate") -> Approximate
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom ("exact" | "Exact" | "approximate" | "Approximate") :: _)
         as sexp__027_ ->
         Sexplib0.Sexp_conv_error.stag_no_args error_source__026_ sexp__027_
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__025_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__026_ sexp__025_
       | Sexplib0.Sexp.List [] as sexp__025_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__026_ sexp__025_
       | sexp__025_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag
           error_source__026_
           [ "Exact"; "Approximate" ]
           sexp__025_
        : Sexplib0.Sexp.t -> precision)
    ;;

    let _ = precision_of_sexp

    let sexp_of_precision =
      (function
       | Exact -> Sexplib0.Sexp.Atom "Exact"
       | Approximate -> Sexplib0.Sexp.Atom "Approximate"
        : precision -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_precision

    let compare_precision =
      (Stdlib.compare : precision -> (precision[@merlin.hide]) -> int)
    ;;

    let _ = compare_precision
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  type t =
    { precision : precision
    ; loc : With_inlined_frames.t
    }
  [@@deriving sexp, compare]

  include struct
    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let error_source__031_ = "location.ml.Inferred.t" in
       fun x__032_ ->
         Sexplib0.Sexp_conv_record.record_of_sexp
           ~caller:error_source__031_
           ~fields:
             (Field
                { name = "precision"
                ; kind = Required
                ; conv = precision_of_sexp
                ; rest =
                    Field
                      { name = "loc"
                      ; kind = Required
                      ; conv = With_inlined_frames.t_of_sexp
                      ; rest = Empty
                      }
                })
           ~index_of_field:
             (function
              | "precision" -> 0
              | "loc" -> 1
              | _ -> -1)
           ~allow_extra_fields:false
           ~create:(fun (precision, (loc, ())) : t -> { precision; loc })
           x__032_
        : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (fun { precision = precision__034_; loc = loc__036_ } ->
         let bnds__033_ = ([] : _ Stdlib.List.t) in
         let bnds__033_ =
           let arg__037_ = With_inlined_frames.sexp_of_t loc__036_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg__037_ ] :: bnds__033_
             : _ Stdlib.List.t)
         in
         let bnds__033_ =
           let arg__035_ = sexp_of_precision precision__034_ in
           (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "precision"; arg__035_ ] :: bnds__033_
             : _ Stdlib.List.t)
         in
         Sexplib0.Sexp.List bnds__033_
        : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    let compare =
      (fun a__038_ b__039_ ->
         if Stdlib.( == ) a__038_ b__039_
         then 0
         else (
           match compare_precision a__038_.precision b__039_.precision with
           | 0 -> With_inlined_frames.compare a__038_.loc b__039_.loc
           | n -> n)
        : t -> (t[@merlin.hide]) -> int)
    ;;

    let _ = compare
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let extract_byte_offset_within_file { loc; _ } =
    let Simple.
          { loc_start = { pos_cnum = start_cnum; _ }
          ; loc_end = { pos_cnum = end_cnum; _ }
          ; _
          }
      =
      With_inlined_frames.in_file loc
    in
    start_cnum, end_cnum
  ;;

  let extract_filename { loc; _ } = With_inlined_frames.in_file loc |> Simple.fname
  let extract { loc; _ } = With_inlined_frames.in_file loc

  module For_testing = struct
    let create pos_fname loc_start loc_end =
      let pos_cnum, pos_bol, pos_lnum = loc_start in
      let loc_start =
        let open Position in
        { pos_cnum; pos_bol; pos_lnum; pos_fname }
      in
      let pos_cnum, pos_bol, pos_lnum = loc_end in
      let loc_end =
        let open Position in
        { pos_cnum; pos_bol; pos_lnum; pos_fname }
      in
      { precision = Exact
      ; loc =
          With_inlined_frames.of_simple
            (let open Simple in
            { loc_start; loc_end })
      }
    ;;
  end
end
