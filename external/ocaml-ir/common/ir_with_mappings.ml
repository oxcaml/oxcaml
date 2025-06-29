open Ocaml_ir_fiber
open Sexplib.Std

module Raw_or_file = struct
  type 'a t =
    | Raw of 'a
    | File of string
  [@@deriving sexp]

  include struct
    let _ = fun (_ : 'a t) -> ()

    let t_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a t =
      fun (type a__016_) : ((Sexplib0.Sexp.t -> a__016_) -> Sexplib0.Sexp.t -> a__016_ t) ->
       let error_source__004_ = "ir_with_mappings.ml.Raw_or_file.t" in
       fun _of_a__001_ -> function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom (("raw" | "Raw") as _tag__007_) :: sexp_args__008_) as
           _sexp__006_ ->
           (match sexp_args__008_ with
            | arg0__009_ :: [] ->
              let res0__010_ = _of_a__001_ arg0__009_ in
              Raw res0__010_
            | _ ->
              Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                error_source__004_
                _tag__007_
                _sexp__006_)
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom (("file" | "File") as _tag__012_) :: sexp_args__013_) as
           _sexp__011_ ->
           (match sexp_args__013_ with
            | arg0__014_ :: [] ->
              let res0__015_ = string_of_sexp arg0__014_ in
              File res0__015_
            | _ ->
              Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                error_source__004_
                _tag__012_
                _sexp__011_)
         | Sexplib0.Sexp.Atom ("raw" | "Raw" | "file" | "File") as sexp__005_ ->
           Sexplib0.Sexp_conv_error.stag_takes_args error_source__004_ sexp__005_
         | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__003_ ->
           Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__004_ sexp__003_
         | Sexplib0.Sexp.List [] as sexp__003_ ->
           Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__004_ sexp__003_
         | sexp__003_ ->
           Sexplib0.Sexp_conv_error.unexpected_stag
             error_source__004_
             [ "Raw"; "File" ]
             sexp__003_
    ;;

    let _ = t_of_sexp

    let sexp_of_t : 'a. ('a -> Sexplib0.Sexp.t) -> 'a t -> Sexplib0.Sexp.t =
      fun (type a__022_) : ((a__022_ -> Sexplib0.Sexp.t) -> a__022_ t -> Sexplib0.Sexp.t) ->
       fun _of_a__017_ -> function
        | Raw arg0__018_ ->
          let res0__019_ = _of_a__017_ arg0__018_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Raw"; res0__019_ ]
        | File arg0__020_ ->
          let res0__021_ = sexp_of_string arg0__020_ in
          Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "File"; res0__021_ ]
    ;;

    let _ = sexp_of_t
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let get ~parse = function
    | Raw r -> Fiber.return r
    | File path ->
      Let_syntax.map (Fiber.Io.read_file ~path) ~f:(fun content -> parse content)
  ;;
end

module Stable = struct
  module V1 = struct
    type t =
      { ir : string Raw_or_file.t
      ; mappings : Ir_map.raw Raw_or_file.t option
      }
    [@@deriving sexp]

    include struct
      let _ = fun (_ : t) -> ()

      let t_of_sexp =
        (let error_source__025_ = "ir_with_mappings.ml.Stable.V1.t" in
         fun x__026_ ->
           Sexplib0.Sexp_conv_record.record_of_sexp
             ~caller:error_source__025_
             ~fields:
               (Field
                  { name = "ir"
                  ; kind = Required
                  ; conv = Raw_or_file.t_of_sexp string_of_sexp
                  ; rest =
                      Field
                        { name = "mappings"
                        ; kind = Required
                        ; conv = option_of_sexp (Raw_or_file.t_of_sexp Ir_map.raw_of_sexp)
                        ; rest = Empty
                        }
                  })
             ~index_of_field:
               (function
                | "ir" -> 0
                | "mappings" -> 1
                | _ -> -1)
             ~allow_extra_fields:false
             ~create:(fun (ir, (mappings, ())) : t -> { ir; mappings })
             x__026_
          : Sexplib0.Sexp.t -> t)
      ;;

      let _ = t_of_sexp

      let sexp_of_t =
        (fun { ir = ir__028_; mappings = mappings__030_ } ->
           let bnds__027_ = ([] : _ Stdlib.List.t) in
           let bnds__027_ =
             let arg__031_ =
               sexp_of_option (Raw_or_file.sexp_of_t Ir_map.sexp_of_raw) mappings__030_
             in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "mappings"; arg__031_ ]
              :: bnds__027_
               : _ Stdlib.List.t)
           in
           let bnds__027_ =
             let arg__029_ = Raw_or_file.sexp_of_t sexp_of_string ir__028_ in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "ir"; arg__029_ ] :: bnds__027_
               : _ Stdlib.List.t)
           in
           Sexplib0.Sexp.List bnds__027_
          : t -> Sexplib0.Sexp.t)
      ;;

      let _ = sexp_of_t
    end [@@ocaml.doc "@inline"] [@@merlin.hide]

    let serialize t = `V1 t
  end

  module Latest = V1

  type versions = [ `V1 of V1.t ] [@@deriving sexp]

  include struct
    let _ = fun (_ : versions) -> ()

    let __versions_of_sexp__ =
      (let error_source__040_ = "ir_with_mappings.ml.Stable.versions" in
       function
       | Sexplib0.Sexp.Atom atom__033_ as _sexp__035_ ->
         (match atom__033_ with
          | "V1" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__040_ _sexp__035_
          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
       | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__033_ :: sexp_args__036_) as
         _sexp__035_ ->
         (match atom__033_ with
          | "V1" as _tag__037_ ->
            (match sexp_args__036_ with
             | arg0__038_ :: [] ->
               let res0__039_ = V1.t_of_sexp arg0__038_ in
               `V1 res0__039_
             | _ ->
               Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                 error_source__040_
                 _tag__037_
                 _sexp__035_)
          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__034_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
           error_source__040_
           sexp__034_
       | Sexplib0.Sexp.List [] as sexp__034_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
           error_source__040_
           sexp__034_
        : Sexplib0.Sexp.t -> versions)
    ;;

    let _ = __versions_of_sexp__

    let versions_of_sexp =
      (let error_source__042_ = "ir_with_mappings.ml.Stable.versions" in
       fun sexp__041_ ->
         try __versions_of_sexp__ sexp__041_ with
         | Sexplib0.Sexp_conv_error.No_variant_match ->
           Sexplib0.Sexp_conv_error.no_matching_variant_found
             error_source__042_
             sexp__041_
        : Sexplib0.Sexp.t -> versions)
    ;;

    let _ = versions_of_sexp

    let sexp_of_versions =
      (fun (`V1 v__043_) ->
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "V1"; V1.sexp_of_t v__043_ ]
        : versions -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_versions
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
end

include Stable.Latest

let t_of_sexp t =
  match Stable.versions_of_sexp t with
  | `V1 t -> t
;;

let sexp_of_t s = Stable.sexp_of_versions (serialize s)
let create ?mappings ir = { ir; mappings }
let ir { ir; _ } = Raw_or_file.get ~parse:(fun s -> s) ir

let mappings { mappings; _ } =
  match mappings with
  | Some mappings ->
    Fiber.Let_syntax.Let_syntax.map
      (Raw_or_file.get
         ~parse:(fun content ->
           Parsexp.Single.parse_string_exn content |> Ir_map.raw_of_sexp)
         mappings)
      ~f:(fun r -> Some r)
  | None -> Fiber.return None
;;
