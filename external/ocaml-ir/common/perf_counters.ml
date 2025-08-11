open! Ocaml_ir_fiber
open Std
open Sexplib.Std

module Event_type = struct
  module Stable = struct
    module V1 = struct
      type t = string [@@deriving sexp]

      include struct
        let _ = fun (_ : t) -> ()
        let t_of_sexp = (string_of_sexp : Sexplib0.Sexp.t -> t)
        let _ = t_of_sexp
        let sexp_of_t = (sexp_of_string : t -> Sexplib0.Sexp.t)
        let _ = sexp_of_t
      end [@@ocaml.doc "@inline"] [@@merlin.hide]

      module Map = String.Map
      module Set = Set.Make (String)
    end
  end

  include Stable.V1

  let compare = String.compare
  let to_string t = t
  let of_string t = t
end

module Event = struct
  module Stable = struct
    module V1 = struct
      type t =
        { line : int
        ; value : int
        ; col : int
        ; discriminator : int
        }

      let t_of_sexp (sexp : Sexplib.Sexp.t) =
        match sexp with
        | List (line :: value :: l) ->
          let line = int_of_sexp line in
          let value = int_of_sexp value in
          let col, discriminator =
            match l with
            | [] -> 0, 0
            | col :: [] -> int_of_sexp col, 0
            | [ col; discriminator ] -> int_of_sexp col, int_of_sexp discriminator
            | _ ->
              Sexplib.Conv.of_sexp_error "Event.t_of_sexp: expected at most 4 atoms" sexp
          in
          { line; value; col; discriminator }
        | _ -> Sexplib.Conv.of_sexp_error "Event.t_of_sexp" sexp
      ;;

      let sexp_of_t { line; value; col; discriminator } =
        let l =
          match col, discriminator with
          | 0, 0 -> [ line; value ]
          | _, 0 -> [ line; value; col ]
          | _, _ -> [ line; value; col; discriminator ]
        in
        sexp_of_list sexp_of_int l
      ;;
    end
  end

  include Stable.V1

  let line { line; _ } = line
  let value { value; _ } = value
  let create ~line ~col ~discriminator value = { col; line; discriminator; value }
end

module Event_types_in_project = struct
  module Stable = struct
    module V1 = struct
      type t = Event_type.Stable.V1.t list [@@deriving sexp]

      include struct
        let _ = fun (_ : t) -> ()

        let t_of_sexp =
          (fun x__003_ -> list_of_sexp Event_type.Stable.V1.t_of_sexp x__003_
            : Sexplib0.Sexp.t -> t)
        ;;

        let _ = t_of_sexp

        let sexp_of_t =
          (fun x__004_ -> sexp_of_list Event_type.Stable.V1.sexp_of_t x__004_
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
        (let error_source__013_ =
           "perf_counters.ml.Event_types_in_project.Stable.versions"
         in
         function
         | Sexplib0.Sexp.Atom atom__006_ as _sexp__008_ ->
           (match atom__006_ with
            | "V1" ->
              Sexplib0.Sexp_conv_error.ptag_takes_args error_source__013_ _sexp__008_
            | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
         | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__006_ :: sexp_args__009_) as
           _sexp__008_ ->
           (match atom__006_ with
            | "V1" as _tag__010_ ->
              (match sexp_args__009_ with
               | arg0__011_ :: [] ->
                 let res0__012_ = V1.t_of_sexp arg0__011_ in
                 `V1 res0__012_
               | _ ->
                 Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                   error_source__013_
                   _tag__010_
                   _sexp__008_)
            | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
         | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__007_ ->
           Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
             error_source__013_
             sexp__007_
         | Sexplib0.Sexp.List [] as sexp__007_ ->
           Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
             error_source__013_
             sexp__007_
          : Sexplib0.Sexp.t -> versions)
      ;;

      let _ = __versions_of_sexp__

      let versions_of_sexp =
        (let error_source__015_ =
           "perf_counters.ml.Event_types_in_project.Stable.versions"
         in
         fun sexp__014_ ->
           try __versions_of_sexp__ sexp__014_ with
           | Sexplib0.Sexp_conv_error.No_variant_match ->
             Sexplib0.Sexp_conv_error.no_matching_variant_found
               error_source__015_
               sexp__014_
          : Sexplib0.Sexp.t -> versions)
      ;;

      let _ = versions_of_sexp

      let sexp_of_versions =
        (fun (`V1 v__016_) ->
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "V1"; V1.sexp_of_t v__016_ ]
          : versions -> Sexplib0.Sexp.t)
      ;;

      let _ = sexp_of_versions
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end

  include Stable.Latest

  let to_list t = t
  let of_list t = t

  let t_of_sexp t =
    match Stable.versions_of_sexp t with
    | `V1 t -> t
  ;;

  let sexp_of_t s = Stable.sexp_of_versions (serialize s)
end

module Events_for_a_file = struct
  module Stable = struct
    module V1 = struct
      type t =
        { values : Event.Stable.V1.t list
        ; filename : string
        ; event : Event_type.Stable.V1.t
        ; total_in_file : int
        ; total_in_project : int
        }
      [@@deriving sexp]

      include struct
        let _ = fun (_ : t) -> ()

        let t_of_sexp =
          (let error_source__018_ = "perf_counters.ml.Events_for_a_file.Stable.V1.t" in
           fun x__019_ ->
             Sexplib0.Sexp_conv_record.record_of_sexp
               ~caller:error_source__018_
               ~fields:
                 (Field
                    { name = "values"
                    ; kind = Required
                    ; conv = list_of_sexp Event.Stable.V1.t_of_sexp
                    ; rest =
                        Field
                          { name = "filename"
                          ; kind = Required
                          ; conv = string_of_sexp
                          ; rest =
                              Field
                                { name = "event"
                                ; kind = Required
                                ; conv = Event_type.Stable.V1.t_of_sexp
                                ; rest =
                                    Field
                                      { name = "total_in_file"
                                      ; kind = Required
                                      ; conv = int_of_sexp
                                      ; rest =
                                          Field
                                            { name = "total_in_project"
                                            ; kind = Required
                                            ; conv = int_of_sexp
                                            ; rest = Empty
                                            }
                                      }
                                }
                          }
                    })
               ~index_of_field:
                 (function
                  | "values" -> 0
                  | "filename" -> 1
                  | "event" -> 2
                  | "total_in_file" -> 3
                  | "total_in_project" -> 4
                  | _ -> -1)
               ~allow_extra_fields:false
               ~create:
                 (fun ( values
                      , (filename, (event, (total_in_file, (total_in_project, ())))) )
                    : t -> { values; filename; event; total_in_file; total_in_project })
               x__019_
            : Sexplib0.Sexp.t -> t)
        ;;

        let _ = t_of_sexp

        let sexp_of_t =
          (fun { values = values__021_
               ; filename = filename__023_
               ; event = event__025_
               ; total_in_file = total_in_file__027_
               ; total_in_project = total_in_project__029_
               } ->
             let bnds__020_ = ([] : _ Stdlib.List.t) in
             let bnds__020_ =
               let arg__030_ = sexp_of_int total_in_project__029_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "total_in_project"; arg__030_ ]
                :: bnds__020_
                 : _ Stdlib.List.t)
             in
             let bnds__020_ =
               let arg__028_ = sexp_of_int total_in_file__027_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "total_in_file"; arg__028_ ]
                :: bnds__020_
                 : _ Stdlib.List.t)
             in
             let bnds__020_ =
               let arg__026_ = Event_type.Stable.V1.sexp_of_t event__025_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "event"; arg__026_ ] :: bnds__020_
                 : _ Stdlib.List.t)
             in
             let bnds__020_ =
               let arg__024_ = sexp_of_string filename__023_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "filename"; arg__024_ ]
                :: bnds__020_
                 : _ Stdlib.List.t)
             in
             let bnds__020_ =
               let arg__022_ = sexp_of_list Event.Stable.V1.sexp_of_t values__021_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "values"; arg__022_ ]
                :: bnds__020_
                 : _ Stdlib.List.t)
             in
             Sexplib0.Sexp.List bnds__020_
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
        (let error_source__039_ = "perf_counters.ml.Events_for_a_file.Stable.versions" in
         function
         | Sexplib0.Sexp.Atom atom__032_ as _sexp__034_ ->
           (match atom__032_ with
            | "V1" ->
              Sexplib0.Sexp_conv_error.ptag_takes_args error_source__039_ _sexp__034_
            | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
         | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__032_ :: sexp_args__035_) as
           _sexp__034_ ->
           (match atom__032_ with
            | "V1" as _tag__036_ ->
              (match sexp_args__035_ with
               | arg0__037_ :: [] ->
                 let res0__038_ = V1.t_of_sexp arg0__037_ in
                 `V1 res0__038_
               | _ ->
                 Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                   error_source__039_
                   _tag__036_
                   _sexp__034_)
            | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
         | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__033_ ->
           Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
             error_source__039_
             sexp__033_
         | Sexplib0.Sexp.List [] as sexp__033_ ->
           Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
             error_source__039_
             sexp__033_
          : Sexplib0.Sexp.t -> versions)
      ;;

      let _ = __versions_of_sexp__

      let versions_of_sexp =
        (let error_source__041_ = "perf_counters.ml.Events_for_a_file.Stable.versions" in
         fun sexp__040_ ->
           try __versions_of_sexp__ sexp__040_ with
           | Sexplib0.Sexp_conv_error.No_variant_match ->
             Sexplib0.Sexp_conv_error.no_matching_variant_found
               error_source__041_
               sexp__040_
          : Sexplib0.Sexp.t -> versions)
      ;;

      let _ = versions_of_sexp

      let sexp_of_versions =
        (fun (`V1 v__042_) ->
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "V1"; V1.sexp_of_t v__042_ ]
          : versions -> Sexplib0.Sexp.t)
      ;;

      let _ = sexp_of_versions
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end

  include Stable.Latest

  let print formatter { values; filename; event = _; total_in_file; total_in_project } =
    let filename = Filename.basename filename in
    let file_proportion = Float.of_int total_in_file /. Float.of_int total_in_project in
    Format.fprintf
      formatter
      "%s made up %.2f%% of all samples in this perf profile\n"
      filename
      (file_proportion *. 100.);
    if List.length values > 0
    then (
      let f key x =
        Format.fprintf
          formatter
          "%s:line %d occured in %.2f%% of samples within this file\n"
          filename
          key
          (Float.of_int x *. 100. /. Float.of_int total_in_file)
      in
      let m =
        List.fold_left
          ~init:Int.Map.empty
          ~f:(fun acc event ->
            Int.Map.update
              (Event.line event)
              (function
               | None -> Some (Event.value event)
               | Some v -> Some (v + Event.value event))
              acc)
          values
      in
      Int.Map.iter f m)
  ;;

  let t_of_sexp t =
    match Stable.versions_of_sexp t with
    | `V1 t -> t
  ;;

  let sexp_of_t s = Stable.sexp_of_versions (serialize s)

  let create ~values ~filename ~event ~total_in_file ~total_in_project =
    { values; filename; event; total_in_file; total_in_project }
  ;;
end

module Event_count_per_files = struct
  module Stable = struct
    module V1 = struct
      type t = (string * int) list [@@deriving sexp]

      include struct
        let _ = fun (_ : t) -> ()

        let t_of_sexp =
          (let error_source__049_ =
             "perf_counters.ml.Event_count_per_files.Stable.V1.t"
           in
           fun x__050_ ->
             list_of_sexp
               (function
                | Sexplib0.Sexp.List [ arg0__044_; arg1__045_ ] ->
                  let res0__046_ = string_of_sexp arg0__044_
                  and res1__047_ = int_of_sexp arg1__045_ in
                  res0__046_, res1__047_
                | sexp__048_ ->
                  Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                    error_source__049_
                    2
                    sexp__048_)
               x__050_
            : Sexplib0.Sexp.t -> t)
        ;;

        let _ = t_of_sexp

        let sexp_of_t =
          (fun x__055_ ->
             sexp_of_list
               (fun (arg0__051_, arg1__052_) ->
                 let res0__053_ = sexp_of_string arg0__051_
                 and res1__054_ = sexp_of_int arg1__052_ in
                 Sexplib0.Sexp.List [ res0__053_; res1__054_ ])
               x__055_
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
        (let error_source__064_ =
           "perf_counters.ml.Event_count_per_files.Stable.versions"
         in
         function
         | Sexplib0.Sexp.Atom atom__057_ as _sexp__059_ ->
           (match atom__057_ with
            | "V1" ->
              Sexplib0.Sexp_conv_error.ptag_takes_args error_source__064_ _sexp__059_
            | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
         | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__057_ :: sexp_args__060_) as
           _sexp__059_ ->
           (match atom__057_ with
            | "V1" as _tag__061_ ->
              (match sexp_args__060_ with
               | arg0__062_ :: [] ->
                 let res0__063_ = V1.t_of_sexp arg0__062_ in
                 `V1 res0__063_
               | _ ->
                 Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                   error_source__064_
                   _tag__061_
                   _sexp__059_)
            | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
         | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__058_ ->
           Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
             error_source__064_
             sexp__058_
         | Sexplib0.Sexp.List [] as sexp__058_ ->
           Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
             error_source__064_
             sexp__058_
          : Sexplib0.Sexp.t -> versions)
      ;;

      let _ = __versions_of_sexp__

      let versions_of_sexp =
        (let error_source__066_ =
           "perf_counters.ml.Event_count_per_files.Stable.versions"
         in
         fun sexp__065_ ->
           try __versions_of_sexp__ sexp__065_ with
           | Sexplib0.Sexp_conv_error.No_variant_match ->
             Sexplib0.Sexp_conv_error.no_matching_variant_found
               error_source__066_
               sexp__065_
          : Sexplib0.Sexp.t -> versions)
      ;;

      let _ = versions_of_sexp

      let sexp_of_versions =
        (fun (`V1 v__067_) ->
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "V1"; V1.sexp_of_t v__067_ ]
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
  let of_list t = t
  let to_list t = t
end
