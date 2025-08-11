open Sexplib.Std
open Std

module Stable = struct
  module V1 = struct
    module Allocation = struct
      type kind =
        | Ccall of string
        | Ocaml
      [@@deriving sexp]

      include struct
        let _ = fun (_ : kind) -> ()

        let kind_of_sexp =
          (let error_source__003_ = "allocations.ml.Stable.V1.Allocation.kind" in
           function
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom (("ccall" | "Ccall") as _tag__006_) :: sexp_args__007_)
             as _sexp__005_ ->
             (match sexp_args__007_ with
              | arg0__008_ :: [] ->
                let res0__009_ = string_of_sexp arg0__008_ in
                Ccall res0__009_
              | _ ->
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  error_source__003_
                  _tag__006_
                  _sexp__005_)
           | Sexplib0.Sexp.Atom ("ocaml" | "Ocaml") -> Ocaml
           | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("ocaml" | "Ocaml") :: _) as
             sexp__004_ ->
             Sexplib0.Sexp_conv_error.stag_no_args error_source__003_ sexp__004_
           | Sexplib0.Sexp.Atom ("ccall" | "Ccall") as sexp__004_ ->
             Sexplib0.Sexp_conv_error.stag_takes_args error_source__003_ sexp__004_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__002_ ->
             Sexplib0.Sexp_conv_error.nested_list_invalid_sum
               error_source__003_
               sexp__002_
           | Sexplib0.Sexp.List [] as sexp__002_ ->
             Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__003_ sexp__002_
           | sexp__002_ ->
             Sexplib0.Sexp_conv_error.unexpected_stag
               error_source__003_
               [ "Ccall"; "Ocaml" ]
               sexp__002_
            : Sexplib0.Sexp.t -> kind)
        ;;

        let _ = kind_of_sexp

        let sexp_of_kind =
          (function
           | Ccall arg0__010_ ->
             let res0__011_ = sexp_of_string arg0__010_ in
             Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Ccall"; res0__011_ ]
           | Ocaml -> Sexplib0.Sexp.Atom "Ocaml"
            : kind -> Sexplib0.Sexp.t)
        ;;

        let _ = sexp_of_kind
      end [@@ocaml.doc "@inline"] [@@merlin.hide]

      type context =
        | Raise
        | Direct
        | Catch
      [@@deriving sexp]

      include struct
        let _ = fun (_ : context) -> ()

        let context_of_sexp =
          (let error_source__014_ = "allocations.ml.Stable.V1.Allocation.context" in
           function
           | Sexplib0.Sexp.Atom ("raise" | "Raise") -> Raise
           | Sexplib0.Sexp.Atom ("direct" | "Direct") -> Direct
           | Sexplib0.Sexp.Atom ("catch" | "Catch") -> Catch
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom
                  ("raise" | "Raise" | "direct" | "Direct" | "catch" | "Catch")
               :: _) as sexp__015_ ->
             Sexplib0.Sexp_conv_error.stag_no_args error_source__014_ sexp__015_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__013_ ->
             Sexplib0.Sexp_conv_error.nested_list_invalid_sum
               error_source__014_
               sexp__013_
           | Sexplib0.Sexp.List [] as sexp__013_ ->
             Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__014_ sexp__013_
           | sexp__013_ ->
             Sexplib0.Sexp_conv_error.unexpected_stag
               error_source__014_
               [ "Raise"; "Direct"; "Catch" ]
               sexp__013_
            : Sexplib0.Sexp.t -> context)
        ;;

        let _ = context_of_sexp

        let sexp_of_context =
          (function
           | Raise -> Sexplib0.Sexp.Atom "Raise"
           | Direct -> Sexplib0.Sexp.Atom "Direct"
           | Catch -> Sexplib0.Sexp.Atom "Catch"
            : context -> Sexplib0.Sexp.t)
        ;;

        let _ = sexp_of_context
      end [@@ocaml.doc "@inline"] [@@merlin.hide]

      type alloc =
        { kind : kind
        ; context : context
        }
      [@@deriving sexp]

      include struct
        let _ = fun (_ : alloc) -> ()

        let alloc_of_sexp =
          (let error_source__017_ = "allocations.ml.Stable.V1.Allocation.alloc" in
           fun x__018_ ->
             Sexplib0.Sexp_conv_record.record_of_sexp
               ~caller:error_source__017_
               ~fields:
                 (Field
                    { name = "kind"
                    ; kind = Required
                    ; conv = kind_of_sexp
                    ; rest =
                        Field
                          { name = "context"
                          ; kind = Required
                          ; conv = context_of_sexp
                          ; rest = Empty
                          }
                    })
               ~index_of_field:
                 (function
                  | "kind" -> 0
                  | "context" -> 1
                  | _ -> -1)
               ~allow_extra_fields:false
               ~create:(fun (kind, (context, ())) : alloc -> { kind; context })
               x__018_
            : Sexplib0.Sexp.t -> alloc)
        ;;

        let _ = alloc_of_sexp

        let sexp_of_alloc =
          (fun { kind = kind__020_; context = context__022_ } ->
             let bnds__019_ = ([] : _ Stdlib.List.t) in
             let bnds__019_ =
               let arg__023_ = sexp_of_context context__022_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "context"; arg__023_ ]
                :: bnds__019_
                 : _ Stdlib.List.t)
             in
             let bnds__019_ =
               let arg__021_ = sexp_of_kind kind__020_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "kind"; arg__021_ ] :: bnds__019_
                 : _ Stdlib.List.t)
             in
             Sexplib0.Sexp.List bnds__019_
            : alloc -> Sexplib0.Sexp.t)
        ;;

        let _ = sexp_of_alloc
      end [@@ocaml.doc "@inline"] [@@merlin.hide]

      type t = Location.Inferred.t option * alloc [@@deriving sexp]

      include struct
        let _ = fun (_ : t) -> ()

        let t_of_sexp =
          (let error_source__030_ = "allocations.ml.Stable.V1.Allocation.t" in
           function
           | Sexplib0.Sexp.List [ arg0__025_; arg1__026_ ] ->
             let res0__027_ = option_of_sexp Location.Inferred.t_of_sexp arg0__025_
             and res1__028_ = alloc_of_sexp arg1__026_ in
             res0__027_, res1__028_
           | sexp__029_ ->
             Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
               error_source__030_
               2
               sexp__029_
            : Sexplib0.Sexp.t -> t)
        ;;

        let _ = t_of_sexp

        let sexp_of_t =
          (fun (arg0__031_, arg1__032_) ->
             let res0__033_ = sexp_of_option Location.Inferred.sexp_of_t arg0__031_
             and res1__034_ = sexp_of_alloc arg1__032_ in
             Sexplib0.Sexp.List [ res0__033_; res1__034_ ]
            : t -> Sexplib0.Sexp.t)
        ;;

        let _ = sexp_of_t
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    end

    type one_function =
      { function_name : string
      ; allocations : Allocation.t list
      }
    [@@deriving sexp]

    include struct
      let _ = fun (_ : one_function) -> ()

      let one_function_of_sexp =
        (let error_source__036_ = "allocations.ml.Stable.V1.one_function" in
         fun x__037_ ->
           Sexplib0.Sexp_conv_record.record_of_sexp
             ~caller:error_source__036_
             ~fields:
               (Field
                  { name = "function_name"
                  ; kind = Required
                  ; conv = string_of_sexp
                  ; rest =
                      Field
                        { name = "allocations"
                        ; kind = Required
                        ; conv = list_of_sexp Allocation.t_of_sexp
                        ; rest = Empty
                        }
                  })
             ~index_of_field:
               (function
                | "function_name" -> 0
                | "allocations" -> 1
                | _ -> -1)
             ~allow_extra_fields:false
             ~create:(fun (function_name, (allocations, ())) : one_function ->
               { function_name; allocations })
             x__037_
          : Sexplib0.Sexp.t -> one_function)
      ;;

      let _ = one_function_of_sexp

      let sexp_of_one_function =
        (fun { function_name = function_name__039_; allocations = allocations__041_ } ->
           let bnds__038_ = ([] : _ Stdlib.List.t) in
           let bnds__038_ =
             let arg__042_ = sexp_of_list Allocation.sexp_of_t allocations__041_ in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "allocations"; arg__042_ ]
              :: bnds__038_
               : _ Stdlib.List.t)
           in
           let bnds__038_ =
             let arg__040_ = sexp_of_string function_name__039_ in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "function_name"; arg__040_ ]
              :: bnds__038_
               : _ Stdlib.List.t)
           in
           Sexplib0.Sexp.List bnds__038_
          : one_function -> Sexplib0.Sexp.t)
      ;;

      let _ = sexp_of_one_function
    end [@@ocaml.doc "@inline"] [@@merlin.hide]

    type t = one_function list [@@deriving sexp]

    include struct
      let _ = fun (_ : t) -> ()

      let t_of_sexp =
        (fun x__044_ -> list_of_sexp one_function_of_sexp x__044_ : Sexplib0.Sexp.t -> t)
      ;;

      let _ = t_of_sexp

      let sexp_of_t =
        (fun x__045_ -> sexp_of_list sexp_of_one_function x__045_ : t -> Sexplib0.Sexp.t)
      ;;

      let _ = sexp_of_t
    end [@@ocaml.doc "@inline"] [@@merlin.hide]

    let serialize t = `V1 t
  end

  module V2 = struct
    module Allocation = struct
      module T = struct
        let compare_string = String.compare

        type kind = V1.Allocation.kind =
          | Ccall of string
          | Ocaml
        [@@deriving sexp, compare]

        include struct
          let _ = fun (_ : kind) -> ()

          let kind_of_sexp =
            (let error_source__048_ = "allocations.ml.Stable.V2.Allocation.T.kind" in
             function
             | Sexplib0.Sexp.List
                 (Sexplib0.Sexp.Atom (("ccall" | "Ccall") as _tag__051_)
                 :: sexp_args__052_) as _sexp__050_ ->
               (match sexp_args__052_ with
                | arg0__053_ :: [] ->
                  let res0__054_ = string_of_sexp arg0__053_ in
                  Ccall res0__054_
                | _ ->
                  Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                    error_source__048_
                    _tag__051_
                    _sexp__050_)
             | Sexplib0.Sexp.Atom ("ocaml" | "Ocaml") -> Ocaml
             | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("ocaml" | "Ocaml") :: _) as
               sexp__049_ ->
               Sexplib0.Sexp_conv_error.stag_no_args error_source__048_ sexp__049_
             | Sexplib0.Sexp.Atom ("ccall" | "Ccall") as sexp__049_ ->
               Sexplib0.Sexp_conv_error.stag_takes_args error_source__048_ sexp__049_
             | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__047_ ->
               Sexplib0.Sexp_conv_error.nested_list_invalid_sum
                 error_source__048_
                 sexp__047_
             | Sexplib0.Sexp.List [] as sexp__047_ ->
               Sexplib0.Sexp_conv_error.empty_list_invalid_sum
                 error_source__048_
                 sexp__047_
             | sexp__047_ ->
               Sexplib0.Sexp_conv_error.unexpected_stag
                 error_source__048_
                 [ "Ccall"; "Ocaml" ]
                 sexp__047_
              : Sexplib0.Sexp.t -> kind)
          ;;

          let _ = kind_of_sexp

          let sexp_of_kind =
            (function
             | Ccall arg0__055_ ->
               let res0__056_ = sexp_of_string arg0__055_ in
               Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Ccall"; res0__056_ ]
             | Ocaml -> Sexplib0.Sexp.Atom "Ocaml"
              : kind -> Sexplib0.Sexp.t)
          ;;

          let _ = sexp_of_kind

          let compare_kind =
            (fun a__057_ b__058_ ->
               if Stdlib.( == ) a__057_ b__058_
               then 0
               else (
                 match a__057_, b__058_ with
                 | Ccall _a__059_, Ccall _b__060_ -> compare_string _a__059_ _b__060_
                 | Ccall _, _ -> -1
                 | _, Ccall _ -> 1
                 | Ocaml, Ocaml -> 0)
              : kind -> (kind[@merlin.hide]) -> int)
          ;;

          let _ = compare_kind
        end [@@ocaml.doc "@inline"] [@@merlin.hide]

        let kind_of_sexp s =
          try kind_of_sexp s with
          | _ -> Ocaml
        ;;

        type context = V1.Allocation.context =
          | Raise
          | Direct
          | Catch
        [@@deriving sexp, compare]

        include struct
          let _ = fun (_ : context) -> ()

          let context_of_sexp =
            (let error_source__063_ = "allocations.ml.Stable.V2.Allocation.T.context" in
             function
             | Sexplib0.Sexp.Atom ("raise" | "Raise") -> Raise
             | Sexplib0.Sexp.Atom ("direct" | "Direct") -> Direct
             | Sexplib0.Sexp.Atom ("catch" | "Catch") -> Catch
             | Sexplib0.Sexp.List
                 (Sexplib0.Sexp.Atom
                    ("raise" | "Raise" | "direct" | "Direct" | "catch" | "Catch")
                 :: _) as sexp__064_ ->
               Sexplib0.Sexp_conv_error.stag_no_args error_source__063_ sexp__064_
             | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__062_ ->
               Sexplib0.Sexp_conv_error.nested_list_invalid_sum
                 error_source__063_
                 sexp__062_
             | Sexplib0.Sexp.List [] as sexp__062_ ->
               Sexplib0.Sexp_conv_error.empty_list_invalid_sum
                 error_source__063_
                 sexp__062_
             | sexp__062_ ->
               Sexplib0.Sexp_conv_error.unexpected_stag
                 error_source__063_
                 [ "Raise"; "Direct"; "Catch" ]
                 sexp__062_
              : Sexplib0.Sexp.t -> context)
          ;;

          let _ = context_of_sexp

          let sexp_of_context =
            (function
             | Raise -> Sexplib0.Sexp.Atom "Raise"
             | Direct -> Sexplib0.Sexp.Atom "Direct"
             | Catch -> Sexplib0.Sexp.Atom "Catch"
              : context -> Sexplib0.Sexp.t)
          ;;

          let _ = sexp_of_context

          let compare_context =
            (Stdlib.compare : context -> (context[@merlin.hide]) -> int)
          ;;

          let _ = compare_context
        end [@@ocaml.doc "@inline"] [@@merlin.hide]

        let context_of_sexp s =
          try context_of_sexp s with
          | _ -> Direct
        ;;

        type locality =
          | Global
          | Local
        [@@deriving sexp, compare]

        include struct
          let _ = fun (_ : locality) -> ()

          let locality_of_sexp =
            (let error_source__069_ = "allocations.ml.Stable.V2.Allocation.T.locality" in
             function
             | Sexplib0.Sexp.Atom ("global" | "Global") -> Global
             | Sexplib0.Sexp.Atom ("local" | "Local") -> Local
             | Sexplib0.Sexp.List
                 (Sexplib0.Sexp.Atom ("global" | "Global" | "local" | "Local") :: _) as
               sexp__070_ ->
               Sexplib0.Sexp_conv_error.stag_no_args error_source__069_ sexp__070_
             | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__068_ ->
               Sexplib0.Sexp_conv_error.nested_list_invalid_sum
                 error_source__069_
                 sexp__068_
             | Sexplib0.Sexp.List [] as sexp__068_ ->
               Sexplib0.Sexp_conv_error.empty_list_invalid_sum
                 error_source__069_
                 sexp__068_
             | sexp__068_ ->
               Sexplib0.Sexp_conv_error.unexpected_stag
                 error_source__069_
                 [ "Global"; "Local" ]
                 sexp__068_
              : Sexplib0.Sexp.t -> locality)
          ;;

          let _ = locality_of_sexp

          let sexp_of_locality =
            (function
             | Global -> Sexplib0.Sexp.Atom "Global"
             | Local -> Sexplib0.Sexp.Atom "Local"
              : locality -> Sexplib0.Sexp.t)
          ;;

          let _ = sexp_of_locality

          let compare_locality =
            (Stdlib.compare : locality -> (locality[@merlin.hide]) -> int)
          ;;

          let _ = compare_locality
        end [@@ocaml.doc "@inline"] [@@merlin.hide]

        let locality_of_sexp s =
          try locality_of_sexp s with
          | _ -> Local
        ;;

        type alloc =
          { kind : kind
          ; context : context
          ; locality : locality
          }
        [@@deriving sexp, compare]

        include struct
          let _ = fun (_ : alloc) -> ()

          let alloc_of_sexp =
            (let error_source__074_ = "allocations.ml.Stable.V2.Allocation.T.alloc" in
             fun x__075_ ->
               Sexplib0.Sexp_conv_record.record_of_sexp
                 ~caller:error_source__074_
                 ~fields:
                   (Field
                      { name = "kind"
                      ; kind = Required
                      ; conv = kind_of_sexp
                      ; rest =
                          Field
                            { name = "context"
                            ; kind = Required
                            ; conv = context_of_sexp
                            ; rest =
                                Field
                                  { name = "locality"
                                  ; kind = Required
                                  ; conv = locality_of_sexp
                                  ; rest = Empty
                                  }
                            }
                      })
                 ~index_of_field:
                   (function
                    | "kind" -> 0
                    | "context" -> 1
                    | "locality" -> 2
                    | _ -> -1)
                 ~allow_extra_fields:false
                 ~create:(fun (kind, (context, (locality, ()))) : alloc ->
                   { kind; context; locality })
                 x__075_
              : Sexplib0.Sexp.t -> alloc)
          ;;

          let _ = alloc_of_sexp

          let sexp_of_alloc =
            (fun { kind = kind__077_; context = context__079_; locality = locality__081_ } ->
               let bnds__076_ = ([] : _ Stdlib.List.t) in
               let bnds__076_ =
                 let arg__082_ = sexp_of_locality locality__081_ in
                 (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "locality"; arg__082_ ]
                  :: bnds__076_
                   : _ Stdlib.List.t)
               in
               let bnds__076_ =
                 let arg__080_ = sexp_of_context context__079_ in
                 (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "context"; arg__080_ ]
                  :: bnds__076_
                   : _ Stdlib.List.t)
               in
               let bnds__076_ =
                 let arg__078_ = sexp_of_kind kind__077_ in
                 (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "kind"; arg__078_ ]
                  :: bnds__076_
                   : _ Stdlib.List.t)
               in
               Sexplib0.Sexp.List bnds__076_
              : alloc -> Sexplib0.Sexp.t)
          ;;

          let _ = sexp_of_alloc

          let compare_alloc =
            (fun a__083_ b__084_ ->
               if Stdlib.( == ) a__083_ b__084_
               then 0
               else (
                 match compare_kind a__083_.kind b__084_.kind with
                 | 0 ->
                   (match compare_context a__083_.context b__084_.context with
                    | 0 -> compare_locality a__083_.locality b__084_.locality
                    | n -> n)
                 | n -> n)
              : alloc -> (alloc[@merlin.hide]) -> int)
          ;;

          let _ = compare_alloc
        end [@@ocaml.doc "@inline"] [@@merlin.hide]

        type t = Location.Inferred.t option * alloc [@@deriving sexp, compare]

        include struct
          let _ = fun (_ : t) -> ()

          let t_of_sexp =
            (let error_source__091_ = "allocations.ml.Stable.V2.Allocation.T.t" in
             function
             | Sexplib0.Sexp.List [ arg0__086_; arg1__087_ ] ->
               let res0__088_ = option_of_sexp Location.Inferred.t_of_sexp arg0__086_
               and res1__089_ = alloc_of_sexp arg1__087_ in
               res0__088_, res1__089_
             | sexp__090_ ->
               Sexplib0.Sexp_conv_error.tuple_of_size_n_expected
                 error_source__091_
                 2
                 sexp__090_
              : Sexplib0.Sexp.t -> t)
          ;;

          let _ = t_of_sexp

          let sexp_of_t =
            (fun (arg0__092_, arg1__093_) ->
               let res0__094_ = sexp_of_option Location.Inferred.sexp_of_t arg0__092_
               and res1__095_ = sexp_of_alloc arg1__093_ in
               Sexplib0.Sexp.List [ res0__094_; res1__095_ ]
              : t -> Sexplib0.Sexp.t)
          ;;

          let _ = sexp_of_t

          let compare =
            (fun a__096_ b__097_ ->
               let t__098_, t__099_ = a__096_ in
               let t__100_, t__101_ = b__097_ in
               match
                 compare_option
                   (fun a__102_ (b__103_ [@merlin.hide]) ->
                     (Location.Inferred.compare a__102_ b__103_ [@merlin.hide]))
                   t__098_
                   t__100_
               with
               | 0 -> compare_alloc t__099_ t__101_
               | n -> n
              : t -> (t[@merlin.hide]) -> int)
          ;;

          let _ = compare
        end [@@ocaml.doc "@inline"] [@@merlin.hide]

        let create ~kind ~context ~locality location =
          location, { kind; context; locality }
        ;;

        let here_because_inlined (location, _) =
          match location with
          | Some Location.Inferred.{ loc; _ } ->
            Location.With_inlined_frames.is_inlined loc
          | None -> false
        ;;

        let of_v1 ((l, { context; kind }) : V1.Allocation.t) =
          l, { context; kind; locality = Global }
        ;;

        let to_v1 ((l, { context; kind; _ }) : t) : V1.Allocation.t = l, { context; kind }
      end

      module Set = Set.Make (T)
      include T
    end

    type state =
      | Deadcode_eliminated of Location.Inferred.t
      | Allocations of Allocation.t list
    [@@deriving sexp]

    include struct
      let _ = fun (_ : state) -> ()

      let state_of_sexp =
        (let error_source__106_ = "allocations.ml.Stable.V2.state" in
         function
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom
                (("deadcode_eliminated" | "Deadcode_eliminated") as _tag__109_)
             :: sexp_args__110_) as _sexp__108_ ->
           (match sexp_args__110_ with
            | arg0__111_ :: [] ->
              let res0__112_ = Location.Inferred.t_of_sexp arg0__111_ in
              Deadcode_eliminated res0__112_
            | _ ->
              Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                error_source__106_
                _tag__109_
                _sexp__108_)
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom (("allocations" | "Allocations") as _tag__114_)
             :: sexp_args__115_) as _sexp__113_ ->
           (match sexp_args__115_ with
            | arg0__116_ :: [] ->
              let res0__117_ = list_of_sexp Allocation.t_of_sexp arg0__116_ in
              Allocations res0__117_
            | _ ->
              Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                error_source__106_
                _tag__114_
                _sexp__113_)
         | Sexplib0.Sexp.Atom
             ( "deadcode_eliminated"
             | "Deadcode_eliminated"
             | "allocations"
             | "Allocations" ) as sexp__107_ ->
           Sexplib0.Sexp_conv_error.stag_takes_args error_source__106_ sexp__107_
         | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__105_ ->
           Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__106_ sexp__105_
         | Sexplib0.Sexp.List [] as sexp__105_ ->
           Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__106_ sexp__105_
         | sexp__105_ ->
           Sexplib0.Sexp_conv_error.unexpected_stag
             error_source__106_
             [ "Deadcode_eliminated"; "Allocations" ]
             sexp__105_
          : Sexplib0.Sexp.t -> state)
      ;;

      let _ = state_of_sexp

      let sexp_of_state =
        (function
         | Deadcode_eliminated arg0__118_ ->
           let res0__119_ = Location.Inferred.sexp_of_t arg0__118_ in
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Deadcode_eliminated"; res0__119_ ]
         | Allocations arg0__120_ ->
           let res0__121_ = sexp_of_list Allocation.sexp_of_t arg0__120_ in
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Allocations"; res0__121_ ]
          : state -> Sexplib0.Sexp.t)
      ;;

      let _ = sexp_of_state
    end [@@ocaml.doc "@inline"] [@@merlin.hide]

    type one_function =
      { function_name : string
      ; state : state
      }
    [@@deriving sexp]

    include struct
      let _ = fun (_ : one_function) -> ()

      let one_function_of_sexp =
        (let error_source__123_ = "allocations.ml.Stable.V2.one_function" in
         fun x__124_ ->
           Sexplib0.Sexp_conv_record.record_of_sexp
             ~caller:error_source__123_
             ~fields:
               (Field
                  { name = "function_name"
                  ; kind = Required
                  ; conv = string_of_sexp
                  ; rest =
                      Field
                        { name = "state"
                        ; kind = Required
                        ; conv = state_of_sexp
                        ; rest = Empty
                        }
                  })
             ~index_of_field:
               (function
                | "function_name" -> 0
                | "state" -> 1
                | _ -> -1)
             ~allow_extra_fields:false
             ~create:(fun (function_name, (state, ())) : one_function ->
               { function_name; state })
             x__124_
          : Sexplib0.Sexp.t -> one_function)
      ;;

      let _ = one_function_of_sexp

      let sexp_of_one_function =
        (fun { function_name = function_name__126_; state = state__128_ } ->
           let bnds__125_ = ([] : _ Stdlib.List.t) in
           let bnds__125_ =
             let arg__129_ = sexp_of_state state__128_ in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "state"; arg__129_ ] :: bnds__125_
               : _ Stdlib.List.t)
           in
           let bnds__125_ =
             let arg__127_ = sexp_of_string function_name__126_ in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "function_name"; arg__127_ ]
              :: bnds__125_
               : _ Stdlib.List.t)
           in
           Sexplib0.Sexp.List bnds__125_
          : one_function -> Sexplib0.Sexp.t)
      ;;

      let _ = sexp_of_one_function
    end [@@ocaml.doc "@inline"] [@@merlin.hide]

    type t = one_function list [@@deriving sexp]

    include struct
      let _ = fun (_ : t) -> ()

      let t_of_sexp =
        (fun x__131_ -> list_of_sexp one_function_of_sexp x__131_ : Sexplib0.Sexp.t -> t)
      ;;

      let _ = t_of_sexp

      let sexp_of_t =
        (fun x__132_ -> sexp_of_list sexp_of_one_function x__132_ : t -> Sexplib0.Sexp.t)
      ;;

      let _ = sexp_of_t
    end [@@ocaml.doc "@inline"] [@@merlin.hide]

    let serialize t = `V2 t

    let of_v1 v1 =
      List.map v1 ~f:(function { V1.function_name; allocations } ->
        { function_name; state = Allocations (List.map ~f:Allocation.of_v1 allocations) })
    ;;

    let to_v1 v1 =
      List.filter_map v1 ~f:(function { function_name; state } ->
        (match state with
         | Allocations allocations ->
           Some
             { V1.function_name; allocations = List.map ~f:Allocation.to_v1 allocations }
         | Deadcode_eliminated _ -> None))
    ;;
  end

  module Latest = V2

  type versions =
    [ `V1 of V1.t
    | `V2 of V2.t
    ]
  [@@deriving sexp]

  include struct
    let _ = fun (_ : versions) -> ()

    let __versions_of_sexp__ =
      (let error_source__141_ = "allocations.ml.Stable.versions" in
       function
       | Sexplib0.Sexp.Atom atom__134_ as _sexp__136_ ->
         (match atom__134_ with
          | "V1" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__141_ _sexp__136_
          | "V2" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__141_ _sexp__136_
          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
       | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__134_ :: sexp_args__137_) as
         _sexp__136_ ->
         (match atom__134_ with
          | "V1" as _tag__142_ ->
            (match sexp_args__137_ with
             | arg0__143_ :: [] ->
               let res0__144_ = V1.t_of_sexp arg0__143_ in
               `V1 res0__144_
             | _ ->
               Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                 error_source__141_
                 _tag__142_
                 _sexp__136_)
          | "V2" as _tag__138_ ->
            (match sexp_args__137_ with
             | arg0__139_ :: [] ->
               let res0__140_ = V2.t_of_sexp arg0__139_ in
               `V2 res0__140_
             | _ ->
               Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                 error_source__141_
                 _tag__138_
                 _sexp__136_)
          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__135_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
           error_source__141_
           sexp__135_
       | Sexplib0.Sexp.List [] as sexp__135_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
           error_source__141_
           sexp__135_
        : Sexplib0.Sexp.t -> versions)
    ;;

    let _ = __versions_of_sexp__

    let versions_of_sexp =
      (let error_source__146_ = "allocations.ml.Stable.versions" in
       fun sexp__145_ ->
         try __versions_of_sexp__ sexp__145_ with
         | Sexplib0.Sexp_conv_error.No_variant_match ->
           Sexplib0.Sexp_conv_error.no_matching_variant_found
             error_source__146_
             sexp__145_
        : Sexplib0.Sexp.t -> versions)
    ;;

    let _ = versions_of_sexp

    let sexp_of_versions =
      (function
       | `V1 v__147_ ->
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "V1"; V1.sexp_of_t v__147_ ]
       | `V2 v__148_ ->
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "V2"; V2.sexp_of_t v__148_ ]
        : versions -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_versions
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
end

include Stable.Latest

let t_of_sexp t =
  match Stable.versions_of_sexp t with
  | `V1 t -> of_v1 t
  | `V2 t -> t
;;

let serialize version t =
  match version with
  | `V2 -> Stable.sexp_of_versions (Stable.V2.serialize t)
  | `V1 -> Stable.sexp_of_versions (Stable.V1.serialize (to_v1 t))
;;

module Item = struct
  type t =
    | Deadcode of Location.Inferred.t
    | Allocation of Allocation.t
  [@@deriving sexp, compare]

  include struct
    let _ = fun (_ : t) -> ()

    let t_of_sexp =
      (let error_source__151_ = "allocations.ml.Item.t" in
       function
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom (("deadcode" | "Deadcode") as _tag__154_)
           :: sexp_args__155_) as _sexp__153_ ->
         (match sexp_args__155_ with
          | arg0__156_ :: [] ->
            let res0__157_ = Location.Inferred.t_of_sexp arg0__156_ in
            Deadcode res0__157_
          | _ ->
            Sexplib0.Sexp_conv_error.stag_incorrect_n_args
              error_source__151_
              _tag__154_
              _sexp__153_)
       | Sexplib0.Sexp.List
           (Sexplib0.Sexp.Atom (("allocation" | "Allocation") as _tag__159_)
           :: sexp_args__160_) as _sexp__158_ ->
         (match sexp_args__160_ with
          | arg0__161_ :: [] ->
            let res0__162_ = Allocation.t_of_sexp arg0__161_ in
            Allocation res0__162_
          | _ ->
            Sexplib0.Sexp_conv_error.stag_incorrect_n_args
              error_source__151_
              _tag__159_
              _sexp__158_)
       | Sexplib0.Sexp.Atom ("deadcode" | "Deadcode" | "allocation" | "Allocation") as
         sexp__152_ ->
         Sexplib0.Sexp_conv_error.stag_takes_args error_source__151_ sexp__152_
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__150_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__151_ sexp__150_
       | Sexplib0.Sexp.List [] as sexp__150_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__151_ sexp__150_
       | sexp__150_ ->
         Sexplib0.Sexp_conv_error.unexpected_stag
           error_source__151_
           [ "Deadcode"; "Allocation" ]
           sexp__150_
        : Sexplib0.Sexp.t -> t)
    ;;

    let _ = t_of_sexp

    let sexp_of_t =
      (function
       | Deadcode arg0__163_ ->
         let res0__164_ = Location.Inferred.sexp_of_t arg0__163_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Deadcode"; res0__164_ ]
       | Allocation arg0__165_ ->
         let res0__166_ = Allocation.sexp_of_t arg0__165_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Allocation"; res0__166_ ]
        : t -> Sexplib0.Sexp.t)
    ;;

    let _ = sexp_of_t

    let compare =
      (fun a__167_ b__168_ ->
         if Stdlib.( == ) a__167_ b__168_
         then 0
         else (
           match a__167_, b__168_ with
           | Deadcode _a__169_, Deadcode _b__170_ ->
             Location.Inferred.compare _a__169_ _b__170_
           | Deadcode _, _ -> -1
           | _, Deadcode _ -> 1
           | Allocation _a__171_, Allocation _b__172_ ->
             Allocation.compare _a__171_ _b__172_)
        : t -> (t[@merlin.hide]) -> int)
    ;;

    let _ = compare
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  let loc = function
    | Deadcode loc -> Some loc
    | Allocation (loc, _) -> loc
  ;;

  let byte_offset_within_file t =
    match loc t with
    | None -> None
    | Some loc -> Some (Location.Inferred.extract_byte_offset_within_file loc)
  ;;
end

let empty = []
let open_function ~function_name t = { function_name; state = Allocations [] } :: t

let notify_deadcode_eliminated ~name ~dbg t =
  { function_name = name; state = Deadcode_eliminated dbg } :: t
;;

let filter_functions ~pattern t =
  List.filter
    ~f:(fun { function_name; _ } -> String.is_substring ~substring:pattern function_name)
    t
;;

let add allocation t =
  match t with
  | [] -> { function_name = ""; state = Allocations [ allocation ] } :: t
  | { state = Deadcode_eliminated _; _ } :: _ -> t
  | { function_name; state = Allocations allocations } :: tl ->
    { function_name; state = Allocations (allocation :: allocations) } :: tl
;;

let remove_duplicates t =
  let remove_duplicates_in_list allocations =
    Allocation.Set.of_list allocations |> Allocation.Set.to_seq |> List.of_seq
  in
  List.map t ~f:(function
    | { function_name; state = Deadcode_eliminated dbg } ->
      { function_name; state = Deadcode_eliminated dbg }
    | { function_name; state = Allocations allocations } ->
      { function_name; state = Allocations (remove_duplicates_in_list allocations) })
;;

let to_list t =
  List.concat_map
    ~f:
      (function
       | { state = Allocations allocations; _ } ->
         List.map allocations ~f:(fun a -> Item.Allocation a)
       | { state = Deadcode_eliminated dbg; _ } -> [ Item.Deadcode dbg ])
    t
;;

let print_hum ppf t =
  List.rev t
  |> List.iter ~f:(function
       | { function_name; state = Deadcode_eliminated _ } ->
         Format.fprintf ppf "Function %s has been deadcode eliminated\n\n" function_name
       | { function_name; state = Allocations [] } ->
         Format.fprintf ppf "Function %s does not allocate\n\n" function_name
       | { function_name; state = Allocations allocations } ->
         Format.fprintf ppf "Function %s directly allocates\n" function_name;
         List.rev allocations
         |> List.iter ~f:(fun (location, Allocation.{ context; _ }) ->
              let context =
                match context with
                | Direct -> ""
                | Raise -> " in raise"
                | Catch -> " in catch"
              in
              match location with
              | None -> Format.fprintf ppf "Allocation%s at unknown location\n" context
              | Some Location.Inferred.{ loc; _ } ->
                if Location.With_inlined_frames.is_inlined loc
                then
                  Format.fprintf
                    ppf
                    "Allocation%s at %a. Full inlined locations are:\n    %a\n"
                    context
                    Location.Simple.print
                    (Location.With_inlined_frames.in_file loc)
                    Location.With_inlined_frames.print
                    loc
                else
                  Format.fprintf
                    ppf
                    "Allocation%s at %a\n"
                    context
                    Location.Simple.print
                    (Location.With_inlined_frames.in_file loc));
         Format.fprintf ppf "\n")
;;

let exclude_matching ~patterns t =
  if List.length patterns = 0
  then t
  else
    List.filter_map
      ~f:
        (function
         | { state = Deadcode_eliminated _; _ } -> None
         | { function_name; state = Allocations allocations } ->
           let filtered =
             List.filter
               ~f:
                 (function
                  | Some Location.Inferred.{ loc; _ }, _ ->
                    not (List.exists ~f:(Location.Pattern.matches loc) patterns)
                  | _ -> true)
               allocations
           in
           (match filtered with
            | [] -> None
            | _ -> Some { function_name; state = Allocations filtered }))
      t
;;

let infer_and_move_allocations_inlined_intra_file t =
  let allocations, to_move =
    List.fold_left
      t
      ~init:([], [])
      ~f:(fun (prior_allocations, prior_to_move) ({ function_name; state } as t) ->
      match state with
      | Deadcode_eliminated _ -> t :: prior_allocations, prior_to_move
      | Allocations allocations ->
        let filtered_allocations, to_move =
          List.partition
            ~f:
              (function
               | Some Location.Inferred.{ loc; _ }, _ ->
                 Location.With_inlined_frames.is_inlined_within_file loc
               | _ -> false)
            allocations
        in
        (match filtered_allocations with
         | [] -> prior_allocations, to_move @ prior_to_move
         | _ ->
           ( { function_name; state = Allocations filtered_allocations }
             :: prior_allocations
           , to_move @ prior_to_move )))
  in
  let to_move =
    to_move
    |> List.map ~f:(function
         | Some Location.Inferred.{ loc; precision }, a ->
           ( Some
               (let open Location.Inferred in
               { loc =
                   Location.With_inlined_frames.of_simple
                     (Location.With_inlined_frames.get_first_declaration loc)
               ; precision
               })
           , a )
         | a -> a)
    |> List.sort_uniq ~cmp:Allocation.compare
  in
  match to_move with
  | [] -> allocations
  | _ -> { function_name = ""; state = Allocations to_move } :: allocations
;;

let prune_deadcode t =
  List.filter_map t ~f:(function
    | { state = Deadcode_eliminated _; _ } -> None
    | { state = Allocations _; _ } as t -> Some t)
;;

module Hide_inlined = struct
  type t =
    | All
    | Same_file
    | External
    | None

  let print ppf = function
    | All -> Format.fprintf ppf "all"
    | Same_file -> Format.fprintf ppf "same-file"
    | External -> Format.fprintf ppf "external"
    | None -> Format.fprintf ppf "none"
  ;;

  let parse s =
    match String.lowercase_ascii s with
    | "all" -> `Ok All
    | "same" | "same-file" -> `Ok Same_file
    | "external" -> `Ok External
    | "none" -> `Ok None
    | _ ->
      `Error
        (Printf.sprintf "Got '%s', expecting one of all,same-file,external or none" s)
  ;;

  let is_none t = t = None
end

let hide_inlined which t =
  if Hide_inlined.is_none which
  then t
  else
    List.filter_map
      ~f:
        (function
         | { state = Deadcode_eliminated _; _ } as t -> Some t
         | { function_name; state = Allocations allocations } ->
           let filtered =
             List.filter
               ~f:
                 (function
                  | Some Location.Inferred.{ loc; _ }, _ ->
                    let should_hide =
                      match which with
                      | Hide_inlined.All -> Location.With_inlined_frames.is_inlined loc
                      | Same_file ->
                        Location.With_inlined_frames.is_inlined_within_file loc
                      | External ->
                        Location.With_inlined_frames.is_inlined loc
                        && not (Location.With_inlined_frames.is_inlined_within_file loc)
                      | None -> false
                    in
                    not should_hide
                  | _ -> true)
               allocations
           in
           (match filtered with
            | [] -> None
            | _ -> Some { function_name; state = Allocations filtered }))
      t
;;

module Locality_filter = struct
  type t =
    | Only_local
    | Only_global
    | All

  let print ppf = function
    | All -> Format.fprintf ppf "all"
    | Only_local -> Format.fprintf ppf "only-local"
    | Only_global -> Format.fprintf ppf "only-global"
  ;;

  let parse s =
    match String.lowercase_ascii s with
    | "all" -> `Ok All
    | "only-local" | "local" -> `Ok Only_local
    | "only-global" | "global" -> `Ok Only_global
    | _ ->
      `Error (Printf.sprintf "Got '%s', expecting one of all,only-global or only-local" s)
  ;;
end

let filter_by_locality locality t =
  let only_keep locality t =
    List.filter_map
      ~f:
        (function
         | { state = Deadcode_eliminated _; _ } as t -> Some t
         | { function_name; state = Allocations allocations } ->
           let filtered =
             List.filter
               ~f:
                 (function
                  | _, { Allocation.locality = l; _ } ->
                    Allocation.compare_locality l locality = 0)
               allocations
           in
           (match filtered with
            | [] -> None
            | _ -> Some { function_name; state = Allocations filtered }))
      t
  in
  match locality with
  | Locality_filter.All -> t
  | Only_global -> only_keep Global t
  | Only_local -> only_keep Local t
;;
