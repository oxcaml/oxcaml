open Sexplib.Std
open Std

module Stable = struct
  module V1 = struct
    module Path = struct
      type element =
        | Compilation_unit of string
        | Module of string
        | Class of string
        | Unknown
        | Call of
            { fn : t
            ; loc : Location.Simple.t
            }
        | Fundecl of
            { name : string
            ; loc : Location.Simple.t
            }
      [@@deriving sexp]

      and t = element list [@@deriving sexp]

      include struct
        let _ = fun (_ : element) -> ()
        let _ = fun (_ : t) -> ()

        let rec element_of_sexp =
          (let error_source__003_ = "inlining_report.ml.Stable.V1.Path.element" in
           function
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom
                  (("compilation_unit" | "Compilation_unit") as _tag__006_)
               :: sexp_args__007_) as _sexp__005_ ->
             (match sexp_args__007_ with
              | arg0__008_ :: [] ->
                let res0__009_ = string_of_sexp arg0__008_ in
                Compilation_unit res0__009_
              | _ ->
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  error_source__003_
                  _tag__006_
                  _sexp__005_)
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom (("module" | "Module") as _tag__011_)
               :: sexp_args__012_) as _sexp__010_ ->
             (match sexp_args__012_ with
              | arg0__013_ :: [] ->
                let res0__014_ = string_of_sexp arg0__013_ in
                Module res0__014_
              | _ ->
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  error_source__003_
                  _tag__011_
                  _sexp__010_)
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom (("class" | "Class") as _tag__016_) :: sexp_args__017_)
             as _sexp__015_ ->
             (match sexp_args__017_ with
              | arg0__018_ :: [] ->
                let res0__019_ = string_of_sexp arg0__018_ in
                Class res0__019_
              | _ ->
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  error_source__003_
                  _tag__016_
                  _sexp__015_)
           | Sexplib0.Sexp.Atom ("unknown" | "Unknown") -> Unknown
           | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("call" | "Call") :: sexps__021_) as
             sexp__020_ ->
             Sexplib0.Sexp_conv_record.record_of_sexps
               ~context:sexp__020_
               ~caller:error_source__003_
               ~fields:
                 (Field
                    { name = "fn"
                    ; kind = Required
                    ; conv = t_of_sexp
                    ; rest =
                        Field
                          { name = "loc"
                          ; kind = Required
                          ; conv = Location.Simple.t_of_sexp
                          ; rest = Empty
                          }
                    })
               ~index_of_field:
                 (function
                  | "fn" -> 0
                  | "loc" -> 1
                  | _ -> -1)
               ~allow_extra_fields:false
               ~create:(fun (fn, (loc, ())) : element -> Call { fn; loc })
               sexps__021_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("fundecl" | "Fundecl") :: sexps__023_)
             as sexp__022_ ->
             Sexplib0.Sexp_conv_record.record_of_sexps
               ~context:sexp__022_
               ~caller:error_source__003_
               ~fields:
                 (Field
                    { name = "name"
                    ; kind = Required
                    ; conv = string_of_sexp
                    ; rest =
                        Field
                          { name = "loc"
                          ; kind = Required
                          ; conv = Location.Simple.t_of_sexp
                          ; rest = Empty
                          }
                    })
               ~index_of_field:
                 (function
                  | "name" -> 0
                  | "loc" -> 1
                  | _ -> -1)
               ~allow_extra_fields:false
               ~create:(fun (name, (loc, ())) : element -> Fundecl { name; loc })
               sexps__023_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("unknown" | "Unknown") :: _) as
             sexp__004_ ->
             Sexplib0.Sexp_conv_error.stag_no_args error_source__003_ sexp__004_
           | Sexplib0.Sexp.Atom
               ( "compilation_unit"
               | "Compilation_unit"
               | "module"
               | "Module"
               | "class"
               | "Class"
               | "call"
               | "Call"
               | "fundecl"
               | "Fundecl" ) as sexp__004_ ->
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
               [ "Compilation_unit"; "Module"; "Class"; "Unknown"; "Call"; "Fundecl" ]
               sexp__002_
            : Sexplib0.Sexp.t -> element)

        and t_of_sexp =
          (fun x__025_ -> list_of_sexp element_of_sexp x__025_ : Sexplib0.Sexp.t -> t)
        ;;

        let _ = element_of_sexp
        and _ = t_of_sexp

        let rec sexp_of_element =
          (function
           | Compilation_unit arg0__026_ ->
             let res0__027_ = sexp_of_string arg0__026_ in
             Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Compilation_unit"; res0__027_ ]
           | Module arg0__028_ ->
             let res0__029_ = sexp_of_string arg0__028_ in
             Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Module"; res0__029_ ]
           | Class arg0__030_ ->
             let res0__031_ = sexp_of_string arg0__030_ in
             Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Class"; res0__031_ ]
           | Unknown -> Sexplib0.Sexp.Atom "Unknown"
           | Call { fn = fn__033_; loc = loc__035_ } ->
             let bnds__032_ = ([] : _ Stdlib.List.t) in
             let bnds__032_ =
               let arg__036_ = Location.Simple.sexp_of_t loc__035_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg__036_ ] :: bnds__032_
                 : _ Stdlib.List.t)
             in
             let bnds__032_ =
               let arg__034_ = sexp_of_t fn__033_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "fn"; arg__034_ ] :: bnds__032_
                 : _ Stdlib.List.t)
             in
             Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Call" :: bnds__032_)
           | Fundecl { name = name__038_; loc = loc__040_ } ->
             let bnds__037_ = ([] : _ Stdlib.List.t) in
             let bnds__037_ =
               let arg__041_ = Location.Simple.sexp_of_t loc__040_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg__041_ ] :: bnds__037_
                 : _ Stdlib.List.t)
             in
             let bnds__037_ =
               let arg__039_ = sexp_of_string name__038_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "name"; arg__039_ ] :: bnds__037_
                 : _ Stdlib.List.t)
             in
             Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Fundecl" :: bnds__037_)
            : element -> Sexplib0.Sexp.t)

        and sexp_of_t =
          (fun x__042_ -> sexp_of_list sexp_of_element x__042_ : t -> Sexplib0.Sexp.t)
        ;;

        let _ = sexp_of_element
        and _ = sexp_of_t

        let rec element_of_sexp =
          (let error_source__045_ = "inlining_report.ml.Stable.V1.Path.element" in
           function
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom
                  (("compilation_unit" | "Compilation_unit") as _tag__048_)
               :: sexp_args__049_) as _sexp__047_ ->
             (match sexp_args__049_ with
              | arg0__050_ :: [] ->
                let res0__051_ = string_of_sexp arg0__050_ in
                Compilation_unit res0__051_
              | _ ->
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  error_source__045_
                  _tag__048_
                  _sexp__047_)
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom (("module" | "Module") as _tag__053_)
               :: sexp_args__054_) as _sexp__052_ ->
             (match sexp_args__054_ with
              | arg0__055_ :: [] ->
                let res0__056_ = string_of_sexp arg0__055_ in
                Module res0__056_
              | _ ->
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  error_source__045_
                  _tag__053_
                  _sexp__052_)
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom (("class" | "Class") as _tag__058_) :: sexp_args__059_)
             as _sexp__057_ ->
             (match sexp_args__059_ with
              | arg0__060_ :: [] ->
                let res0__061_ = string_of_sexp arg0__060_ in
                Class res0__061_
              | _ ->
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  error_source__045_
                  _tag__058_
                  _sexp__057_)
           | Sexplib0.Sexp.Atom ("unknown" | "Unknown") -> Unknown
           | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("call" | "Call") :: sexps__063_) as
             sexp__062_ ->
             Sexplib0.Sexp_conv_record.record_of_sexps
               ~context:sexp__062_
               ~caller:error_source__045_
               ~fields:
                 (Field
                    { name = "fn"
                    ; kind = Required
                    ; conv = t_of_sexp
                    ; rest =
                        Field
                          { name = "loc"
                          ; kind = Required
                          ; conv = Location.Simple.t_of_sexp
                          ; rest = Empty
                          }
                    })
               ~index_of_field:
                 (function
                  | "fn" -> 0
                  | "loc" -> 1
                  | _ -> -1)
               ~allow_extra_fields:false
               ~create:(fun (fn, (loc, ())) : element -> Call { fn; loc })
               sexps__063_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("fundecl" | "Fundecl") :: sexps__065_)
             as sexp__064_ ->
             Sexplib0.Sexp_conv_record.record_of_sexps
               ~context:sexp__064_
               ~caller:error_source__045_
               ~fields:
                 (Field
                    { name = "name"
                    ; kind = Required
                    ; conv = string_of_sexp
                    ; rest =
                        Field
                          { name = "loc"
                          ; kind = Required
                          ; conv = Location.Simple.t_of_sexp
                          ; rest = Empty
                          }
                    })
               ~index_of_field:
                 (function
                  | "name" -> 0
                  | "loc" -> 1
                  | _ -> -1)
               ~allow_extra_fields:false
               ~create:(fun (name, (loc, ())) : element -> Fundecl { name; loc })
               sexps__065_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("unknown" | "Unknown") :: _) as
             sexp__046_ ->
             Sexplib0.Sexp_conv_error.stag_no_args error_source__045_ sexp__046_
           | Sexplib0.Sexp.Atom
               ( "compilation_unit"
               | "Compilation_unit"
               | "module"
               | "Module"
               | "class"
               | "Class"
               | "call"
               | "Call"
               | "fundecl"
               | "Fundecl" ) as sexp__046_ ->
             Sexplib0.Sexp_conv_error.stag_takes_args error_source__045_ sexp__046_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__044_ ->
             Sexplib0.Sexp_conv_error.nested_list_invalid_sum
               error_source__045_
               sexp__044_
           | Sexplib0.Sexp.List [] as sexp__044_ ->
             Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__045_ sexp__044_
           | sexp__044_ ->
             Sexplib0.Sexp_conv_error.unexpected_stag
               error_source__045_
               [ "Compilation_unit"; "Module"; "Class"; "Unknown"; "Call"; "Fundecl" ]
               sexp__044_
            : Sexplib0.Sexp.t -> element)

        and t_of_sexp =
          (fun x__067_ -> list_of_sexp element_of_sexp x__067_ : Sexplib0.Sexp.t -> t)
        ;;

        let _ = element_of_sexp
        and _ = t_of_sexp

        let rec sexp_of_element =
          (function
           | Compilation_unit arg0__068_ ->
             let res0__069_ = sexp_of_string arg0__068_ in
             Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Compilation_unit"; res0__069_ ]
           | Module arg0__070_ ->
             let res0__071_ = sexp_of_string arg0__070_ in
             Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Module"; res0__071_ ]
           | Class arg0__072_ ->
             let res0__073_ = sexp_of_string arg0__072_ in
             Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Class"; res0__073_ ]
           | Unknown -> Sexplib0.Sexp.Atom "Unknown"
           | Call { fn = fn__075_; loc = loc__077_ } ->
             let bnds__074_ = ([] : _ Stdlib.List.t) in
             let bnds__074_ =
               let arg__078_ = Location.Simple.sexp_of_t loc__077_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg__078_ ] :: bnds__074_
                 : _ Stdlib.List.t)
             in
             let bnds__074_ =
               let arg__076_ = sexp_of_t fn__075_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "fn"; arg__076_ ] :: bnds__074_
                 : _ Stdlib.List.t)
             in
             Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Call" :: bnds__074_)
           | Fundecl { name = name__080_; loc = loc__082_ } ->
             let bnds__079_ = ([] : _ Stdlib.List.t) in
             let bnds__079_ =
               let arg__083_ = Location.Simple.sexp_of_t loc__082_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "loc"; arg__083_ ] :: bnds__079_
                 : _ Stdlib.List.t)
             in
             let bnds__079_ =
               let arg__081_ = sexp_of_string name__080_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "name"; arg__081_ ] :: bnds__079_
                 : _ Stdlib.List.t)
             in
             Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Fundecl" :: bnds__079_)
            : element -> Sexplib0.Sexp.t)

        and sexp_of_t =
          (fun x__084_ -> sexp_of_list sexp_of_element x__084_ : t -> Sexplib0.Sexp.t)
        ;;

        let _ = sexp_of_element
        and _ = sexp_of_t
      end [@@ocaml.doc "@inline"] [@@merlin.hide]

      let rec compare_element e1 e2 =
        match e1, e2 with
        | Compilation_unit s1, Compilation_unit s2
        | Module s1, Module s2
        | Class s1, Class s2 -> String.compare s1 s2
        | Unknown, Unknown -> 0
        | Call { fn = fn1; loc = loc1 }, Call { fn = fn2; loc = loc2 } ->
          let c = Location.Simple.compare loc1 loc2 in
          if c <> 0 then c else compare fn1 fn2
        | Fundecl { name = n1; loc = loc1 }, Fundecl { name = n2; loc = loc2 } ->
          let c = Location.Simple.compare loc1 loc2 in
          if c <> 0 then c else String.compare n1 n2
        | Compilation_unit _, (Module _ | Class _ | Unknown | Call _ | Fundecl _) -> -1
        | Module _, (Class _ | Unknown | Call _ | Fundecl _) -> -1
        | Class _, (Unknown | Call _ | Fundecl _) -> -1
        | Unknown, (Call _ | Fundecl _) -> -1
        | Call _, Fundecl _ -> -1
        | Fundecl _, (Compilation_unit _ | Module _ | Class _ | Unknown | Call _) -> 1
        | Call _, (Compilation_unit _ | Module _ | Class _ | Unknown) -> 1
        | Unknown, (Compilation_unit _ | Module _ | Class _) -> 1
        | Class _, (Compilation_unit _ | Module _) -> 1
        | Module _, Compilation_unit _ -> 1

      and compare c1 c2 = List.compare ~cmp:compare_element c1 c2

      let add t elt = elt :: t
      let empty cu = [ Compilation_unit cu ]

      let location = function
        | Call { loc; _ } :: _ | Fundecl { loc; _ } :: _ -> Some loc
        | _ -> None
      ;;

      module Map = struct
        module M = Map.Make (struct
          type t = element

          let compare = compare_element
        end)

        include M
        include Make_map_sexp (M)

        let t_of_sexp data_of_sexp m = map_of_sexp element_of_sexp data_of_sexp m
        let sexp_of_t sexp_of_data m = sexp_of_map sexp_of_element sexp_of_data m
      end

      let from_root t = List.rev t

      let fold_from_root ~skip_compilation_unit ~f ~init t =
        List.fold_left
          ~f:
            (fun acc -> function
              | Compilation_unit _ when skip_compilation_unit -> acc
              | e -> f acc e)
          ~init
          (from_root t)
      ;;
    end

    module Call_decision = struct
      type t =
        | Reference of Path.t
        | Unavailable
        | Inlined
        | Not_inlined
      [@@deriving sexp, compare]

      include struct
        let _ = fun (_ : t) -> ()

        let t_of_sexp =
          (let error_source__087_ = "inlining_report.ml.Stable.V1.Call_decision.t" in
           function
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom (("reference" | "Reference") as _tag__090_)
               :: sexp_args__091_) as _sexp__089_ ->
             (match sexp_args__091_ with
              | arg0__092_ :: [] ->
                let res0__093_ = Path.t_of_sexp arg0__092_ in
                Reference res0__093_
              | _ ->
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  error_source__087_
                  _tag__090_
                  _sexp__089_)
           | Sexplib0.Sexp.Atom ("unavailable" | "Unavailable") -> Unavailable
           | Sexplib0.Sexp.Atom ("inlined" | "Inlined") -> Inlined
           | Sexplib0.Sexp.Atom ("not_inlined" | "Not_inlined") -> Not_inlined
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom
                  ( "unavailable"
                  | "Unavailable"
                  | "inlined"
                  | "Inlined"
                  | "not_inlined"
                  | "Not_inlined" )
               :: _) as sexp__088_ ->
             Sexplib0.Sexp_conv_error.stag_no_args error_source__087_ sexp__088_
           | Sexplib0.Sexp.Atom ("reference" | "Reference") as sexp__088_ ->
             Sexplib0.Sexp_conv_error.stag_takes_args error_source__087_ sexp__088_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__086_ ->
             Sexplib0.Sexp_conv_error.nested_list_invalid_sum
               error_source__087_
               sexp__086_
           | Sexplib0.Sexp.List [] as sexp__086_ ->
             Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__087_ sexp__086_
           | sexp__086_ ->
             Sexplib0.Sexp_conv_error.unexpected_stag
               error_source__087_
               [ "Reference"; "Unavailable"; "Inlined"; "Not_inlined" ]
               sexp__086_
            : Sexplib0.Sexp.t -> t)
        ;;

        let _ = t_of_sexp

        let sexp_of_t =
          (function
           | Reference arg0__094_ ->
             let res0__095_ = Path.sexp_of_t arg0__094_ in
             Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Reference"; res0__095_ ]
           | Unavailable -> Sexplib0.Sexp.Atom "Unavailable"
           | Inlined -> Sexplib0.Sexp.Atom "Inlined"
           | Not_inlined -> Sexplib0.Sexp.Atom "Not_inlined"
            : t -> Sexplib0.Sexp.t)
        ;;

        let _ = sexp_of_t

        let compare =
          (fun a__096_ b__097_ ->
             if Stdlib.( == ) a__096_ b__097_
             then 0
             else (
               match a__096_, b__097_ with
               | Reference _a__098_, Reference _b__099_ -> Path.compare _a__098_ _b__099_
               | Reference _, _ -> -1
               | _, Reference _ -> 1
               | Unavailable, Unavailable -> 0
               | Unavailable, _ -> -1
               | _, Unavailable -> 1
               | Inlined, Inlined -> 0
               | Inlined, _ -> -1
               | _, Inlined -> 1
               | Not_inlined, Not_inlined -> 0)
            : t -> (t[@merlin.hide]) -> int)
        ;;

        let _ = compare
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    end

    module Fundecl_decision = struct
      type t =
        | Cannot_be_inlined
        | Inlinable
        | Must_be_inlined
      [@@deriving sexp, compare]

      include struct
        let _ = fun (_ : t) -> ()

        let t_of_sexp =
          (let error_source__102_ = "inlining_report.ml.Stable.V1.Fundecl_decision.t" in
           function
           | Sexplib0.Sexp.Atom ("cannot_be_inlined" | "Cannot_be_inlined") ->
             Cannot_be_inlined
           | Sexplib0.Sexp.Atom ("inlinable" | "Inlinable") -> Inlinable
           | Sexplib0.Sexp.Atom ("must_be_inlined" | "Must_be_inlined") -> Must_be_inlined
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom
                  ( "cannot_be_inlined"
                  | "Cannot_be_inlined"
                  | "inlinable"
                  | "Inlinable"
                  | "must_be_inlined"
                  | "Must_be_inlined" )
               :: _) as sexp__103_ ->
             Sexplib0.Sexp_conv_error.stag_no_args error_source__102_ sexp__103_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__101_ ->
             Sexplib0.Sexp_conv_error.nested_list_invalid_sum
               error_source__102_
               sexp__101_
           | Sexplib0.Sexp.List [] as sexp__101_ ->
             Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__102_ sexp__101_
           | sexp__101_ ->
             Sexplib0.Sexp_conv_error.unexpected_stag
               error_source__102_
               [ "Cannot_be_inlined"; "Inlinable"; "Must_be_inlined" ]
               sexp__101_
            : Sexplib0.Sexp.t -> t)
        ;;

        let _ = t_of_sexp

        let sexp_of_t =
          (function
           | Cannot_be_inlined -> Sexplib0.Sexp.Atom "Cannot_be_inlined"
           | Inlinable -> Sexplib0.Sexp.Atom "Inlinable"
           | Must_be_inlined -> Sexplib0.Sexp.Atom "Must_be_inlined"
            : t -> Sexplib0.Sexp.t)
        ;;

        let _ = sexp_of_t
        let compare = (Stdlib.compare : t -> (t[@merlin.hide]) -> int)
        let _ = compare
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    end

    module Tree = struct
      type element =
        | Scope of t
        | Call of
            { decision_description : string
            ; decision : Call_decision.t
            ; inlined : t
            }
        | Fundecl of
            { decision_description : string
            ; decision : Fundecl_decision.t
            ; body : t
            }

      and t = element Path.Map.t [@@deriving sexp]

      include struct
        let _ = fun (_ : element) -> ()
        let _ = fun (_ : t) -> ()

        let rec element_of_sexp =
          (let error_source__108_ = "inlining_report.ml.Stable.V1.Tree.element" in
           function
           | Sexplib0.Sexp.List
               (Sexplib0.Sexp.Atom (("scope" | "Scope") as _tag__111_) :: sexp_args__112_)
             as _sexp__110_ ->
             (match sexp_args__112_ with
              | arg0__113_ :: [] ->
                let res0__114_ = t_of_sexp arg0__113_ in
                Scope res0__114_
              | _ ->
                Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                  error_source__108_
                  _tag__111_
                  _sexp__110_)
           | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("call" | "Call") :: sexps__116_) as
             sexp__115_ ->
             Sexplib0.Sexp_conv_record.record_of_sexps
               ~context:sexp__115_
               ~caller:error_source__108_
               ~fields:
                 (Field
                    { name = "decision_description"
                    ; kind = Required
                    ; conv = string_of_sexp
                    ; rest =
                        Field
                          { name = "decision"
                          ; kind = Required
                          ; conv = Call_decision.t_of_sexp
                          ; rest =
                              Field
                                { name = "inlined"
                                ; kind = Required
                                ; conv = t_of_sexp
                                ; rest = Empty
                                }
                          }
                    })
               ~index_of_field:
                 (function
                  | "decision_description" -> 0
                  | "decision" -> 1
                  | "inlined" -> 2
                  | _ -> -1)
               ~allow_extra_fields:false
               ~create:(fun (decision_description, (decision, (inlined, ()))) : element ->
                 Call { decision_description; decision; inlined })
               sexps__116_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("fundecl" | "Fundecl") :: sexps__118_)
             as sexp__117_ ->
             Sexplib0.Sexp_conv_record.record_of_sexps
               ~context:sexp__117_
               ~caller:error_source__108_
               ~fields:
                 (Field
                    { name = "decision_description"
                    ; kind = Required
                    ; conv = string_of_sexp
                    ; rest =
                        Field
                          { name = "decision"
                          ; kind = Required
                          ; conv = Fundecl_decision.t_of_sexp
                          ; rest =
                              Field
                                { name = "body"
                                ; kind = Required
                                ; conv = t_of_sexp
                                ; rest = Empty
                                }
                          }
                    })
               ~index_of_field:
                 (function
                  | "decision_description" -> 0
                  | "decision" -> 1
                  | "body" -> 2
                  | _ -> -1)
               ~allow_extra_fields:false
               ~create:(fun (decision_description, (decision, (body, ()))) : element ->
                 Fundecl { decision_description; decision; body })
               sexps__118_
           | Sexplib0.Sexp.Atom
               ("scope" | "Scope" | "call" | "Call" | "fundecl" | "Fundecl") as sexp__109_
             -> Sexplib0.Sexp_conv_error.stag_takes_args error_source__108_ sexp__109_
           | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__107_ ->
             Sexplib0.Sexp_conv_error.nested_list_invalid_sum
               error_source__108_
               sexp__107_
           | Sexplib0.Sexp.List [] as sexp__107_ ->
             Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__108_ sexp__107_
           | sexp__107_ ->
             Sexplib0.Sexp_conv_error.unexpected_stag
               error_source__108_
               [ "Scope"; "Call"; "Fundecl" ]
               sexp__107_
            : Sexplib0.Sexp.t -> element)

        and t_of_sexp =
          (fun x__120_ -> Path.Map.t_of_sexp element_of_sexp x__120_
            : Sexplib0.Sexp.t -> t)
        ;;

        let _ = element_of_sexp
        and _ = t_of_sexp

        let rec sexp_of_element =
          (function
           | Scope arg0__121_ ->
             let res0__122_ = sexp_of_t arg0__121_ in
             Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Scope"; res0__122_ ]
           | Call
               { decision_description = decision_description__124_
               ; decision = decision__126_
               ; inlined = inlined__128_
               } ->
             let bnds__123_ = ([] : _ Stdlib.List.t) in
             let bnds__123_ =
               let arg__129_ = sexp_of_t inlined__128_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "inlined"; arg__129_ ]
                :: bnds__123_
                 : _ Stdlib.List.t)
             in
             let bnds__123_ =
               let arg__127_ = Call_decision.sexp_of_t decision__126_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "decision"; arg__127_ ]
                :: bnds__123_
                 : _ Stdlib.List.t)
             in
             let bnds__123_ =
               let arg__125_ = sexp_of_string decision_description__124_ in
               (Sexplib0.Sexp.List
                  [ Sexplib0.Sexp.Atom "decision_description"; arg__125_ ]
                :: bnds__123_
                 : _ Stdlib.List.t)
             in
             Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Call" :: bnds__123_)
           | Fundecl
               { decision_description = decision_description__131_
               ; decision = decision__133_
               ; body = body__135_
               } ->
             let bnds__130_ = ([] : _ Stdlib.List.t) in
             let bnds__130_ =
               let arg__136_ = sexp_of_t body__135_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "body"; arg__136_ ] :: bnds__130_
                 : _ Stdlib.List.t)
             in
             let bnds__130_ =
               let arg__134_ = Fundecl_decision.sexp_of_t decision__133_ in
               (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "decision"; arg__134_ ]
                :: bnds__130_
                 : _ Stdlib.List.t)
             in
             let bnds__130_ =
               let arg__132_ = sexp_of_string decision_description__131_ in
               (Sexplib0.Sexp.List
                  [ Sexplib0.Sexp.Atom "decision_description"; arg__132_ ]
                :: bnds__130_
                 : _ Stdlib.List.t)
             in
             Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Fundecl" :: bnds__130_)
            : element -> Sexplib0.Sexp.t)

        and sexp_of_t =
          (fun x__137_ -> Path.Map.sexp_of_t sexp_of_element x__137_
            : t -> Sexplib0.Sexp.t)
        ;;

        let _ = sexp_of_element
        and _ = sexp_of_t
      end [@@ocaml.doc "@inline"] [@@merlin.hide]

      let empty = Path.Map.empty
      let insert ~key ~element t = Path.Map.add key element t

      let get_tree_strictly_under ~path t =
        Path.fold_from_root
          ~skip_compilation_unit:true
          ~init:t
          ~f:(fun acc key ->
            match Path.Map.find key acc with
            | Scope t -> t
            | Call { inlined; _ } -> inlined
            | Fundecl { body; _ } -> body)
          path
      ;;

      let get_function_call ~(path : Path.t) t =
        match path with
        | (Call _ as call) :: path ->
          let t = get_tree_strictly_under ~path t in
          Path.Map.find_opt call t
        | _ -> None
      ;;

      let get_function_def ~(path : Path.t) t =
        match path with
        | (Fundecl _ as fundecl) :: path ->
          let t = get_tree_strictly_under ~path t in
          Path.Map.find_opt fundecl t
        | _ -> None
      ;;

      let find_all_function_calls ~under t =
        let rec aux ~calls ~path t =
          Path.Map.to_seq t
          |> Seq.fold_left
               (fun acc (key, element) ->
                 match element with
                 | Scope t -> aux ~calls:acc ~path:(Path.add path key) t
                 | Call { decision; _ } -> (Path.add path key, decision) :: acc
                 | Fundecl { body; _ } -> aux ~calls:acc ~path:(Path.add path key) body)
               calls
        in
        let t = get_tree_strictly_under ~path:under t in
        aux ~calls:[] ~path:under t
      ;;

      let find_all_function_defs ~under t =
        let rec aux ~defs ~path t =
          Path.Map.to_seq t
          |> Seq.fold_left
               (fun acc (key, element) ->
                 match element with
                 | Scope t -> aux ~defs:acc ~path:(Path.add path key) t
                 | Fundecl { decision; _ } -> (Path.add path key, decision) :: acc
                 | Call _ -> acc)
               defs
        in
        let t = get_tree_strictly_under ~path:under t in
        aux ~defs:[] ~path:under t
      ;;
    end

    type t =
      | Org_mode of { content : string }
      | Tree of Tree.t
    [@@deriving sexp]

    include struct
      let _ = fun (_ : t) -> ()

      let t_of_sexp =
        (let error_source__140_ = "inlining_report.ml.Stable.V1.t" in
         function
         | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom ("org_mode" | "Org_mode") :: sexps__143_)
           as sexp__142_ ->
           Sexplib0.Sexp_conv_record.record_of_sexps
             ~context:sexp__142_
             ~caller:error_source__140_
             ~fields:
               (Field
                  { name = "content"
                  ; kind = Required
                  ; conv = string_of_sexp
                  ; rest = Empty
                  })
             ~index_of_field:
               (function
                | "content" -> 0
                | _ -> -1)
             ~allow_extra_fields:false
             ~create:(fun (content, ()) : t -> Org_mode { content })
             sexps__143_
         | Sexplib0.Sexp.List
             (Sexplib0.Sexp.Atom (("tree" | "Tree") as _tag__145_) :: sexp_args__146_) as
           _sexp__144_ ->
           (match sexp_args__146_ with
            | arg0__147_ :: [] ->
              let res0__148_ = Tree.t_of_sexp arg0__147_ in
              Tree res0__148_
            | _ ->
              Sexplib0.Sexp_conv_error.stag_incorrect_n_args
                error_source__140_
                _tag__145_
                _sexp__144_)
         | Sexplib0.Sexp.Atom ("org_mode" | "Org_mode" | "tree" | "Tree") as sexp__141_ ->
           Sexplib0.Sexp_conv_error.stag_takes_args error_source__140_ sexp__141_
         | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__139_ ->
           Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__140_ sexp__139_
         | Sexplib0.Sexp.List [] as sexp__139_ ->
           Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__140_ sexp__139_
         | sexp__139_ ->
           Sexplib0.Sexp_conv_error.unexpected_stag
             error_source__140_
             [ "Org_mode"; "Tree" ]
             sexp__139_
          : Sexplib0.Sexp.t -> t)
      ;;

      let _ = t_of_sexp

      let sexp_of_t =
        (function
         | Org_mode { content = content__150_ } ->
           let bnds__149_ = ([] : _ Stdlib.List.t) in
           let bnds__149_ =
             let arg__151_ = sexp_of_string content__150_ in
             (Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "content"; arg__151_ ] :: bnds__149_
               : _ Stdlib.List.t)
           in
           Sexplib0.Sexp.List (Sexplib0.Sexp.Atom "Org_mode" :: bnds__149_)
         | Tree arg0__152_ ->
           let res0__153_ = Tree.sexp_of_t arg0__152_ in
           Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Tree"; res0__153_ ]
          : t -> Sexplib0.Sexp.t)
      ;;

      let _ = sexp_of_t
    end [@@ocaml.doc "@inline"] [@@merlin.hide]

    let print ppf = function
      | Org_mode { content } -> Format.fprintf ppf "%s" content
      | Tree tree ->
        Format.fprintf ppf "%s" (Sexplib.Sexp.to_string_hum (Tree.sexp_of_t tree))
    ;;

    let serialize t = `V1 t
  end

  module Latest = V1

  type versions = [ `V1 of V1.t ] [@@deriving sexp]

  include struct
    let _ = fun (_ : versions) -> ()

    let __versions_of_sexp__ =
      (let error_source__162_ = "inlining_report.ml.Stable.versions" in
       function
       | Sexplib0.Sexp.Atom atom__155_ as _sexp__157_ ->
         (match atom__155_ with
          | "V1" ->
            Sexplib0.Sexp_conv_error.ptag_takes_args error_source__162_ _sexp__157_
          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
       | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom atom__155_ :: sexp_args__158_) as
         _sexp__157_ ->
         (match atom__155_ with
          | "V1" as _tag__159_ ->
            (match sexp_args__158_ with
             | arg0__160_ :: [] ->
               let res0__161_ = V1.t_of_sexp arg0__160_ in
               `V1 res0__161_
             | _ ->
               Sexplib0.Sexp_conv_error.ptag_incorrect_n_args
                 error_source__162_
                 _tag__159_
                 _sexp__157_)
          | _ -> Sexplib0.Sexp_conv_error.no_variant_match ())
       | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__156_ ->
         Sexplib0.Sexp_conv_error.nested_list_invalid_poly_var
           error_source__162_
           sexp__156_
       | Sexplib0.Sexp.List [] as sexp__156_ ->
         Sexplib0.Sexp_conv_error.empty_list_invalid_poly_var
           error_source__162_
           sexp__156_
        : Sexplib0.Sexp.t -> versions)
    ;;

    let _ = __versions_of_sexp__

    let versions_of_sexp =
      (let error_source__164_ = "inlining_report.ml.Stable.versions" in
       fun sexp__163_ ->
         try __versions_of_sexp__ sexp__163_ with
         | Sexplib0.Sexp_conv_error.No_variant_match ->
           Sexplib0.Sexp_conv_error.no_matching_variant_found
             error_source__164_
             sexp__163_
        : Sexplib0.Sexp.t -> versions)
    ;;

    let _ = versions_of_sexp

    let sexp_of_versions =
      (fun (`V1 v__165_) ->
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "V1"; V1.sexp_of_t v__165_ ]
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
