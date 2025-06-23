module Fields = struct
  type _ t =
    | Field :
        { name : string
        ; conv : Sexp.t -> 'a
        ; rest : 'b t
        }
        -> ('a * 'b) t
    | Empty : unit t

  let rec length_loop : type a. a t -> int -> int =
    fun t acc ->
    match t with
    | Empty -> acc
    | Field field -> length_loop field.rest (acc + 1)
  ;;

  let length t = length_loop t 0
end

let[@tail_mod_cons] rec of_list
  : type a.
    caller:string
    -> fields:a Fields.t
    -> len:int
    -> original_sexp:Sexp.t
    -> pos:int
    -> Sexp.t list
    -> a
  =
  fun ~caller ~fields ~len ~original_sexp ~pos list ->
  match fields with
  | Empty ->
    (match list with
     | [] -> ()
     | _ :: _ -> Sexp_conv_error.tuple_of_size_n_expected caller len original_sexp)
  | Field { name; conv; rest } ->
    (match list with
     | [] -> Sexp_conv_error.tuple_of_size_n_expected caller len original_sexp
     | sexp :: list ->
       (match sexp with
        | List [ Atom atom; sexp ] ->
          if String.equal atom name
          then
            ( conv sexp
            , of_list ~caller ~fields:rest ~len ~original_sexp ~pos:(pos + 1) list )
          else Sexp_conv_error.tuple_incorrect_label caller name pos original_sexp
        | _ -> Sexp_conv_error.tuple_pair_expected caller name sexp))
;;

let labeled_tuple_of_sexp ~caller ~fields ~create original_sexp =
  let len = Fields.length fields in
  match (original_sexp : Sexp.t) with
  | Atom _ -> Sexp_conv_error.tuple_of_size_n_expected caller len original_sexp
  | List list -> create (of_list ~caller ~fields ~len ~original_sexp ~pos:0 list)
;;
