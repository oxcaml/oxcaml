type _ kind =
  | Float : float kind
  | Int : int kind
  | Span : int64 kind

type _ not_float =
  | Integer : int not_float
  | Time_span : int64 not_float

type (_, _) equal = Refl : ('a, 'a) equal

type ('a, 'b) either =
  | First of 'a
  | Second of 'b

let find : type a. a kind -> (a not_float, (a, float) equal) either = function
  | Float -> Second Refl
  | Int -> First Integer
  | Span -> First Time_span
;;

type 'a nonempty =
  { head : 'a
  ; tail : 'a list
  }

type _ t =
  | Float_buckets : float array -> float t
  | Immediate :
      { kind : 'a not_float
      ; values : 'a array
      }
      -> 'a t

external make : int -> 'a -> 'a array = "caml_make_vect"
external get : 'a array -> int -> 'a = "%array_safe_get"
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
external length : 'a array -> int = "%array_length"
external float_less : float -> float -> bool = "%lessthan"
external span_less : int64 -> int64 -> bool = "%lessthan"
external raise : exn -> 'a = "%raise"

exception Negative_bucket

let rec list_length = function
  | [] -> 0
  | _ :: tail -> 1 + list_length tail
;;

let to_array { head; tail } =
  let values = make (1 + list_length tail) head in
  let rec fill index = function
    | [] -> ()
    | value :: tail ->
      set values index value;
      fill (index + 1) tail
  in
  fill 1 tail;
  values
;;

let exists predicate values =
  let rec loop index =
    index < length values && (predicate (get values index) || loop (index + 1))
  in
  loop 0
;;

let validate_non_negative : type a. a t -> unit = function
  | Float_buckets values ->
    if exists (fun value -> float_less value 0.) values then raise Negative_bucket
  | Immediate { kind; values } ->
    let negative : a -> bool =
      match kind with
      | Integer -> fun value -> value < 0
      | Time_span -> fun value -> span_less value 0L
    in
    if exists negative values then raise Negative_bucket
;;

let create_exn : type a. a kind -> a nonempty -> a t =
  fun kind buckets ->
  let values = to_array buckets in
  let buckets : a t =
    match find kind with
    | First kind -> Immediate { kind; values }
    | Second Refl -> Float_buckets values
  in
  validate_non_negative buckets;
  buckets
;;

let floats =
  create_exn
    Float
    { head = 0.0001
    ; tail = [ 0.0002; 0.0005; 0.001; 0.002; 0.005; 0.01; 0.02; 0.05; 0.1; 1.; 5.; 10. ]
    }
;;

let spans =
  create_exn
    Span
    { head = 100L
    ; tail =
        [ 200L
        ; 500L
        ; 1000L
        ; 2000L
        ; 5000L
        ; 10000L
        ; 20000L
        ; 50000L
        ; 100000L
        ; 1000000L
        ; 5000000L
        ; 10000000L
        ]
    }
;;

let bucket_count : type a. a t -> int = function
  | Float_buckets values -> length values
  | Immediate { values; _ } -> length values
;;

let count () = bucket_count floats + bucket_count spans
