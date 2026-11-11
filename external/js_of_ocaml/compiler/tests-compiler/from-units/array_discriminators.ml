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

type _ buckets =
  | Float_buckets : float array -> float buckets
  | Immediate :
      { kind : 'a not_float
      ; values : 'a array
      }
      -> 'a buckets

external float_negative : float -> bool = "partition_float_negative"
external int64_negative : int64 -> bool = "partition_int64_negative"
external make : int -> 'a -> 'a array = "caml_make_vect"
external get : 'a array -> int -> 'a = "%array_unsafe_get"
external set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external report_progress : string -> unit = "report_progress"

let rec length = function
  | [] -> 0
  | _ :: tail -> 1 + length tail
;;

let to_array (head, tail) =
  let array = make (1 + length tail) head in
  let rec fill i = function
    | [] -> ()
    | head :: tail ->
      set array i head;
      fill (i + 1) tail
  in
  fill 1 tail;
  array
;;

let negative : type a. a buckets -> bool = function
  | Float_buckets values -> float_negative (get values 1)
  | Immediate { kind; values } ->
    let predicate : a -> bool =
      match kind with
      | Integer -> fun value -> value < 0
      | Time_span -> int64_negative
    in
    predicate (get values 1)
;;

let create : type a. a kind -> a * a list -> a buckets * bool =
  fun kind values ->
  let values = to_array values in
  let buckets : a buckets =
    match find kind with
    | First kind -> Immediate { kind; values }
    | Second Refl -> Float_buckets values
  in
  buckets, negative buckets
;;

let _, floats = create Float (0.0001, [ 1.; 10. ])
let _, spans = create Span (100L, [ -1L; 1000L ])

let () =
  report_progress
    (if floats then "float bucket is negative" else "float bucket is not negative");
  report_progress
    (if spans then "span bucket is negative" else "span bucket is not negative")
;;
