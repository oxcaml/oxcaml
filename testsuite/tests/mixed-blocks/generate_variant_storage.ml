(* TEST
 setup-ocamlc.opt-build-env;
 ocamlc.opt;
 {
   arguments = "50 native";
   output = "${test_build_directory}/generated_variant_storage.ml.corrected";
   run;
   reference = "${test_source_directory}/generated_variant_storage.ml";
   check-program-output;
 }{
   arguments = "200 byte";
   output = "${test_build_directory}/generated_variant_storage_byte.ml.corrected";
   run;
   reference = "${test_source_directory}/generated_variant_storage_byte.ml";
   check-program-output;
 }
*)

let printf = Printf.printf
let sprintf = Printf.sprintf

type scalar = Text | Tagged | Narrow | Wide | Fraction
type field = Scalar of scalar | Pair of scalar * scalar

let scalars = [Text; Tagged; Narrow; Wide; Fraction]

(* Diagonal enumeration, as in generate_mixed_blocks_code.ml. Seq.product
   visits both dimensions before exhausting either one. *)
let rec lists_of_length elements = function
  | 0 -> Seq.return []
  | n -> Seq.product elements (lists_of_length elements (n - 1))
         |> Seq.map (fun (x, xs) -> x :: xs)

let shuffle state xs =
  let a = Array.of_list xs in
  Array.shuffle ~rand:(fun bound -> Random.State.int state bound) a;
  Array.to_list a

(* Emit the flat argument list and each way to group one adjacent pair. *)
let rec group_one_pair = function
  | a :: b :: rest ->
    (Pair (a, b) :: List.map (fun x -> Scalar x) rest)
    :: List.map (List.cons (Scalar a)) (group_one_pair (b :: rest))
  | _ -> []

let arm_palette state =
  let arms width =
    lists_of_length (List.to_seq scalars) width
    |> Seq.flat_map (fun xs ->
         List.to_seq (List.map (fun x -> Scalar x) xs :: group_one_pair xs))
    |> List.of_seq |> shuffle state |> List.take 12
  in
  (* Sample each width separately so four-field arms do not crowd out
     nullary and single-field arms. *)
  List.init 5 arms |> List.concat

let leaves = function Scalar x -> [x] | Pair (a, b) -> [a; b]

let mixed arms =
  let fields = List.concat_map (List.concat_map leaves) arms in
  List.mem Text fields && List.exists (function
    | Narrow | Wide | Fraction -> true | Text | Tagged -> false) fields

let variants () =
  let state = Random.State.make [|1234567|] in
  let arms = arm_palette state in
  let rec constructor_lists = function
    | 0 -> Seq.return []
    | count ->
      let choices = shuffle state arms |> List.to_seq in
      Seq.product choices (constructor_lists (count - 1))
      |> Seq.map (fun (arm, others) -> arm :: others)
  in
  let family count =
    constructor_lists count |> Seq.filter mixed |> Seq.take 2048
    |> List.of_seq |> shuffle state |> List.to_seq
  in
  let two = family 2 in
  let three = family 3 in
  let four = family 4 in
  let five = family 5 in
  Seq.interleave (Seq.interleave two four) (Seq.interleave three five)

(* Keep the original field-order regressions in addition to the enumeration. *)
let regressions =
  let flat xs = List.map (fun x -> Scalar x) xs in
  [ [flat [Text; Text; Wide]; flat [Text; Narrow; Wide; Fraction];
     flat [Narrow; Wide]; []];
    [flat [Narrow; Wide; Fraction; Text]; flat [Wide; Narrow]; [];
     flat [Text; Wide; Text]];
    [flat [Narrow; Wide]; []; [Pair (Wide, Text); Scalar Text];
     [Pair (Wide, Fraction); Scalar Text; Scalar Narrow]] ]

let type_name = function
  | Text -> "string" | Tagged -> "int" | Narrow -> "int32_u"
  | Wide -> "int64_u" | Fraction -> "float#"

let kind_name = function
  | Text -> "Text" | Tagged -> "Tagged" | Narrow -> "Narrow"
  | Wide -> "Wide" | Fraction -> "Fraction"

let field_expressions ~sep render fields =
  let index = ref 0 in
  let next scalar =
    let n = !index in incr index; render scalar n
  in
  List.map (function
    | Scalar x -> next x
    | Pair (a, b) ->
      let left = next a in
      sprintf "#(%s%s%s)" left sep (next b)) fields

let application tag = function
  | [] -> tag
  | [x] -> tag ^ " (" ^ x ^ ")"
  | xs -> tag ^ " (" ^ String.concat ", " xs ^ ")"

let construct scalar n =
  let get f = sprintf "%s seed %d" f n in
  match scalar with
  | Text -> get "text" | Tagged -> get "number"
  | Narrow -> "unbox32 (" ^ get "narrow" ^ ")"
  | Wide -> "unbox64 (" ^ get "wide" ^ ")"
  | Fraction -> "unbox_float (" ^ get "fraction" ^ ")"

let observed scalar n =
  let v = sprintf "x%d" n in
  match scalar with
  | Text -> "S " ^ v | Tagged -> "I " ^ v | Narrow -> "I32 (box32 " ^ v ^ ")"
  | Wide -> "I64 (box64 " ^ v ^ ")" | Fraction -> "F (box_float " ^ v ^ ")"

let emit index arms =
  let each f = List.iteri (fun i fields -> f i (sprintf "K%d" i) fields) arms in
  printf "\nmodule V%d = struct\n  type payload =\n" index;
  each (fun _ tag fields ->
    let args = field_expressions ~sep:" * " (fun s _ -> type_name s) fields in
    printf "    | %s%s\n" tag (match args with
      | [] -> "" | _ -> " of " ^ String.concat " * " args));
  print_endline {|  [@@unboxed]
  type cell = { left : string; left_bits : int32_u; mutable data : payload;
                right_bits : int64_u; right : string }
  let make seed = function|};
  each (fun i tag fields ->
    let case = if i = List.length arms - 1 then
        sprintf "i -> assert (i = %d);" i else sprintf "%d ->" i in
    printf "    | %s %s\n" case
      (application tag (field_expressions ~sep:", " construct fields)));
  print_endline "  let read = function";
  each (fun i tag fields ->
    let pattern = field_expressions ~sep:", " (fun _ n -> sprintf "x%d" n) fields in
    let atoms = List.concat_map leaves fields |> List.mapi (fun n s -> observed s n) in
    printf "    | %s -> %d, [%s]\n" (application tag pattern) i (String.concat "; " atoms));
  print_endline {|  let[@inline never] create seed i =
    { left = guard "left" seed; left_bits = #0xdead_beefl; data = make seed i;
      right_bits = #0x1234_5678_9abc_def0L; right = guard "right" seed }
  let[@inline never] replace cell seed i = cell.data <- make seed i
  let[@inline never] observe cell =
    cell.left, box32 cell.left_bits, read cell.data, box64 cell.right_bits, cell.right|};
  let arm fields = List.concat_map leaves fields |> List.map kind_name
                   |> String.concat "; " |> sprintf "[%s]" in
  printf "  let () = exercise \"V%d\" [|%s|] ~create ~replace ~observe\nend\n"
    index (String.concat "; " (List.map arm arms))

let () =
  let count = int_of_string Sys.argv.(1) in
  let backend = match Sys.argv.(2) with
    | "native" -> "native" | "byte" -> "bytecode" | s -> invalid_arg s in
  printf "(* TEST\n modules = \"variant_storage_oracle.ml\";\n %s;\n*)\n" backend;
  print_endline "(* Generated by generate_variant_storage.ml. *)\nopen Variant_storage_oracle";
  Seq.append (List.to_seq regressions) (variants ())
  |> Seq.take count |> Seq.iteri emit
