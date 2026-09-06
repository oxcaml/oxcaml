(******************************************************************************
 *                                  OxCaml                                    *
 *                        Basile Clément, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

open Datalog_imports

type bindings_ref =
  | Bindings_ref_innermost_first :
      ('a Value.hlist * 'a Or_null_receiver.hlist with_names)
      -> bindings_ref
[@@unboxed]

module Make (Iterator : Leapfrog.Iterator) = struct
  module Join_iterator = Leapfrog.Join (Iterator)

  type label = int

  (* In order to produce a bytecode structure that follows the bytecode
     structure of loops, all opcodes that take a destination labels, except for
     [Advance], fall through if the condition *fails*, and jumps to the label if
     the condition succeeds.

     This is a bit counter-intuitive: one could reasonably expect that an opcode
     called e.g. [absent] would jump if the value is absent. However, this makes
     each opcode read like an assertion mapping to the corresponding condition
     in the original rule and plan, as in the example below. This is deemed more
     useful than renaming e.g. [absent] to [jump_if_in]. *)
  (* f(x) :- P(x), ~Q(x).
   *
   * for x in P:
   *   if x not in Q:
   *       f(x)
   *
   *       init x, [P], 005        # jump to 005 if P is empty
   * 002:    absent Q, [x], 004    # jump to 004 if x in Q
   *         call f, [x]
   * 004:    advance x, [P], 002   # jump to 002 if P is *not* exhausted
   * 005:  exit
   *)
  type code =
    | Exit
    | Goto : label -> code
    | Init :
        'k Or_null_sender.t * string * 'k Join_iterator.t * string list * label
        -> code
    | Advance :
        'k Or_null_sender.t * string * 'k Join_iterator.t * string list * label
        -> code
    | Absent :
        ('t, 'k, 'v) Trie.is_trie
        * 't Or_null_receiver.t
        * string
        * 'k Or_null_receiver.hlist
        * string list
        * label
        -> code
    | Distinct :
        'k Value.repr
        * 'k Or_null_receiver.t
        * string
        * 'k Or_null_receiver.t
        * string
        * label
        -> code
    | Seek :
        'k Join_iterator.t
        * string list
        * 'k Or_null_receiver.t
        * string
        * label
        -> code
    | Filter :
        ('k Constant.hlist -> bool)
        * string
        * 'k Or_null_receiver.hlist
        * string list
        * label
        -> code
    | Call_with_bindings :
        (bindings_ref -> 'b Constant.hlist -> unit)
        * string
        * bindings_ref
        * 'b Or_null_receiver.hlist
        * string list
        -> code

  let labels = function
    | Exit | Call_with_bindings _ -> []
    | Goto lab
    | Init (_, _, _, _, lab)
    | Advance (_, _, _, _, lab)
    | Absent (_, _, _, _, _, lab)
    | Distinct (_, _, _, _, _, lab)
    | Seek (_, _, _, _, lab)
    | Filter (_, _, _, _, lab) ->
      [lab]

  let print_list =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf ";@ ")
      Format.pp_print_string

  let print_label digits ppf lab = Format.fprintf ppf "%0*d" digits lab

  let print_code digits ppf = function
    | Exit -> Format.fprintf ppf "exit"
    | Goto lab -> Format.fprintf ppf "goto@ %a" (print_label digits) lab
    | Init (_sender, name, _iterator, names, if_empty) ->
      Format.fprintf ppf "init %s,@ @[[%a]@],@ %a" name print_list names
        (print_label digits) if_empty
    | Advance (_sender, name, _iterator, names, if_not_empty) ->
      Format.fprintf ppf "advance %s,@ @[[%a]@],@ %a" name print_list names
        (print_label digits) if_not_empty
    | Distinct (_, _, name1, _, name2, if_equal) ->
      Format.fprintf ppf "distinct %s,@ %s,@ %a" name1 name2
        (print_label digits) if_equal
    | Absent (_, _, table, _, args, if_not_in) ->
      Format.fprintf ppf "absent @[[%a]@],@ %s,@ %a" print_list args table
        (print_label digits) if_not_in
    | Seek (_, names, _, name, if_empty) ->
      Format.fprintf ppf "seek %s,@ @[[%a]@],@ %a" name print_list names
        (print_label digits) if_empty
    | Filter (_, name, _, names, if_false) ->
      Format.fprintf ppf "filter %s,@ @[[%a]@],@ %a" name print_list names
        (print_label digits) if_false
    | Call_with_bindings (_, name, _, _, names) ->
      Format.fprintf ppf "call %s,@ @[[%a]@]" name print_list names

  let print_code_iarray ppf code =
    let length = Iarray.length code in
    let digits = max 3 (log10 (float length) |> ceil |> int_of_float) in
    let all_labels = Hashtbl.create 0 in
    Iarray.iter
      (fun code ->
        List.iter (fun lab -> Hashtbl.replace all_labels lab ()) (labels code))
      code;
    let depth = ref 0 in
    let pop_depths = ref [] in
    Format.fprintf ppf "@[<v>";
    for i = 0 to length - 1 do
      let instruction = Iarray.get code i in
      while
        match !pop_depths with
        | d :: rest when d = i ->
          decr depth;
          pop_depths := rest;
          true
        | _ -> false
      do
        ()
      done;
      if i > 0 then Format.fprintf ppf "@ ";
      Format.fprintf ppf "@[<h>";
      if Hashtbl.mem all_labels i
      then Format.fprintf ppf "%0*d:@ " digits i
      else Format.pp_print_string ppf (String.make (digits + 2) ' ');
      Format.pp_print_string ppf (String.make (2 * !depth) ' ');
      Format.fprintf ppf "%a" (print_code digits) instruction;
      Format.fprintf ppf "@]";
      match[@warning "-fragile-match"] instruction with
      | Init (_, _, _, _, lab) when lab > i ->
        incr depth;
        pop_depths := lab :: !pop_depths
      | _ -> ()
    done;
    assert (List.is_empty !pop_depths);
    Format.fprintf ppf "@]"

  let lab () = ref (-1)

  let get_label len label =
    let pc = !label in
    if pc < 0 then Misc.fatal_error "get_label: unbound label";
    if pc >= len then Misc.fatal_error "get_label: out of bounds";
    pc

  type 'a late = Late of (int -> 'a) [@@unboxed]

  let run_late (Late f) len = f len

  type state =
    { code : code late Dynarray.t;
      bindings : bindings_ref;
      loops : int ref list
    }

  let here label st =
    if !label >= 0 then Misc.fatal_error "here: label already defined";
    label := Dynarray.length st.code

  let emit insn st = Dynarray.add_last st.code (Late (fun _ -> insn))

  let emit_goto label f st =
    Dynarray.add_last st.code (Late (fun len -> f (get_label len label)))

  (* Note: [break 0] doesn't break out of any loops *)
  let break n st =
    if n > 0
    then
      match List.nth st.loops (n - 1) with
      | lab -> emit_goto lab (fun lab -> Goto lab) st
      | exception Not_found -> Misc.fatal_error "cannot break"

  let with_binding repr receiver body st =
    let (Bindings_ref_innermost_first (reprs, receivers)) = st.bindings in
    body receiver
      { st with
        bindings =
          Bindings_ref_innermost_first
            ( repr.value :: reprs,
              { values = receiver :: receivers.values;
                names = repr.name :: receivers.names
              } )
      }

  let with_scope lab body st = body { st with loops = lab :: st.loops }

  let init receiver iterator label =
    emit_goto label (fun label ->
        Init
          (receiver.value, receiver.name, iterator.values, iterator.names, label))

  let advance receiver iterator label =
    emit_goto label (fun label ->
        Advance
          (receiver.value, receiver.name, iterator.values, iterator.names, label))

  let absent is_trie { value = trie; name } { values = args; names } label =
    emit_goto label (fun label ->
        Absent (is_trie, trie, name, args, names, label))

  let distinct repr key1 key2 label =
    emit_goto label (fun label ->
        Distinct (repr, key1.value, key1.name, key2.value, key2.name, label))

  let seek iterator { value = receiver; name } label =
    emit_goto label (fun label ->
        Seek (iterator.values, iterator.names, receiver, name, label))

  let filter { value = fn; name } { values = args; names } label =
    emit_goto label (fun label -> Filter (fn, name, args, names, label))

  let ( ++ ) fn1 fn2 code =
    fn1 code;
    fn2 code

  let list xs st = List.iter (fun x -> x st) xs

  let for_in repr iterators body =
    let iterator =
      { iterators with values = Join_iterator.create iterators.values }
    in
    let start_of_body = lab () in
    let after_loop = lab () in
    let sender, receiver = Channel.create_or_null Or_null.null in
    with_binding repr receiver (fun receiver ->
        list
          [ init { repr with value = sender } iterator after_loop;
            here start_of_body;
            with_scope after_loop (body receiver);
            advance { repr with value = sender } iterator start_of_body;
            here after_loop ])

  type assembler = state -> unit

  let if_template test body =
    let after_body = lab () in
    list [test after_body; body; here after_body]

  let if_in key iterators body =
    let iterator =
      { iterators with values = Join_iterator.create iterators.values }
    in
    if_template (seek iterator key) body

  let if_not_in is_trie table args body =
    if_template (absent is_trie table args) body

  let if_not_equal repr arg1 arg2 body =
    if_template (distinct repr arg1 arg2) body

  let if_ fn args body = if_template (filter fn args) body

  let call_with_bindings { value = fn; name } { values = args; names } st =
    emit (Call_with_bindings (fn, name, st.bindings, args, names)) st

  (* Use a [private] type from an anonymous module to ensure that we only ever
     construct bytecode that satisfies the requirements of [exec] below (namely,
     there are no out-of-range labels, and the code ends with an [Exit]
     instruction) . *)
  include (
    struct
      type t = { code : code iarray }

      let validate ~length code =
        List.for_all (fun lab -> 0 <= lab && lab < length) (labels code)

      let is_exit = function[@warning "-fragile-match"]
        | Exit -> true
        | _ -> false

      let create code =
        if
          (not (Iarray.for_all (validate ~length:(Iarray.length code)) code))
          || not (is_exit (Iarray.get code (Iarray.length code - 1)))
        then Misc.fatal_error "";
        { code }
    end :
      sig
        type t = private { code : code iarray }

        val create : code iarray -> t
      end)

  let print ppf { code } = print_code_iarray ppf code

  let assemble text =
    let delayed = Dynarray.create () in
    let st =
      { code = delayed;
        loops = [];
        bindings = Bindings_ref_innermost_first ([], { values = []; names = [] })
      }
    in
    text st;
    (* The last instruction is always an [Exit], which means we can use
       [unsafe_get] in [exec]. *)
    emit Exit st;
    let len = Dynarray.length delayed in
    Dynarray.unsafe_to_iarray ~capacity:len (fun code ->
        Dynarray.iter
          (fun insn -> Dynarray.add_last code (run_late insn len))
          delayed)
    |> create

  let read = Or_null_receiver.recv

  let read_hlist = Or_null_receiver.recv_hlist

  let write = Or_null_sender.send

  let[@loop] rec exec code pc =
    (* This assumes that the bytecode is well-formed, i.e. there are no
       out-of-range labels and the last instruction is an [Exit], in order to
       avoid performing a bounds check on the read of the [code] array.

       Codes created from the [assemble] function are guaranteed to satisfy
       these requirements. *)
    let[@inline] goto new_pc = exec code new_pc in
    let[@inline] next () = goto (pc + 1) in
    match Iarray.unsafe_get code pc with
    | Exit -> ()
    | Goto lab -> goto lab
    | Init (key_out, _name, iterator, _names, if_empty) -> (
      Join_iterator.init iterator;
      match Join_iterator.current iterator with
      | Null -> goto if_empty
      | This current_key ->
        Join_iterator.accept iterator;
        write key_out current_key;
        next ())
    | Advance (key_out, _name, iterator, _names, if_not_empty) -> (
      Join_iterator.advance iterator;
      match Join_iterator.current iterator with
      | Null -> next ()
      | This current_key ->
        Join_iterator.accept iterator;
        write key_out current_key;
        goto if_not_empty)
    | Seek (iterator, _names, key_in, _name, if_not_in) -> (
      let key = read key_in in
      Join_iterator.init iterator;
      Join_iterator.seek iterator key;
      match Join_iterator.current iterator with
      | This current_key when Join_iterator.equal_key iterator current_key key
        ->
        Join_iterator.accept iterator;
        next ()
      | This _ | Null -> goto if_not_in)
    | Absent (is_trie, table, _name, args, _names, if_mem) -> (
      match Trie.find_or_null is_trie (read_hlist args) (read table) with
      | This _ -> goto if_mem
      | Null -> next ())
    | Distinct (repr, key1, _name1, key2, _name2, if_equal) ->
      if Value.equal_repr repr (read key1) (read key2)
      then goto if_equal
      else next ()
    | Filter (func, _name, args, _names, if_false) ->
      if func (read_hlist args) then next () else goto if_false
    | Call_with_bindings (func, _name, bindings, args, _names) ->
      func bindings (read_hlist args);
      next ()

  let run t = exec t.code 0
end

(* API for the [Cursor]-based [Scheduler]. *)

type bindings =
  | Bindings_innermost_first :
      'a Value.hlist * 'a Constant.hlist with_names
      -> bindings

let get_bindings (Bindings_ref_innermost_first (reprs, receivers)) =
  let values = Or_null_receiver.recv_hlist receivers.values in
  Bindings_innermost_first (reprs, { receivers with values })

let print_bindings ppf (Bindings_innermost_first (reprs, { values; names })) =
  let rec loop : type a.
      Format.formatter ->
      a Value.hlist ->
      a Constant.hlist ->
      string list ->
      bool =
   fun ppf reprs values names ->
    match reprs, values, names with
    | [], [], _ :: _ | _ :: _, _ :: _, [] ->
      Misc.fatal_error "Wrong number of names"
    | [], [], [] -> true
    | repr :: reprs, value :: values, name :: names ->
      let first = loop ppf reprs values names in
      if not first then Format.fprintf ppf ";@,";
      Format.fprintf ppf "@[<1>%s =@ %a@]" name (Value.print_repr repr) value;
      false
  in
  Format.fprintf ppf "@[<2>{ @[<v>";
  ignore (loop ppf reprs values names);
  Format.fprintf ppf "@] }@]"
