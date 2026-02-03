(******************************************************************************
 *                                 Chamelon                                   *
 *                         Milla Valnet, OCamlPro                             *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2023 OCamlPro                                                *
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

(* Iterators for the minimizer *)

open Utils

type 'a minimized_step_result =
  | New_state of 'a (* New (smaller) states that produces an error *)
  | Change_removes_error
    (* This change removes the error, but other changes might be possible *)
  | No_more_changes
(* The last possible position for changes has been reached *)

let minimize_basic (state : 'a) (f : 'a -> pos:int -> 'a minimized_step_result)
    : 'a * bool =
  let rec aux (state : 'a) (pos : int) (ever_changed : bool) =
    match f state ~pos with
    | New_state nstate -> aux nstate pos true
    | Change_removes_error -> aux state (pos + 1) ever_changed
    | No_more_changes -> (state, ever_changed)
  in
  aux state 0 false

let minimize_ranged (state : 'a)
    (f : 'a -> pos:int -> len:int -> 'a minimized_step_result) : 'a * bool =
  let rec loop_increasing (state : 'a) (pos : int) (len : int)
      (ever_changed : bool) =
    match f state ~pos ~len with
    | New_state nstate -> loop_increasing nstate pos (2 * len) true
    | Change_removes_error -> loop_decreasing state pos (len / 2) ever_changed
    | No_more_changes -> (state, ever_changed)
  and loop_decreasing (state : 'a) (pos : int) (len : int) (ever_changed : bool)
      =
    if len = 0 then loop_increasing state (pos + 1) 1 ever_changed
    else
      match f state ~pos ~len with
      | New_state nstate -> loop_decreasing nstate pos (len / 2) true
      | Change_removes_error -> loop_decreasing state pos (len / 2) ever_changed
      | No_more_changes -> (state, ever_changed)
  in
  loop_increasing state 0 1 false

let minimize_at minimize cur_file map ~pos ~len =
  let r = ref (-1) in
  let nmap =
    minimize.minimizer_func
      (fun () ->
        incr r;
        pos <= !r && !r < pos + len)
      map cur_file
  in
  (nmap, pos <= !r)

let step_minimizer ~check minimize cur_file map ~pos ~len =
  Format.eprintf "Trying %s: pos=%d, len=%d... @?" minimize.minimizer_name pos
    len;
  let map, changed = minimize_at minimize cur_file map ~pos ~len in
  let r =
    if changed then if check map then New_state map else Change_removes_error
    else No_more_changes
  in
  let () =
    match r with
    | New_state _ -> Format.eprintf "Reduced.@."
    | Change_removes_error -> Format.eprintf "Removes error.@."
    | No_more_changes -> Format.eprintf "No more changes.@."
  in
  r

let test_minimizer ~check ~pos ~len map cur_file minimize =
  let result, has_changed = minimize_at minimize cur_file map ~pos ~len in
  if has_changed && check result then (result, true) else (result, false)

type strategy = Basic | Dichotomy | Test of { pos : int; len : int }

let run_strategy strategy ~check map cur_file minimize =
  match strategy with
  | Dichotomy -> minimize_ranged map (step_minimizer ~check minimize cur_file)
  | Basic -> minimize_basic map (step_minimizer ~check minimize cur_file ~len:1)
  | Test { pos; len } -> test_minimizer ~check ~pos ~len map cur_file minimize

let basic = Basic
let dichotomy = Dichotomy
let test ~pos ~len = Test { pos; len }

type ('a, 'b) schedule =
  | Minimizer of ('a, 'b) Utils.minimizer
  | With_strategy of strategy * ('a, 'b) schedule
  | Sequence of ('a, 'b) schedule list
  | Fix of ('a, 'b) schedule

let rec run0 : type a b.
    strategy:strategy ->
    check:(a -> bool) ->
    (a, b) schedule ->
    a ->
    b ->
    a * bool =
 fun ~strategy ~check schedule map cur_file ->
  match schedule with
  | Minimizer minimizer -> run_strategy strategy ~check map cur_file minimizer
  | With_strategy (strategy, schedule) ->
      run0 ~strategy ~check schedule map cur_file
  | Sequence schedules ->
      List.fold_left
        (fun (map, ever_changed) schedule ->
          let map', has_changed = run0 ~strategy ~check schedule map cur_file in
          (map', has_changed || ever_changed))
        (map, false) schedules
  | Fix schedule ->
      let rec loop map ever_changed =
        let map, has_changed = run0 ~strategy ~check schedule map cur_file in
        if has_changed then loop map true else (map, ever_changed)
      in
      loop map false

let run ?(strategy = dichotomy) ~check schedule map cur_file =
  run0 ~strategy ~check schedule map cur_file

let minimizer minimizer = Minimizer minimizer

let list schedules =
  Sequence
    (List.concat_map
       (function
         | Sequence schedules -> schedules
         | (Minimizer _ | Fix _ | With_strategy _) as schedule -> [ schedule ])
       schedules)

let with_strategy strategy schedules =
  match list schedules with
  | With_strategy (_, _) as schedule -> schedule
  | (Sequence _ | Minimizer _ | Fix _) as schedule ->
      With_strategy (strategy, schedule)

let fix schedules =
  match list schedules with
  | Fix _ as schedule -> schedule
  | (Sequence _ | Minimizer _ | With_strategy _) as schedule -> Fix schedule
