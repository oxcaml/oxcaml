(* TEST
 flags = "-extension runtime_metaprogramming -extension comprehensions";
 expect;
 expect.opt;
*)

(* disallowed usages of break_: *)

(* break statements must be in a loop *)

let x = break_;;
[%%expect{|
Line 1, characters 8-14:
1 | let x = break_;;
            ^^^^^^
Error: break_ cannot be used outside of a loop
|}]

let x =
  for i = 0 to 10 do () done;
  break_;
  for i = 0 to 10 do () done
[%%expect{|
Line 3, characters 2-8:
3 |   break_;
      ^^^^^^
Error: break_ cannot be used outside of a loop
|}]

let x =
  let f () = () in
  f break_
[%%expect{|
Line 3, characters 4-10:
3 |   f break_
        ^^^^^^
Error: break_ cannot be used outside of a loop
|}]

let x =
  while break_ do
    ()
  done
[%%expect{|
Line 2, characters 8-14:
2 |   while break_ do
            ^^^^^^
Error: break_ cannot be used outside of a loop
|}]

(* break statements are typed as 'a *)

let x =
  for i = 0 to 10 do
    if i > 3
    then (break_; Format.printf "unreachable@.")
    else ()
  done
[%%expect{|
Line 4, characters 10-16:
4 |     then (break_; Format.printf "unreachable@.")
              ^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)

val x : unit = ()
|}]

let x =
  for i = 0 to 10 do
    Format.printf "hello@.";
    break_
  done
[%%expect{|
Line 4, characters 4-10:
4 |     break_
        ^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)
hello

val x : unit = ()
|}]

let x =
  for i = 0 to 10 do
    let open Stdlib in break_
  done
[%%expect{|
Line 3, characters 23-29:
3 |     let open Stdlib in break_
                           ^^^^^^
Warning 21 [nonreturning-statement]: this statement never returns (or has an unsound type.)

val x : unit = ()
|}]

(* break statements cannot be in a closure *)

let x =
  for i = 0 to 10 do
    let f () = break_ in
    f ()
  done
[%%expect{|
Line 3, characters 15-21:
3 |     let f () = break_ in
                   ^^^^^^
Error: break_ cannot be used inside a function (at line 3, characters 10-21).
|}]

let x =
  for i = 0 to 10 do
    Array.iter (fun _ -> break_) [| 1 ; 2 |]
  done
[%%expect{|
Line 3, characters 25-31:
3 |     Array.iter (fun _ -> break_) [| 1 ; 2 |]
                             ^^^^^^
Error: break_ cannot be used inside a function (at line 3, characters 15-32).
|}]

let x =
  for i = 0 to 10 do
    ignore (lazy break_)
  done
[%%expect{|
Line 3, characters 17-23:
3 |     ignore (lazy break_)
                     ^^^^^^
Error: break_ cannot be used inside a lazy expression (at line 3, characters 11-24).
|}]

let x =
  for i = 0 to 10 do
    let (let*) x f = f x in
    let* y = 0 in
    break_
  done
;;
[%%expect {|
Line 5, characters 4-10:
5 |     break_
        ^^^^^^
Error: break_ cannot be used inside a letop (at lines 4-5, characters 4-10).
|}]

module type S = sig module F () : sig end end

let x =
  for i = 0 to 10 do
    ignore (module struct module F () = struct let () = break_ end end : S)
  done
[%%expect {|
module type S = sig module F : functor () -> sig end end
Line 5, characters 56-62:
5 |     ignore (module struct module F () = struct let () = break_ end end : S)
                                                            ^^^^^^
Error: break_ cannot be used inside a functor (at line 5, characters 35-66).
|}]

#syntax quotations on

let f g =
  for i = 0 to 10 do
    g <[ break_ ]>
  done
[%%expect {|
Line 3, characters 9-15:
3 |     g <[ break_ ]>
             ^^^^^^
Error: break_ cannot be used inside a quoted expression (at line 3, characters 6-18).
|}]

#syntax quotations off

let f g =
  for i = 0 to 10 do
    g (object val v : unit = break_ end)
  done
[%%expect {|
Line 3, characters 29-35:
3 |     g (object val v : unit = break_ end)
                                 ^^^^^^
Error: break_ cannot be used inside an object (at line 3, characters 6-40).
|}]

class d () = object end

let f () =
  for i = 0 to 10 do
    let module M = struct class c = d break_ end in ()
  done
[%%expect {|
class d : unit -> object  end
>> Fatal error: Loop_label_map.find: missing label
Uncaught exception: Misc.Fatal_error

|}]


let f () =
  for i = 0 to 10 do
    ignore [ break_ for _ in [1; 2; 3] ]
  done
[%%expect {|
>> Fatal error: Loop_label_map.find: missing label
Uncaught exception: Misc.Fatal_error

|}]

(* allowed usages of break_: *)

(* break statements break the loop *)

let x =
  let n = ref 0 in
  for i = 0 to 10 do
    incr n;
    if !n >= 3 then break_
  done;
  !n
;;
[%%expect {|
val x : int = 3
|}]

let x =
  while true do
    if true then break_
  done;
  Format.printf "does not loop forever@."
;;
[%%expect {|
does not loop forever
val x : unit = ()
|}]

let x =
  let find a p =
    let mutable found = None in
    for i = 0 to Array.length a - 1 do
      if p a.(i) then (found <- Some a.(i); break_)
    done;
    found
  in
  find [| 0; 2; 3; 5; 6 |] (fun n -> n mod 2 != 0)
[%%expect {|
val x : int option = Some 3
|}]

(* break statements work inside nested loops *)

let x =
  let add_until_zero m =
    let mutable i = 0 in
    let mutable sum = 0 in
    while i < Array.length m do
      let mutable j = 0 in
      while j < Array.length m.(i) do
        if m.(i).(j) == 0 then break_;
        sum <- sum + m.(i).(j);
        j <- j + 1
      done;
      i <- i + 1
    done;
    sum
  in
  add_until_zero [| [| 1; 2; 0; 3 |] ; [| 4; 0 |] ; [| 0; 5; 6 |] |]
[%%expect{|
val x : int = 7
|}]

let x =
  let first_primes k limit =
    let mutable count = 0 in
    for n = 2 to limit do
      if count = k then break_;
      let mutable is_prime = true in
      for d = 2 to n - 1 do
        if d * d > n then break_;
        if n mod d = 0 then begin
          is_prime <- false;
          break_
        end
      done;
      if is_prime then begin
        Format.printf "%d@." n;
        count <- count + 1
      end
    done
  in
  first_primes 5 100
[%%expect{|
2
3
5
7
11
val x : unit = ()
|}]

let x =
  let remove_whitespace_at_ends arr =
    let mutable k = 0 in
    while k < Array.length arr do
      let s = arr.(k) in
      let len = String.length s in

      let mutable start_idx = 0 in
      for i = 0 to len - 1 do
        if s.[i] <> ' ' then break_;
        start_idx <- start_idx + 1
      done;

      let mutable end_idx = len in
      for i = len - 1 downto start_idx do
        if s.[i] <> ' ' then break_;
        end_idx <- end_idx - 1
      done;

      Format.printf "[%s]@." (String.sub s start_idx (end_idx - start_idx));
      k <- k + 1
    done
  in
  remove_whitespace_at_ends [| " ab "; "c"; "  " |]
[%%expect{|
[ab]
[c]
[]
val x : unit = ()
|}]

(* break statements are typed as 'a (continued) *)

let x =
  let f () = Format.printf "not reached@." in
  for i = 0 to 10 do
    f break_
  done;
[%%expect{|
val x : unit = ()
|}]

let x =
  let f (b : bool) = Format.printf "not reached: %b@." b in
  for i = 0 to 10 do
    f break_
  done;
[%%expect{|
val x : unit = ()
|}]

let x =
  let r = ref 0 in
  for i = 0 to 10 do
    r := break_;
    r := !r + 1
  done;
  !r
[%%expect{|
val x : int = 0
|}]

let x =
  let n = ref 0 in
  for i = 0 to 10 do
    for j = 0 to break_ do
      incr n (* not reached *)
    done
  done;
  !n
[%%expect{|
val x : int = 0
|}]

let x =
  let n = ref 0 in
  while true do
    while break_ do
      incr n (* not reached *)
    done
  done;
  !n
[%%expect{|
val x : int = 0
|}]


let x =
  let r = ref [] in
  for i = 1 to 10 do
    let pair =
      if i < 4 then (i, i + 1) else (i, break_)
    in
    r := pair :: !r
  done;
  !r
[%%expect{|
val x : (int * int) list = [(3, 4); (2, 3); (1, 2)]
|}]

(* break statements can be in a loop inside a function *)

let x =
  let n = ref 0 in
  let f () =
    for i = 0 to 10 do
      incr n;
      if !n >= 3 then break_
    done;
    !n
  in
  f ()
[%%expect{|
val x : int = 3
|}]


(* while true loops with a break are typed as unit *)

let x : string =
  while true do
    ignore (break_)
  done;
;;
[%%expect{|
Lines 2-4, characters 2-6:
2 | ..while true do
3 |     ignore (break_)
4 |   done.
Error: This expression has type "unit" but an expression was expected of type
         "string"
|}]

(* while true loops without a break are still typed as 'a *)

let x () : 'a =
  while true do
    ()
  done
;;
[%%expect{|
val x : unit -> 'a = <fun>
|}]

let x () : 'a =
  while true do
    while true do
      ignore (break_)
    done
  done
;;
[%%expect{|
val x : unit -> 'a = <fun>
|}]

(* combining break and other sources of static try/catch *)

let () =
  let f a b =
    for i = 0 to 3 do
      Format.printf "%d@." i;
      match a, b with
      | true, true -> break_
      | _ -> ()
    done
  in
  f true true
[%%expect{|
0
|}]

let x =
  let n = ref 0 in
  (try
     for i = 0 to 10 do
       incr n;
       (try if i = 2 then break_ with Not_found -> ())
     done;
     raise Exit
   with Exit -> n := !n + 100);
  !n
[%%expect{|
val x : int = 103
|}]

let x =
  let n = ref 0 in
  for i = 0 to 10 do
    incr n;
    match if i = 2 then raise Not_found else i with
    | exception Not_found -> break_
    | 1 -> break_
    | _ -> ()
  done;
  !n
[%%expect{|
val x : int = 2
|}]

(* break at a non-value type *)

external unbox : float -> float# = "%unbox_float"
external box : float# -> float = "%box_float"

let x =
  let acc = ref 0. in
  for i = 0 to 10 do
    let u : float# = if i = 3 then break_ else unbox (Float.of_int i) in
    acc := !acc +. box u
  done;
  !acc

[%%expect{|
external unbox : float -> float# = "%unbox_float"
external box : float# -> float = "%box_float"
val x : float = 3.
|}]
