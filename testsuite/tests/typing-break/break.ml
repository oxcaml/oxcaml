(* TEST
 expect;
*)

(* disallowed usages of break_: *)

(* break statements cannot be in a loop *)

let x = break_;;
[%%expect{|
Line 1, characters 8-14:
1 | let x = break_;;
            ^^^^^^
Error: Cannot use break_ outside loop
|}]

let x =
  for i = 0 to 10 do () done;
  break_;
  for i = 0 to 10 do () done
[%%expect{|
Line 3, characters 2-8:
3 |   break_;
      ^^^^^^
Error: Cannot use break_ outside loop
|}]

let x =
  let f () = () in
  f break_
[%%expect{|
Line 3, characters 4-10:
3 |   f break_
        ^^^^^^
Error: Cannot use break_ outside loop
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
  let r = ref 0 in
  for i = 0 to 10 do
    let f () = r := break_ in
    f ()
  done
[%%expect{|
Line 4, characters 20-26:
4 |     let f () = r := break_ in
                        ^^^^^^
Error: break_ cannot be used inside a function (at line 4, characters 10-26).
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

(* allowed usages of break_: *)

(* break statements break the loop *)

let x =
  let n = ref 0 in
  for i = 0 to 10 do
    if !n == 3
    then break_
    else (incr n)
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

(* for
      for
          break

   for
      for
          break
      break

   for
      for
          break
      for
          break
*)

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
