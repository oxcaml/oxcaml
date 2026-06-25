(* TEST
  flags = "-extension runtime_metaprogramming -gno-upstream-dwarf -g";
  native;
*)

#syntax quotations on

(* Original test that broke when heap-allocating binder-generated closures *)

let _ =
  let n = 10000 in
  <[ fun () ->
     let knocked_out = ref 0 in
     let _ : _ =
      (* It is key that [loop] is stack-allocated, so this test might pass
         with [loop @ global]. *)
       $(let rec (loop @ local) : _ @ global -> _ @ local once = fun #(acc, n) ->
           if n <= 0
           then <[ [] ]>
           else
             <[ let knocked_out_date =
                  try raise Not_found with
                  | Not_found -> 42
                in
                let date = knocked_out_date in
                knocked_out := date;
                $(loop #(acc, n - 1)) ]>
         in
         loop #(<[ [] ]>, n))
     in
     42 ]>
;;
