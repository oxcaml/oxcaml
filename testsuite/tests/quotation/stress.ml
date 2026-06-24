(* TEST
  flags = "-extension runtime_metaprogramming -gno-upstream-dwarf -g";
  native;
*)

#syntax quotations on

(* The structure of this program is roughly significant, but in unclear ways *)
let build n =
  <[ fun () ->
     let knocked_out = ref 0 in
     let _ : _ =
       $(let rec loop acc n =
           if n <= 0
           then <[ [] ]>
           else
             <[ let knocked_out_date =
                  try raise Not_found with
                  | Not_found -> 20130322
                in
                let date = knocked_out_date in
                knocked_out := date;
                $(loop acc (n - 1)) ]>
         in
         loop <[ [] ]> n)
     in
     20130322 ]>
;;

let _ = Sys.opaque_identity (build 10000)
