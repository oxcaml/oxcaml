(* TEST
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)

let () =
  (Unix.putenv [@alert "-unsafe_multidomain"]) "TZ" "UTC";
  let tm = Unix.{
    tm_sec = 45;
    tm_min = 30;
    tm_hour = 12;
    tm_mday = 14;
    tm_mon = 7;
    tm_year = 2025 - 1900;
    tm_wday = 4;
    tm_yday = 225;
    tm_isdst = true;
  } in
  let tm_to_string (tm: Unix.tm) =
    Printf.sprintf
      {|{ tm_sec   = %d;
  tm_min   = %d;
  tm_hour  = %d;
  tm_mday  = %d;
  tm_mon   = %d;
  tm_year  = %d;
  tm_wday  = %d;
  tm_yday  = %d;
  tm_isdst = %b }|}
      tm.tm_sec
      tm.tm_min
      tm.tm_hour
      tm.tm_mday
      tm.tm_mon
      tm.tm_year
      tm.tm_wday
      tm.tm_yday
      tm.tm_isdst
  in
  let seconds, tm = Unix.mktime tm in
  print_endline (tm_to_string tm);
  print_endline (tm_to_string (Unix.gmtime seconds));
  print_endline (tm_to_string (Unix.localtime seconds))
;;
