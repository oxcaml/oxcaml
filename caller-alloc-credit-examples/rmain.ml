let () =
  Printf.printf "rx1 %d %d\n" (Rx1.g 5) (Rx1.g (-3));
  Printf.printf "rx2 %d %d %d\n" (Rx2.g 5) (Rx2.g 20) (Rx2.g (-3));
  Printf.printf "rx3 %d %d\n" (Rx3.g 5) (Rx3.g (-3));
  Printf.printf "rx4 %f %f\n" (Rx4.g 5) (Rx4.g (-3));
  Printf.printf "rx5 %d %d\n" (Rx5.g 5) (Rx5.g (-3))
