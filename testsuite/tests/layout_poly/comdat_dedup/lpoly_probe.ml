let[@inline never] poly_ lpoly_probe n x y =
  [%probe "lpoly_probe" (print_int n)];
  #(n + 1, y, x)
