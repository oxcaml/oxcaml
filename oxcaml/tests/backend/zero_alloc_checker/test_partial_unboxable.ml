let[@zero_alloc partial] f (x [@unboxable] : float @ local) y =
  exclave_ x +. y
