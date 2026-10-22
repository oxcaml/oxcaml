The signature for package types, as well as packed expressions changed
in between.

Originally we had a bug in the migrations that would silently drop some
attributes. Now we split them internally and when migrating back to compiler's
that can distinguish these attributes we make sure to split them back
correctly.

  $ cat > test.ml << EOF
  > module type S = sig type t end
  > let f (module K : S with type t = int [@foo]) = ()
  > let f (module K : S with type t = (int [@foo])) = ()
  > let f (module K : S with type t = (int [@foo])[@bar]) = ()
  > type t = (module S [@inner])[@outer]
  > 
  > EOF

  $ ./id_driver.exe test.ml
  module type S  = sig type t end
  let f ((module K)  : (((module S with type t = int))[@foo ])) = ()
  let f ((module K)  : (module S with type t = ((int)[@foo ]))) = ()
  let f ((module K)  : (((module S with type t = ((int)[@foo ])))[@bar ])) = ()
  type t = (((module S))[@outer ][@inner ])
  $ ./id_driver.exe --use-compiler-pp test.ml
  module type S  = sig type t end
  let f (module K : ((S with type t = int)[@foo ])) = ()
  let f (module K : S with type t = ((int)[@foo ])) = ()
  let f (module K : ((S with type t = ((int)[@foo ]))[@bar ])) = ()
  type t = (((module ((S)[@inner ])))[@outer ])

