  $ check() {
  >   cat > allocation.ml
  >   if $OCAMLC -w -a -c allocation.ml > compiler.log 2>&1; then
  >     echo 'compiler: accepted'
  >   else
  >     echo 'compiler: rejected'
  >   fi
  >   for mode in single server; do
  >     echo "merlin $mode:"
  >     $MERLIN "$mode" errors -filename allocation.ml < allocation.ml \
  >       | revert-newlines \
  >       | jq -r '.class, (.value | map(select(.type != "warning") | .message))'
  >   done
  > }

  $ check <<'EOF'
  > let (compare_int @ noalloc_strict) (x : int) = compare x 0
  > let (compare_string @ noalloc_strict) (x : string) = compare x "a"
  > let (equal_none @ noalloc_strict) (x : int option) = x = None
  > let (get_int @ noalloc_strict) (x : int array) = Array.unsafe_get x 0
  > EOF
  compiler: accepted
  merlin single:
  return
  []
  merlin server:
  return
  []

  $ check <<'EOF'
  > external identity : ('a[@local_opt]) -> ('a[@local_opt]) = "%identity"
  > external opaque : ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"
  > let (use_identity @ noalloc_strict) (x : int) = identity x
  > let (use_opaque @ noalloc_strict) (x : int) = opaque x
  > EOF
  compiler: accepted
  merlin single:
  return
  []
  merlin server:
  return
  []

  $ check <<'EOF'
  > external equal : string -> string -> bool = "caml_string_equal" [@@noalloc]
  > let (use_equal @ noalloc_strict) x = equal x "a"
  > EOF
  compiler: accepted
  merlin single:
  return
  []
  merlin server:
  return
  []

  $ check <<'EOF'
  > external make_ref : 'a -> ('a ref[@local_opt]) = "%makemutable"
  > let (use_ref @ noalloc_strict) (x : int) = let r = make_ref x in !r
  > EOF
  compiler: accepted
  merlin single:
  return
  []
  merlin server:
  return
  []

  $ check <<'EOF'
  > let (compare_poly @ noalloc_strict) x = compare x x
  > EOF
  compiler: rejected
  merlin single:
  return
  [
    "The allocation is local\n  because it is allocated inside the function at file \"allocation.ml\", line 1, characters 36-51,\n  which is noalloc_strict and thus cannot allocate on the heap.\nHowever, the allocation highlighted is expected to be global."
  ]
  merlin server:
  return
  [
    "The allocation is local\n  because it is allocated inside the function at file \"allocation.ml\", line 1, characters 36-51,\n  which is noalloc_strict and thus cannot allocate on the heap.\nHowever, the allocation highlighted is expected to be global."
  ]

  $ check <<'EOF'
  > external equal : string -> string -> bool = "caml_string_equal"
  > let (use_equal @ noalloc_strict) x = equal x "a"
  > EOF
  compiler: rejected
  merlin single:
  return
  [
    "The allocation is local\n  because it is allocated inside the function at file \"allocation.ml\", line 2, characters 33-48,\n  which is noalloc_strict and thus cannot allocate on the heap.\nHowever, the allocation highlighted is expected to be global."
  ]
  merlin server:
  return
  [
    "The allocation is local\n  because it is allocated inside the function at file \"allocation.ml\", line 2, characters 33-48,\n  which is noalloc_strict and thus cannot allocate on the heap.\nHowever, the allocation highlighted is expected to be global."
  ]

  $ check <<'EOF'
  > external argv : string array = "%sys_argv"
  > let (use_argv @ noalloc_strict) () = argv
  > EOF
  compiler: rejected
  merlin single:
  return
  [
    "The allocation is local\n  because it is allocated inside the function at file \"allocation.ml\", line 2, characters 32-41,\n  which is noalloc_strict and thus cannot allocate on the heap.\nHowever, the allocation highlighted is expected to be global."
  ]
  merlin server:
  return
  [
    "The allocation is local\n  because it is allocated inside the function at file \"allocation.ml\", line 2, characters 32-41,\n  which is noalloc_strict and thus cannot allocate on the heap.\nHowever, the allocation highlighted is expected to be global."
  ]

  $ check <<'EOF'
  > let (partial_add @ noalloc_strict) (x : int) = ( + ) x
  > EOF
  compiler: rejected
  merlin single:
  return
  [
    "The allocation is local\n  because it is allocated inside the function at file \"allocation.ml\", line 1, characters 35-54,\n  which is noalloc_strict and thus cannot allocate on the heap.\nHowever, the allocation highlighted is expected to be global."
  ]
  merlin server:
  return
  [
    "The allocation is local\n  because it is allocated inside the function at file \"allocation.ml\", line 1, characters 35-54,\n  which is noalloc_strict and thus cannot allocate on the heap.\nHowever, the allocation highlighted is expected to be global."
  ]

  $ check <<'EOF'
  > external compare_prim : 'a -> 'a -> int = "%compare"
  > let (compare_value @ noalloc_strict) x = compare_prim x x
  > EOF
  compiler: rejected
  merlin single:
  return
  [
    "The allocation is local\n  because it is allocated inside the function at file \"allocation.ml\", line 2, characters 37-57,\n  which is noalloc_strict and thus cannot allocate on the heap.\nHowever, the allocation highlighted is expected to be global."
  ]
  merlin server:
  return
  [
    "The allocation is local\n  because it is allocated inside the function at file \"allocation.ml\", line 2, characters 37-57,\n  which is noalloc_strict and thus cannot allocate on the heap.\nHowever, the allocation highlighted is expected to be global."
  ]

  $ check <<'EOF'
  > external compare_prim : 'a -> 'a -> int = "%compare"
  > let (compare_value @ noalloc_strict) (x : int) = compare_prim x x
  > EOF
  compiler: accepted
  merlin single:
  return
  []
  merlin server:
  return
  []
