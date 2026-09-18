This test ensures that Merlin properly recovers from a escape-region error.

  $ cat >local1.ml <<EOF
  > let foo _ = local_ failwith "unimplemented"
  > let bar x = foo x
  > EOF

  $ $MERLIN single locate -position 2:15 \
  > -filename local1.ml < local1.ml | jq '.value.pos'
  {
    "line": 1,
    "col": 4
  }

  $ cat >local2.ml <<EOF
  > let () =
  >   let local_ local_value = "foo" in
  >   let x = ref "bar" in
  >   x := local_value
  > ;;
  > EOF

  $ $MERLIN single locate -position 4:13 \
  > -filename local2.ml < local2.ml | jq '.value.pos'
  {
    "line": 2,
    "col": 13
  }
