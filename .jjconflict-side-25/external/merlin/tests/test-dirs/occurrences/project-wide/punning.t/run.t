Test project-wide occurrences in the presence of punning (both let and record punning)

Compile project, create index file, and configure Merlin to use index file
  $ $OCAMLC -bin-annot -bin-annot-occurrences -c definitions.ml usages.ml
  $ ocaml-index aggregate definitions.cmt usages.cmt
  $ cat > .merlin << EOF
  > INDEX project.ocaml-index
  > EOF

Convenience function for querying occurrences
  $ occurrences () {
  >   file="$1"
  >   location="$2"
  >   $MERLIN single occurrences -scope project -identifier-at "$location" -filename "$file" < "$file" | \
  >     jq -r '.value[] | "\(.file) \(.start.line):\(.start.col)-\(.end.col)"'
  > }

Get occurrences of an identifier that is used as the expression part of a punned let
expression
  $ occurrences definitions.ml 1:4
  $TESTCASE_ROOT/definitions.ml 1:4-5
  $TESTCASE_ROOT/usages.ml 6:7-8

Get occurrences, with the cursor pointing at the identifier in a punned let.
Merlin returns the occurrences of the new variable bound in that let, rather than the
expression being assigned to the variable.
  $ occurrences usages.ml 6:7
  $TESTCASE_ROOT/usages.ml 6:7-8
  $TESTCASE_ROOT/usages.ml 7:7-8

Get occurrences of a record field, where there is an instance of punning that field while
creating a record
  $ occurrences definitions.ml 3:13
  $TESTCASE_ROOT/definitions.ml 3:11-16
  $TESTCASE_ROOT/usages.ml 10:10-15

Get occurrences of a variable that is used as the value being placed into a record in a
punned record field expression
  $ occurrences definitions.ml 4:6
  $TESTCASE_ROOT/definitions.ml 4:4-9
  $TESTCASE_ROOT/usages.ml 10:10-15

Get occurrences, with the cursor pointing at a punned record field expression.
Merlin finds occurrences of the value being placed into the record rather than the record
field
  $ occurrences usages.ml 10:12
  $TESTCASE_ROOT/usages.ml 10:10-15
  $TESTCASE_ROOT/definitions.ml 4:4-9
