Test occurrences in the presence of punning (both let and record punning)

Convenience function for querying occurrences
  $ occurrences () {
  >   location="$1"
  >   $MERLIN single occurrences -identifier-at "$location" -filename test.ml < test.ml | \
  >     jq -r '.value[] | "\(.start.line):\(.start.col)-\(.end.col)"'
  > }

Get occurrences of an identifier that is used as the expression part of a punned let
expression
  $ occurrences 4:6
  4:6-7
  5:7-8

Get occurrences, with the cursor pointing at the identifier in a punned let.
Merlin returns the occurrences of the new variable bound in that let, rather than the
expression being assigned to the variable.
  $ occurrences 5:7
  5:7-8
  6:7-8

Get occurrences of an identifier that was defined in a punned let expression
  $ occurrences 5:7
  5:7-8
  6:7-8

Get occurrences of a record field, where there is an instance of punning that field while
creating a record
  $ occurrences 9:13
  9:11-16
  11:10-15

Get occurrences of a variable that is used as the value being placed into a record in a
punned record field expression
  $ occurrences 10:4
  10:4-9
  11:10-15

Get occurrences, with the cursor pointing at a punned record field expression.
Merlin finds occurrences of the value being placed into the record rather than the record
field
  $ occurrences 10:4
  10:4-9
  11:10-15
