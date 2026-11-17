#!/bin/sh

# usage: sh poll_link.sh
#
# Compiles the units of poll_link.ml with and without poll insertion, links
# them in the combinations below, and prints what each program says of its
# default number of domains. What the compilers print is shown only if one
# of them fails.

set -e
trap 'status=$?; [ $status = 0 ] || cat compilers.log' EXIT

# The default number of domains is the program's own, not one that the
# environment of the test sets.
unset OCAMLRUNPARAM

# The variants base and poll, whatever the default one is: each command
# below states its poll-insertion setting.
OCAMLPARAM=asan=0,frame-pointers=0,stack-checks=0,_
export OCAMLPARAM

ocamlopt () {
  $ocamlrun $ocamlopt_byte -alert -unsafe_multidomain-do_not_spawn_domains \
    "$@" 2>> compilers.log
}
with=-enable-poll-insertion
without=-disable-poll-insertion

# The units on their own in <setting>, and for the pack in <setting>-pack.
for setting in with without; do
  eval "flag=\$$setting"
  mkdir $setting $setting-pack
  for unit in poll_link_a poll_link_b poll_link; do
    ocamlopt $flag -I $setting -c -o $setting/$unit.cmx $unit.ml
  done
  for unit in poll_link_a poll_link_b; do
    ocamlopt $flag -for-pack Poll_link_p -I $setting-pack -c \
      -o $setting-pack/$unit.cmx $unit.ml
  done
done

# usage: link <description> <arguments of the link>
link () {
  description=$1
  shift
  ocamlopt -o program.exe "$@"
  echo "$description: $(./program.exe)"
}

for linked in with without; do
  eval "flag=\$$linked"
  link "units with polls, linked $linked" $flag \
    with/poll_link_a.cmx with/poll_link_b.cmx with/poll_link.cmx
  link "first unit without polls, linked $linked" $flag \
    without/poll_link_a.cmx with/poll_link_b.cmx with/poll_link.cmx
  link "second unit without polls, linked $linked" $flag \
    with/poll_link_a.cmx without/poll_link_b.cmx with/poll_link.cmx
  link "last unit without polls, linked $linked" $flag \
    with/poll_link_a.cmx with/poll_link_b.cmx without/poll_link.cmx
  link "units without polls, linked $linked" $flag \
    without/poll_link_a.cmx without/poll_link_b.cmx without/poll_link.cmx
done

ocamlopt $without -a -o with.cmxa with/poll_link_a.cmx with/poll_link_b.cmx
ocamlopt $with -a -o first.cmxa without/poll_link_a.cmx with/poll_link_b.cmx
ocamlopt $with -a -o second.cmxa with/poll_link_a.cmx without/poll_link_b.cmx
link "archive with polls, linked with" $with with.cmxa with/poll_link.cmx
link "archive with polls, linked without" $without with.cmxa with/poll_link.cmx
link "archive, first unit without polls, linked with" $with \
  first.cmxa with/poll_link.cmx
link "archive, second unit without polls, linked with" $with \
  second.cmxa with/poll_link.cmx

# usage: pack <directory> <units of the pack>
pack () {
  directory=$1
  shift
  mkdir $directory
  ocamlopt $with -pack -o $directory/poll_link_p.cmx "$@"
  ocamlopt $with -I $directory -c -o $directory/poll_link_packed.cmx \
    poll_link_packed.ml
}

pack packed-with with-pack/poll_link_a.cmx with-pack/poll_link_b.cmx
pack packed-first without-pack/poll_link_a.cmx with-pack/poll_link_b.cmx
pack packed-second with-pack/poll_link_a.cmx without-pack/poll_link_b.cmx
link "pack with polls, linked with" $with \
  packed-with/poll_link_p.cmx packed-with/poll_link_packed.cmx
link "pack with polls, linked without" $without \
  packed-with/poll_link_p.cmx packed-with/poll_link_packed.cmx
link "pack, first unit without polls, linked with" $with \
  packed-first/poll_link_p.cmx packed-first/poll_link_packed.cmx
link "pack, second unit without polls, linked with" $with \
  packed-second/poll_link_p.cmx packed-second/poll_link_packed.cmx
