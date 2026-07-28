#!/bin/sh

sed -r 's/caml(.*)_[0-9]+_[0-9]+_code?/caml\1_HIDE_STAMP/'
