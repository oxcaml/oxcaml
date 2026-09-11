open Ppxlib;;

Expansion_helpers.mangle (Prefix "pre") "foo";;
[%%expect{|
- : string = "pre_foo"
|}]

Expansion_helpers.mangle (Suffix "suf") "foo";;
[%%expect{|
- : string = "foo_suf"
|}]

Expansion_helpers.mangle (PrefixSuffix ("pre", "suf")) "foo";;
[%%expect{|
- : string = "pre_foo_suf"
|}]

Expansion_helpers.mangle (Prefix "pre") "t";;
[%%expect{|
- : string = "pre"
|}]

Expansion_helpers.mangle (Suffix "suf") "t";;
[%%expect{|
- : string = "suf"
|}]

Expansion_helpers.mangle (PrefixSuffix ("pre", "suf")) "t";;
[%%expect{|
- : string = "pre_suf"
|}]

Expansion_helpers.mangle ~fixpoint:"foo" (Prefix "pre") "foo";;
[%%expect{|
- : string = "pre"
|}]

Expansion_helpers.mangle ~fixpoint:"foo" (Suffix "suf") "foo";;
[%%expect{|
- : string = "suf"
|}]

Expansion_helpers.mangle ~fixpoint:"foo" (PrefixSuffix ("pre", "suf")) "foo";;
[%%expect{|
- : string = "pre_suf"
|}]
