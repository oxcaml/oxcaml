/* Place the [hot] function of [Moved] ahead of all other code, as OCamlFDO's
   linker scripts do for hot functions. */
SECTIONS
{
  .text.hot_first : { *(.text.caml.camlMoved__hot_*) }
}
INSERT BEFORE .text;
