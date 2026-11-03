#!/usr/bin/perl
# review-pdf.pl BUILD_DIR OUT.pdf FILE.v ...
#
# Typesets what a reviewer of the Rocq development reads as one PDF, in the
# shape of a paper. The first part is the ground the passes share: every
# reviewed .v file of the stdlib, backend and SSA libraries (the definitions
# and the axioms; the implementation and Proofs files are left out). Then
# comes one part per pass, with its sections of ROCQ-REVIEW-GUIDE.md as the
# narrative and its statements; then the extraction directives and the
# axioms the statements rest on. `rocq doc` typesets the sources, with every
# identifier linked to its definition; LuaLaTeX typesets the document.
#
# Run it as `make -C rocq review`, which passes the files of the four
# libraries in dependency order. It needs the .vo and .glob files of a
# complete build, `rocq doc` and `lualatex` on PATH, and a TeX installation
# with fontspec; the fonts fall back to Latin Modern when Libertinus and
# DejaVu Sans Mono are not installed. REVIEW_AUTHOR in the environment sets
# the author line.

use strict;
use warnings;
use utf8;
use open qw(:std :encoding(UTF-8));
use Cwd qw(abs_path);
use Digest::MD5 qw(md5_hex);
use File::Basename qw(basename dirname);
use File::Copy qw(copy);
use File::Path qw(make_path);
use File::Spec;
use POSIX qw(strftime);

my ($build, $out, @files) = @ARGV;
die "usage: review-pdf.pl BUILD_DIR OUT.pdf FILE.v ...\n" unless @files;
my $root = abs_path(dirname(abs_path($0)) . '/..');
$build = File::Spec->rel2abs($build);
$out = File::Spec->rel2abs($out);
make_path($build, "$build/src", "$build/tex");

# ---- The libraries, as rocq/Makefile maps them ------------------------

my @libraries = (
  { dir => 'rocq', name => 'OxCamlStdlib',
    title => "Models of OxCaml's own types",
    intro => <<'TEX' },
The analyses compute with OxCaml's own types: tagged 63-bit integers
(\texttt{int}, modelled by Rocq's primitive 63-bit integers), boxed 64-bit
integers (\texttt{Int64}, an abstract type with axioms relating it to the
integers), lists, and options standing for exceptions. These files hold the
definitions and the axioms only; the lemmas proved about them are in the
\texttt{*Proofs.v} files and need no review. Together with the extraction
directives of Appendix~\ref{app:extraction} they are what the development
takes on trust about OxCaml.
TEX
  { dir => 'backend/rocq', name => 'OxCamlBackend',
    title => 'Backend-wide definitions',
    intro => <<'TEX' },
Definitions shared with the rest of the backend: the meaning of the integer
comparisons as propositions over the integers, and the model's copy of the
backend's \texttt{Operation.t}, which must match the OCaml type constructor
for constructor.
TEX
  { dir => 'backend/ssa/rocq', name => 'OxCamlSsa',
    title => 'The SSA graph and its semantics',
    intro => <<'TEX' },
The SSA graph as the models see it: abstract blocks and values with the few
observations the analyses make of them, the properties of terminators,
edges and dominance that any pass may rely on, and the execution semantics
the statements speak of. Each \texttt{*Spec.v} file holds the definitions
the statements use; the implementation file of the same name without the
suffix is the extracted check and is not shown, nor is the
\texttt{*Proofs.v} file.
TEX
  { dir => 'backend/ssa/loop/rocq', name => 'OxCamlLoop' },
);
my %library = map { $_->{dir} => $_ } @libraries;

# ---- The parts of the document ------------------------------------------

# A part either shows whole libraries, in dependency order, or is about one
# pass: it takes the sections of the review guide whose headings match
# [guide] (the first part whose pattern matches wins) and the Spec files
# whose names match [sources], introduced by [statements]. Every reviewed
# file and every section of the guide must land in a part.
my @parts = (
  { title => 'The shared models',
    libraries => [qw(OxCamlStdlib OxCamlBackend OxCamlSsa)],
    intro => <<'TEX' },
The files of this part are the ground every pass stands on: what the
development takes on trust about OxCaml, and the SSA graph with the
execution semantics that the statements of the following parts speak of.
None of it is about any one pass. A reader who wants the claims first may
begin with the next part and return here for the definitions.
TEX
  { title => 'Empty-loop deletion',
    guide => qr/./,
    sources => qr/^(?:DeleteEmptyLoops|LoopEligibility)Spec$/,
    statements => <<'TEX' },
What is claimed about the extracted decision \texttt{find\_deletions}. The
analyses themselves are extracted code, and their \texttt{*Proofs.v} files
give their meaning and prove the statements; neither is shown.
TEX
  { title => 'Bounds-check elimination',
    sources => qr/^BoundsCheckExecutionSpec$/,
    statements => <<'TEX' },
What is claimed about the extracted decision \texttt{find\_eliminations}.
The analysis itself is extracted code, and its \texttt{*Proofs.v} files give
its meaning and prove the statement; neither is shown.
TEX
);
# The guide numbers its sections by hand, and the sources refer to them by
# number; the references follow the sections wherever the document puts them.
my (%guide_label, @guide_references);
my %part_of_library;
for my $part (@parts) {
  $part_of_library{$_} = $part for @{ $part->{libraries} // [] };
}

# ---- Which files a reviewer reads --------------------------------------

my (@sources, @extracts, %by_base);
for my $file (@files) {
  my $abs = File::Spec->rel2abs($file);
  my $rel = File::Spec->abs2rel($abs, $root);
  my ($dir, $base) = (dirname($rel), basename($rel, '.v'));
  my $lib = $library{$dir}
    or die "$rel: not in a directory rocq/Makefile maps to a library\n";
  next if $base =~ /Proofs$/;
  my $entry = { path => $abs, rel => $rel, base => $base, lib => $lib };
  if ($base =~ /Extract$/ || $base eq 'Extract') {
    push @extracts, $entry;
  } else {
    # In the SSA and loop libraries, a module with a Proofs file is an
    # extracted implementation; its reviewed part is its Spec file.
    next if $lib->{name} =~ /^OxCaml(?:Ssa|Loop)$/
      && -e "$root/$dir/${base}Proofs.v";
    my ($part) = $part_of_library{$lib->{name}}
      // grep { $_->{sources} && $base =~ $_->{sources} } @parts;
    die "$rel: no part of the document shows it\n" unless $part;
    $entry->{part} = $part;
    push @sources, $entry;
  }
  die "two reviewed files are both called $base.v\n" if $by_base{$base};
  $by_base{$base} = $entry;
}
die "no reviewed files among the arguments\n" unless @sources;

# ---- Preparing a source for rocq doc -----------------------------------

# The copy keeps every byte offset, so that the .glob file, which refers to
# them, stays valid once its digest is updated: the licence header is
# blanked, banner comments become section titles and own-line comments
# become documentation comments.
sub prepare {
  my ($entry) = @_;
  my $dst = "$build/src/$entry->{lib}{name}";
  make_path($dst);
  open my $in, '<:raw', $entry->{path} or die "$entry->{rel}: $!\n";
  my @l = <$in>;
  close $in;
  my $orig = join '', @l;
  my $blank = sub { $l[$_[0]] = (' ' x (length($l[$_[0]]) - 1)) . "\n" };
  my $title = sub {
    my ($i, $indent, $text) = @_;
    my $len = length($l[$i]) - 1;
    my $new = "$indent(** *** $text";
    return 0 if length($new) + 3 > $len;
    $l[$i] = $new . (' ' x ($len - length($new) - 3)) . " *)\n";
    return 1;
  };
  if (@l > 26 && $l[0] =~ /^\(\*{70,}$/ && $l[25] =~ /^ \*{70,}\)$/) {
    $blank->($_) for 0 .. 25;
  }
  for my $i (0 .. $#l) {
    if ($i + 2 <= $#l && $l[$i] =~ /^\s*\(\* =+ \*\)\s*$/
        && $l[$i + 2] =~ /^\s*\(\* =+ \*\)\s*$/
        && $l[$i + 1] =~ /^(\s*)\(\* +(\S.*?) +\*\)\s*$/) {
      if ($title->($i + 1, $1, $2)) { $blank->($i); $blank->($i + 2); }
    } elsif ($l[$i] =~ /^(\s*)\(\* -{3,} (\S.*?) -{3,} \*\)\s*$/) {
      $title->($i, $1, $2);
    }
  }
  my $text = join '', @l;
  # Own-line comments become documentation comments. rocq doc wants a space
  # after the opener; it is taken from before the closer.
  $text =~ s{^([ \t]*)\(\* (?!(?:begin|end) hide \*\))(.*?) \*\)}
            {$1(** $2*)}msg;
  die "$entry->{rel}: the preparation changed the file's length\n"
    unless length($text) == length($orig);
  open my $o, '>:raw', "$dst/$entry->{base}.v" or die "$dst: $!\n";
  print $o $text;
  close $o;
  (my $glob = $entry->{path}) =~ s/\.v$/.glob/;
  if (-e $glob) {
    open my $g, '<:raw', $glob or die "$glob: $!\n";
    my @g = <$g>;
    close $g;
    $g[0] =~ s/^DIGEST \S+/'DIGEST ' . md5_hex($text)/e;
    open my $go, '>:raw', "$dst/$entry->{base}.glob" or die "$dst: $!\n";
    print $go @g;
    close $go;
  } else {
    warn "$entry->{rel}: no .glob file (run make -C rocq all first);"
      . " its identifiers will not be linked\n";
  }
  return "$dst/$entry->{base}.v";
}

my @inputs = map { prepare($_) } @sources, @extracts;
my @mappings = map { ('-Q', "$build/src/$_->{name}", $_->{name}) }
  grep { -d "$build/src/$_->{name}" } @libraries;

# ---- rocq doc -----------------------------------------------------------

# Comments that stay regular ones (not on lines of their own) are shown as
# comments rather than dropped.
my @rocqdoc = ('rocq', 'doc', '--latex', '-g', '--utf8', '-s', '--no-index',
  '--no-externals', '--parse-comments', @mappings);
system(@rocqdoc, '--body-only', '-d', "$build/tex", @inputs) == 0
  or die "rocq doc failed\n";
# A complete document is generated once, only for the coqdoc.sty it writes.
make_path("$build/sty");
system(@rocqdoc, '-d', "$build/sty", '-o', "$build/sty/style.tex",
  $inputs[0]) == 0 or die "rocq doc failed\n";
copy("$build/sty/coqdoc.sty", "$build/coqdoc.sty")
  or die "rocq doc did not write coqdoc.sty\n";

# Every definition in the document, so that links to definitions outside it
# (in implementation files, or Rocq's library) can be dropped; then the
# fragments are tidied.
my (%fragment, %defined);
for my $entry (@sources, @extracts) {
  my $file = "$build/tex/$entry->{lib}{name}.$entry->{base}.tex";
  open my $t, '<', $file or die "$file: rocq doc wrote no such file\n";
  local $/;
  my $s = <$t>;
  close $t;
  $defined{$1} = 1 while $s =~ /\\coqdef\{([^}]*)\}/g;
  $fragment{$entry->{base}} = $s;
}
my $braced = qr/(?:[^{}]|\{(?:[^{}]|\{[^{}]*\})*\})*/;
for my $base (keys %fragment) {
  my $s = $fragment{$base};
  $s =~ s/\A(\\begin\{coqdoccode\}\n)(?:\\coqdocemptyline\n)+/$1/;
  $s =~ s/\\coqref\{([^}]*)\}\{($braced)\}/
    $defined{$1} ? "\\coqref{$1}{$2}" : $2/ge;
  $s =~ s/(ROCQ-REVIEW-GUIDE\.md, )section (\d+(?:\.\d+)*)(?!\d)/
    push @guide_references, $2; "$1Section~\\ref{guide:$2}"/ge;
  open my $t, '>', "$build/tex/$base.tex" or die;
  print $t $s;
  close $t;
}

# ---- The statements and their theorems ---------------------------------

my @statements;
for my $entry (@sources) {
  next unless $entry->{base} =~ /^(.*)Spec$/;
  my $proofs = "$1Proofs";
  open my $v, '<', $entry->{path} or die;
  while (<$v>) {
    next unless /^Definition ([a-z_0-9]+)_statement : Prop/;
    push @statements, { name => $1, spec => $entry->{base}, proofs => $proofs,
      lib => $entry->{lib}{name}, entry => $entry };
  }
  close $v;
}

# ---- LaTeX helpers ------------------------------------------------------

sub escape {
  my ($s) = @_;
  $s =~ s/\\/\x01/g;
  $s =~ s/([{}&#\$%_])/\\$1/g;
  $s =~ s/~/\\textasciitilde{}/g;
  $s =~ s/\^/\\textasciicircum{}/g;
  $s =~ s/\x01/\\textbackslash{}/g;
  return $s;
}

# The label rocq doc gives a definition: underscores become spaces.
sub coq_label {
  my ($lib, $module, $name) = @_;
  (my $l = "$lib.$module.$name") =~ s/_/ /g;
  return "coq:$l";
}

sub code_span {
  my ($text, $in_heading) = @_;
  my $e = escape($text);
  # A path or a qualified name may break after a separator.
  $e =~ s{(/|\.|\\_|-)}{$1\\allowbreak{}}g unless $in_heading;
  my $tt = "\\texttt{$e}";
  $tt = "\\texorpdfstring{$tt}{" . escape($text) . '}' if $in_heading;
  if (!$in_heading && $text =~ m{^(?:[\w/]+/)?(\w+)\.v$} && $by_base{$1}) {
    return "\\hyperref[src:$1]{$tt}";
  }
  return $tt;
}

# Markdown inline text (code spans, emphasis) to LaTeX.
sub inline {
  my ($s, $in_heading) = @_;
  my @code;
  $s =~ s/`([^`]+)`/push @code, $1; "\x02" . $#code . "\x03"/ge;
  $s = escape($s);
  $s =~ s/\*\*(.+?)\*\*/\\textbf{$1}/g;
  $s =~ s/(?<![\w\\])\*(?=\S)(.+?)(?<=\S)\*(?!\w)/\\emph{$1}/g;
  $s =~ s/\bsection (\d+(?:\.\d+)*)(?!\d)/
    push @guide_references, $1; "Section~\\ref{guide:$1}"/ge;
  $s =~ s/\x02(\d+)\x03/code_span($code[$1], $in_heading)/ge;
  return $s;
}

sub table_env {
  return system('kpsewhich xltabular.sty >/dev/null 2>&1') == 0
    ? 'xltabular' : 'tabularx';
}
my $table_env = table_env();

sub table {
  my (@rows) = @_;
  my @cells = map {
    my $r = $_;
    $r =~ s/`([^`]*)`/my $c = $1; $c =~ s{\|}{\x04}g; "`$c`"/ge;
    $r =~ s/^\s*\|//;
    $r =~ s/\|\s*$//;
    [ map { s/\x04/|/g; inline($_) } split /\s*\|\s*/, $r ];
  } @rows;
  my $header = shift @cells;
  shift @cells;    # the separator row
  my $n = @$header;
  my $cols = join '', ('>{\raggedright\arraybackslash}X') x $n;
  my $body = join " \\\\\n", map { join ' & ', @$_ } @cells;
  my $head = join ' & ', map { "\\textbf{$_}" } @$header;
  return "\\begin{small}\n\\begin{$table_env}{\\textwidth}{\@{}$cols\@{}}\n"
    . "\\toprule\n$head \\\\\n\\midrule\n\\endhead\n$body \\\\\n"
    . "\\bottomrule\n\\end{$table_env}\n\\end{small}\n\n";
}

# ---- The review guide, from Markdown ------------------------------------

# The guide's title names the document; its text before the first section
# opens the first part that has guide sections; each section goes to the
# part whose [guide] pattern matches its heading.
my ($title, $preamble, @guide) = ('The Rocq development', '');
{
  my $path = "$root/ROCQ-REVIEW-GUIDE.md";
  open my $md, '<', $path or die "$path: $!\n";
  my @lines = <$md>;
  close $md;
  chomp @lines;
  my ($i, @para) = (0);
  my $sink = \$preamble;
  my $flush = sub {
    $$sink .= inline(join ' ', @para) . "\n\n" if @para;
    @para = ();
  };
  while ($i < @lines) {
    my $line = $lines[$i];
    if ($line =~ /^```/) {
      $flush->();
      my @code;
      $i++;
      push @code, $lines[$i++] while $i < @lines && $lines[$i] !~ /^```/;
      $i++;
      $$sink .= "\\begin{Verbatim}[fontsize=\\small,xleftmargin=1.5em]\n"
        . join("\n", @code) . "\n\\end{Verbatim}\n\n";
    } elsif ($line =~ /^(#{1,3}) (.*)$/) {
      $flush->();
      my ($level, $text) = (length $1, $2);
      my $number = $text =~ s/^(\d+(?:\.\d+)*)\.?\s+// ? $1 : undef;
      if ($level == 1) {
        $title = inline($text);
      } else {
        if ($level == 2) {
          push @guide, { heading => $text, tex => '' };
          $sink = \$guide[-1]{tex};
        }
        my $cmd = $level == 2 ? 'section' : 'subsection';
        $$sink .= "\\$cmd\{" . inline($text, 1) . "}\n";
        if (defined $number) {
          $$sink .= "\\label{guide:$number}\n";
          $guide_label{$number} = 1;
        }
      }
      $i++;
    } elsif ($line =~ /^\|/) {
      $flush->();
      my @rows;
      push @rows, $lines[$i++] while $i < @lines && $lines[$i] =~ /^\|/;
      $$sink .= table(@rows);
    } elsif ($line =~ /^(?:- |\d+\. )/) {
      $flush->();
      my $env = $line =~ /^-/ ? 'itemize' : 'enumerate';
      # An item is paragraphs and fenced code blocks, continued on indented
      # lines; a blank line followed by an indented line separates its
      # paragraphs.
      my (@items, $parts, $break);
      while ($i < @lines) {
        if ($lines[$i] =~ /^(?:- |\d+\. )(.*)$/) {
          push @items, $parts = [ [ 'text', $1 ] ];
          $break = 0;
          $i++;
        } elsif ($parts && $lines[$i] =~ /^(\s{2,})```/) {
          my $indent = $1;
          my @code;
          $i++;
          while ($i < @lines && $lines[$i] !~ /^\s*```\s*$/) {
            (my $c = $lines[$i++]) =~ s/^$indent//;
            push @code, $c;
          }
          $i++;
          push @$parts, [ 'code', join "\n", @code ];
          $break = 1;
        } elsif ($parts && $lines[$i] =~ /^\s{2,}(\S.*)$/) {
          if (!$break && $parts->[-1][0] eq 'text') {
            $parts->[-1][1] .= " $1";
          } else {
            push @$parts, [ 'text', $1 ];
          }
          $break = 0;
          $i++;
        } elsif ($parts && $lines[$i] =~ /^\s*$/ && $i + 1 < @lines
            && $lines[$i + 1] =~ /^\s{2,}\S/) {
          $break = 1;
          $i++;
        } else {
          last;
        }
      }
      my $body = '';
      for my $item (@items) {
        $body .= '\item ' . join("\n\n", map {
          $_->[0] eq 'text' ? inline($_->[1])
            : "\\begin{Verbatim}[fontsize=\\small,xleftmargin=1.5em]\n"
              . "$_->[1]\n\\end{Verbatim}" } @$item) . "\n";
      }
      $$sink .= "\\begin{$env}\n$body\\end{$env}\n\n";
    } elsif ($line =~ /^\s*$/) {
      $flush->();
      $i++;
    } else {
      push @para, $line;
      $i++;
    }
  }
  $flush->();
}
for my $n (sort keys %{ { map { $_ => 1 } @guide_references } }) {
  die "the guide refers to section $n, which it has not got\n"
    unless $guide_label{$n};
}
for my $section (@guide) {
  my ($part) = grep { $_->{guide} && $section->{heading} =~ $_->{guide} }
    @parts;
  die "no part of the document takes the guide's section"
    . " \"$section->{heading}\"\n" unless $part;
  push @{ $part->{guide_sections} }, $section;
}
my ($narrative) = grep { $_->{guide_sections} } @parts;

# ---- The axioms the statements rest on ----------------------------------

my @flags = map { ('-Q', "$root/$_->{dir}", $_->{name}) } @libraries;
my %axioms;
for my $st (@statements) {
  my $file = "$build/assumptions-$st->{name}";
  open my $v, '>', "$file.v" or die;
  print $v "From Stdlib Require Import ZArith List.\n";
  print $v "From $_->{lib}{name} Require Import $_->{base}.\n" for @sources;
  print $v "From $st->{lib} Require $st->{proofs}.\n",
    "Open Scope Z_scope.\nSet Printing Width 76.\n",
    "Print Assumptions $st->{proofs}.$st->{name}.\n";
  close $v;
  my $output = `rocq repl -q -batch @flags -l "$file.v" 2>&1`;
  die "rocq repl failed on $file.v:\n$output" if $?;
  $output =~ s/\A.*?^Axioms:\n//ms;
  $st->{listing} = $output;
  $st->{axioms} = { map { $_ => 1 } $output =~ /^(\S+) :/mg };
}
my ($main_statement) = sort {
  keys %{ $b->{axioms} } <=> keys %{ $a->{axioms} } } @statements;

# ---- The document -------------------------------------------------------

my $author = $ENV{REVIEW_AUTHOR} // 'The OxCaml developers';
my $commit = `git -C "$root" rev-parse --short HEAD 2>/dev/null`;
chomp $commit;
my $dirty = `git -C "$root" status --porcelain -- ROCQ-REVIEW-GUIDE.md @{[
  map { "$root/$_->{dir}" } @libraries ]} 2>/dev/null`;
my $date = strftime('%e %B %Y', localtime);
$date =~ s/^\s+//;
my $provenance = $commit
  ? "commit \\texttt{$commit}" . ($dirty ? ' with uncommitted changes' : '')
  : 'the working tree';

sub source_section {
  my ($entry) = @_;
  my $name = escape("$entry->{base}.v");
  return "\\subsection{\\texorpdfstring{\\texttt{$name}}{$name}}\n"
    . "\\label{src:$entry->{base}}\n"
    . "{\\small\\color{black!55}\\texttt{" . escape($entry->{rel}) . "}}\n\n"
    . "\\input{tex/$entry->{base}.tex}\n\n";
}

sub statements_table {
  my (@list) = @_;
  return '' unless @list;
  my $table = "\\begin{center}\\begin{footnotesize}\n"
    . "\\begin{tabular}{\@{}lll\@{}}\n\\toprule\n"
    . "\\textbf{Statement} & \\textbf{Stated in} & \\textbf{Proved in}"
    . " \\\\\n\\midrule\n";
  for my $st (@list) {
    my $label = coq_label($st->{lib}, $st->{spec}, "$st->{name}_statement");
    $table .= "\\hyperlink{$label}{\\texttt{"
      . escape("$st->{name}_statement") . "}} & \\hyperref[src:$st->{spec}]"
      . "{\\texttt{" . escape("$st->{spec}.v") . "}} & \\texttt{"
      . escape("$st->{proofs}.v") . "} \\\\\n";
  }
  return "$table\\bottomrule\n\\end{tabular}\n"
    . "\\end{footnotesize}\\end{center}\n";
}

my $axioms_text = '';
if ($main_statement) {
  my $n = keys %{ $main_statement->{axioms} };
  $axioms_text .= "\\texttt{" . escape($main_statement->{name})
    . "} rests on $n axioms: the \\texttt{Axiom} and \\texttt{Parameter}"
    . " declarations of the files above, and the axioms of Rocq's standard"
    . " library about its primitive integers. They are listed below as"
    . " \\texttt{Print Assumptions} prints them. Nothing is admitted.\n\n";
  for my $st (@statements) {
    next if $st == $main_statement;
    my @extra = sort grep { !$main_statement->{axioms}{$_} }
      keys %{ $st->{axioms} };
    my $m = keys %{ $st->{axioms} };
    $axioms_text .= "\\texttt{" . escape($st->{name}) . "} rests on $m"
      . (@extra ? " axioms, among them these further ones: \\texttt{"
          . join('}, \texttt{', map { escape($_) } @extra) . "}.\n\n"
        : " of them.\n\n");
  }
  $axioms_text .= "\\begin{Verbatim}[fontsize=\\footnotesize]\n"
    . $main_statement->{listing} . "\\end{Verbatim}\n";
}

my $unicode = join '',
  map { "\\newunicodechar{$_->[0]}{\\ensuremath{$_->[1]}}\n" }
  ['∀', '\forall'], ['∃', '\exists'], ['∈', '\in'], ['∉', '\notin'],
  ['∧', '\wedge'], ['∨', '\vee'], ['¬', '\neg'], ['→', '\rightarrow'],
  ['↔', '\leftrightarrow'], ['⇒', '\Rightarrow'], ['▷', '\triangleright'],
  ['≤', '\leq'], ['≥', '\geq'], ['≠', '\neq'], ['×', '\times'],
  ['∅', '\emptyset'], ['⊆', '\subseteq'], ['∘', '\circ'], ['λ', '\lambda'];

my $pdftitle = $title;
$pdftitle =~ s/\\[a-zA-Z]+\{([^}]*)\}/$1/g;
$pdftitle =~ s/[{}]//g;

# The parts about the passes, named in the abstract.
my @topics = map { lcfirst $_->{title} } grep { !$_->{libraries} } @parts;
my $topics = @topics == 1 ? "The second part is about $topics[0]"
  : 'The parts that follow are about '
    . join(', ', @topics[0 .. $#topics - 1]) . " and $topics[-1]; each";

my $conventions = <<'TEX';
\section{How the sources are shown}
Each statement is bound in a \texttt{*Spec.v} file as a proposition, a
\texttt{Definition} of type \texttt{Prop}, and proved under the same name
without the \texttt{\_statement} suffix in the matching \texttt{*Proofs.v}
file; \texttt{make -C rocq claims} checks that each theorem has exactly the
statement bound there. The sections of this part give, library by library
and in dependency order, every file the statements rest on: the definitions
they use, and the axioms and types that model the compiler's data. The
parts that follow give the statements of each pass. The typesetting is that
of \texttt{rocq doc}: keywords in bold, defined constants in green,
inductive types in purple, their constructors in brown, axioms and
parameters in slanted gold, and bound variables in italics. Clicking an
identifier leads to its definition when that definition is in this document.

TEX

my $main = <<"TEX";
\\documentclass[11pt,a4paper]{article}
\\usepackage[margin=26mm]{geometry}
\\usepackage{fontspec}
\\IfFontExistsTF{Libertinus Serif}{\\setmainfont{Libertinus Serif}}
  {\\setmainfont{Latin Modern Roman}}
\\IfFontExistsTF{Libertinus Sans}{\\setsansfont{Libertinus Sans}}
  {\\setsansfont{Latin Modern Sans}}
\\IfFontExistsTF{DejaVu Sans Mono}
  {\\setmonofont{DejaVu Sans Mono}[Scale=MatchLowercase]}
  {\\setmonofont{Latin Modern Mono}}
\\usepackage{amsmath,amssymb}
\\usepackage{newunicodechar}
$unicode\\usepackage{xcolor}
\\usepackage{booktabs,tabularx,longtable,array}
\\usepackage{fancyvrb}
\\usepackage{microtype}
\\usepackage{fancyhdr}
\\usepackage{coqdoc}
\\IfFileExists{xltabular.sty}{\\usepackage{xltabular}}{}
% coqdoc's underscore is a drawn rule; the font has one, and long identifiers
% occur in prose too, so it is a permitted line break.
\\def\\_{\\textunderscore\\allowbreak}
\\definecolor{linkblue}{HTML}{1A4A8A}
\\definecolor{kwcol}{HTML}{23407A}
\\definecolor{defcol}{HTML}{0B5D1E}
\\definecolor{indcol}{HTML}{6A1B9A}
\\definecolor{conscol}{HTML}{8B3A0F}
\\definecolor{axcol}{HTML}{8A6D00}
\\hypersetup{colorlinks=true,linkcolor=black,urlcolor=linkblue,
  pdftitle={$pdftitle},pdfauthor={$author}}
\\renewcommand{\\coqdockw}[1]{{\\ttfamily\\bfseries\\color{kwcol}#1}}
\\renewcommand{\\coqdocvar}[1]{{\\ttfamily\\itshape #1}}
\\renewcommand{\\coqdoccst}[1]{{\\ttfamily\\color{defcol}#1}}
\\renewcommand{\\coqdocind}[1]{{\\ttfamily\\color{indcol}#1}}
\\renewcommand{\\coqdocconstr}[1]{{\\ttfamily\\color{conscol}#1}}
\\renewcommand{\\coqdocax}[1]{{\\ttfamily\\slshape\\color{axcol}#1}}
\\renewcommand{\\coqdocmod}[1]{{\\ttfamily\\bfseries #1}}
\\renewcommand{\\coqdoctac}[1]{{\\ttfamily #1}}
\\renewenvironment{coqdoccode}
  {\\par\\addvspace{0.5\\baselineskip}\\small\\ttfamily\\raggedright
   \\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0pt}}
  {\\par\\addvspace{0.5\\baselineskip}}
\\renewenvironment{coqdoccomment}{\\itshape\\color{black!60}(*}{*)}
\\setcounter{secnumdepth}{2}
\\setcounter{tocdepth}{2}
\\setlength{\\emergencystretch}{3em}
\\pagestyle{fancy}
\\fancyhf{}
\\fancyhead[L]{\\small\\nouppercase{\\leftmark}}
\\fancyhead[R]{\\small\\thepage}
\\renewcommand{\\headrulewidth}{0.2pt}
\\title{$title}
\\author{$author}
\\date{$date\\\\[0.5ex]\\small Generated from $provenance}
\\begin{document}
\\maketitle
\\begin{abstract}
Some passes of the OxCaml SSA backend take their decisions from OCaml
extracted from a Rocq development, and the development proves those
decisions sound. This document collects what a reviewer of the development
reads. The first part is the ground the passes share, typeset from the
sources with the implementation and the proofs left out: the models of
OxCaml's own types, the backend definitions, and the SSA graph with its
execution semantics. $topics gives the review guide for the pass, which
says what is claimed, how the proof is put together, how the execution of
SSA code is reasoned about, and what remains to be checked by inspection
because no proof covers it, followed by the statements themselves. Every
use of an identifier is a link to its definition. The appendices give the
extraction directives and the axioms the statements rest on. Nothing in the
Proofs files or in the extracted implementations appears here: Rocq checks
those, and the statements do not mention them.
\\end{abstract}
\\tableofcontents
TEX

for my $part (@parts) {
  my @in_part = grep { $_->{part} == $part } @sources;
  my @sections = @{ $part->{guide_sections} // [] };
  next unless @in_part || @sections;
  $main .= "\\clearpage\n\\part{$part->{title}}\n" . ($part->{intro} // '');
  $main .= $preamble if $narrative && $part == $narrative;
  $main .= $_->{tex} for @sections;
  if ($part->{libraries}) {
    $main .= $conventions;
    for my $lib (@libraries) {
      my @in_lib = grep { $_->{lib} == $lib } @in_part;
      next unless @in_lib;
      $main .= "\\section{$lib->{title}}\n$lib->{intro}\n";
      $main .= source_section($_) for @in_lib;
    }
  } elsif (@in_part) {
    $main .= "\\section{The statements}\n" . ($part->{statements} // '')
      . statements_table(grep { $_->{entry}{part} == $part } @statements);
    $main .= source_section($_) for @in_part;
  }
}

$main .= "\\appendix\n\\section{The extraction directives}\n"
  . "\\label{app:extraction}\n"
  . "The extraction directives map the models' types and functions onto"
  . " OxCaml's own, and name the functions to extract. They are trusted:"
  . " a wrong directive changes the extracted code without any proof"
  . " noticing, so each must be compared with the OCaml it names.\n\n";
$main .= source_section($_) for @extracts;
$main .= "\\section{The axioms the statements rest on}\n\\label{app:axioms}\n"
  . "$axioms_text\n\\end{document}\n";

open my $tex, '>', "$build/main.tex" or die;
print $tex $main;
close $tex;

# Two runs settle the table of contents and the links; a third is needed
# when the contents moved a page.
for my $run (1 .. 4) {
  system('lualatex', '-interaction=nonstopmode', '-halt-on-error',
    "-output-directory=$build", 'main.tex') == 0
    or die "lualatex failed; see $build/main.log\n";
  open my $log, '<', "$build/main.log" or die;
  my $rerun = grep { /Rerun to get/ } <$log>;
  close $log;
  last unless $rerun;
}
copy("$build/main.pdf", $out) or die "cannot write $out: $!\n";
print "wrote $out\n";
