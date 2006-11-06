# extended prolog compiler

use strict;
use warnings;

use File::Slurp;
use Parse::RecDescent;
use Data::Dump::Streamer;

our $infile = shift or
    die "usage: $0 infile\n";
(my $outfile = $infile) =~ s/\.xpro$/.pro/i;
$outfile .= '.pro' if $outfile !~ /\.pro$/i;

my $source = read_file($infile);

my $grammar = <<'END_GRAMMAR';

program : statement(s) eofile { (join "\n\n", @{ $item[1] }). "\n" }
        | <error>

eofile : /^\Z/

statement : rule
          | comment

comment : /\/\*.*?\*\//s

rule : lhs ':-' <commit> compound '.' /[\n\s]*/

        { "/* line $itempos[1]{line}{from} - line $itempos[5]{line}{to} */\n".
          "$item[1] :- $item{compound}." }

     | compound '=>' <commit> conjunction '.' /[\n\s]*/
     
            { my $conj = $item{conjunction};
              my @s;
              for my $term (@$conj) {
                  push @s, "/* line $itempos[1]{line}{from} - ".
                      "line $itempos[5]{line}{to} */\n".
                      "$term :-" . $item{compound} . ".";
              }
              join "\n\n", @s;
            }

     | clause '.'
     
        { "/* line $itempos[1]{line}{from} - ".
          "line $itempos[2]{line}{to} */\n".
          "$item{clause}.\n" }

     | <error?> <reject>

lhs : clause
    | ''

conjunction : <leftop: clause /,/ clause>

clause : predicate
       | atom

predicate : identifier '(' <commit> arguments ')'  { "$item[1]($item{arguments})" }
          | atom '\\==' atom  { join ' ', $item[1], '\\==', $item[3]; }
          | atom operator atom  { "relation($item[1], $item[3], $item[2])" }
          | <error?> <reject>

arguments : <leftop: clause ',' clause>  { join ', ', @{ $item[1] } }

atom: '!'
    | '[' <commit> arguments ']'  { "[$item{arguments}]" }
    | { extract_delimited($text, '"') }
    | sigil(?) identifier

    { my $sigil = $item[1];
      @$sigil ? "$sigil->[0]($item[2])" : $item[2]; }

    | <error?> <reject>

identifier : /\w(?:\w|-\w)*/

sigil: '#'   { 'plane'  }
     | '\\'  { 'line'   }
     | '@'   { 'vector' }

operator: 'T'   { 'orthogonal'    }
        | '!T'  { 'nonorthogonal' }
        | 'A'   { 'oblique'       }
        | '!A'  { 'nonoblique'    }
        | '//'  { 'parallel'      }
        | '!//' { 'unparallel'    }
        | 'on'  { 'on'            }
        | '!on' { 'not_on'        }

compound: <leftop: clause /([;,])/ clause>

    { "\n    " . join "", (map { m/^[;,]$/ ? "$_\n    " : $_ } @{ $item[1] }); }

END_GRAMMAR

$::RD_HINT = 1;
#$::RD_TRACE = 1;
my $parser = new Parse::RecDescent ($grammar);
my $data = $parser->program($source);
if ($data) {
    write_file($outfile, $data);
}
