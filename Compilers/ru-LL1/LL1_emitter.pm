#: LL1_emitter.pm
#: code emitter for ru::LL1

package LL1::Emitter;

use strict;
use warnings;

use LL1_table;
use Data::Dumper;
use Template;

sub emit {
    my ($self, $ast, $filetype, $package) = @_;
    $filetype ||= 'pl';
    $package  ||= 'Parser';
    my $Firsts = LL1::Table::first_sets($ast);
    my $Follows = LL1::Table::follow_sets($ast, $Firsts);
    my $table = LL1::Table::LL1_table($ast, $Firsts, $Follows);
    #$Data::Dumper::Indent = 1;
    $Data::Dumper::Purity = 1;
    my $data =
        Data::Dumper->Dump(
            [$table, $X::tokens],
            ["${package}::table", "X::tokens"]);
    #warn $s;
    my $tt_ast = {
        data      => $data,
        package   => $package,
        filetype  => $filetype,
        startrule => $ast->{startrule},
    };
    my $tt = Template->new;
    my $buffer;
    $tt->process(\*DATA, $tt_ast, \$buffer)
        or die $tt->error(), "\n";
    $buffer;
}

1;

__DATA__

package main;

our $LL1_TRACE = undef;   # default off

package X;

our ($str, $pos);

package [% package %];

use strict;
use warnings;

use LL1_runtime;

[% data -%]

sub new {
    my $class = shift;
    $class;
}

sub parse {
    my ($self, $text) = @_;
    $X::str = $text;
    $X::pos = 0;
    LL1::Runtime::eval_table($[% package %]::table, '[% startrule %]');
}

[%- IF filetype == 'pm' %]
1;
[%- ELSE %]
package main;

use strict;
use warnings;

use Data::Dumper;
use Getopt::Std;

my %opts;
getopts('d', \%opts);

local $/;
my $src = <>;
die "No input source code.\n" if !defined $src;

my $parser = [% package %]->new;
my $ast;
if ($opts{d}) {
    $::LL1_TRACE = 1;
    $ast = $parser->parse($src);
    if (defined $ast) {
        print "\nsuccess.\n";
    } else {
        warn "$LL1::Runtime::Error at offset $X::pos.\n";
        print "\nfail.\n";
    }
} else {
    $::Data::Dumper::Indent = 1;
    $ast = $parser->parse($src);
    if (!defined $ast) {
        warn "$LL1::Runtime::Error at offset $X::pos.\n";
        exit(1);
    } elsif (ref $ast) {
        print Data::Dumper->Dump([$ast], ['AST']);
    } else {
        print $ast, "\n";
    }
}
[%- END %]
