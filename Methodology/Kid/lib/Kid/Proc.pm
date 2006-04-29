#: Kid::Proc
#: Copyright (c) 2006 Agent Zhang
#: 2006-04-27 2006-04-29

package Kid::Proc;

use strict;
use warnings;

use Kid;
use Kid::Perl;
use Language::AttributeGrammar;
#use Data::Dumper::Simple;
use Clone;
use Data::Visitor::Callback;

our $GetGrammar;
our $SolveGrammar;
our $RenameGrammar;

sub translate {
    my $src = shift;
    my $parser = Kid::Parser->new() or die "Can't construct the parser!\n";
    my $ptree = $parser->program($src) or return undef;
    my $ast = transform($ptree);
    return $ast->kid;
}

sub transform {
    my $ptree = shift;
    my $ast = read_proc($ptree);
    $ast = expand_proc($ast);
    rm_proc_decls($ast);
}

sub read_proc {
    my $ast = shift;
    $Kid::Proc::read_proc::G = Language::AttributeGrammar->new(<<'END_GRAMMAR');

program:    $/.ast = { program->new( $<statement_list>.ast ) }

statement_list: $/.ast = { statement_list->new( $<statement_list>.ast, $<statement>.ast ); }
statement:      $/.ast = { statement->new( $<child>.ast ); }

comment:  $/.ast = { nil->new; }

declaration:     $/.ast = { declaration->new( $<child>.ast ); }
proc_decl:       $/.ast = { Kid::Proc::emit_proc_decl( $<identifier>.ast, $<identifier_list>.ast, $<block>.ast ); }
identifier_list: $/.ast = { identifier_list->new( $<identifier_list>.ast, $<identifier>.ast ) }

if_statement: $/.ast = { if_statement->new( $<condition>.ast, $<statement>.ast, $<else_statement>.ast ); }

condition:    $/.ast = { condition->new( $<expression>.ast, $<rel_op>.ast, $<rhs_expression>.ast ); }
rel_op:       $/.ast = { rel_op->new( $<__VALUE__> ); }
rhs_expression:  $/.ast = { rhs_expression->new( $<expression>.ast ); }

block:           $/.ast = { block->new( $<statement_list>.ast ); }
else_statement:  $/.ast = { else_statement->new( $<statement>.ast ); }

assignment: $/.ast = { assignment->new( $<var>.ast, $<expression>.ast ); }

var:        $/.ast = { var->new( $<identifier>.ast ); }
identifier: $/.ast = { identifier->new( $<__VALUE__> ); }

expression: $/.ast = { expression->new( $<expression>.ast, $<op>, $<term>.ast ); }

expression_list: $/.ast = { expression_list->new( $<expression_list>.ast, $<expression>.ast ); }
term:       $/.ast = { term->new( $<neg>.ast, $<term>.ast, $<op>, $<factor>.ast ); }
neg:        $/.ast = { neg->new('-'); }
factor:     $/.ast = { factor->new( $<child>.ast ); }
number:     $/.ast = { number->new( $<__VALUE__> ); }
proc_call:  $/.ast = { Kid::Proc::emit_proc_call( $<identifier>.ast, $<expression_list>.ast ); }

nil:        $/.ast = { nil->new; }

END_GRAMMAR

    @Kid::Proc::calls = ();
    %Kid::Proc::decls = ();
    $ast = $Kid::Proc::read_proc::G->apply($ast, 'ast');
    #warn "!!!!\n\n", $ast->kid, "\n\n!!!!\n\n";
    #warn "@Kid::Proc::calls";
    #warn join ', ', keys %Kid::Proc::decls;
    #warn join ', ', values %Kid::Proc::decls;
    $ast;
}

sub emit_proc_call {
    my ($id, $expr_list) = @_;
    my $proc_call = proc_call->new($id, $expr_list);
    push @Kid::Proc::calls, $proc_call;
    $proc_call;
}

sub emit_proc_decl {
    my ($id, $id_list, $block) = @_;
    my $proc_decl = proc_decl->new($id, $id_list, $block);
    $Kid::Proc::decls{$id->value} = $proc_decl;
    #warn "emit_proc_decl: ", $block->kid;
    $proc_decl;
}

sub expand_proc {
    my $ast = shift;
    my %c;
    for my $call (@Kid::Proc::calls) {
        my $proc_name = $call->identifier->value;
        my $expr_list = $call->expression_list;

        die "error: proc $proc_name not defined"
            if !defined $Kid::Proc::decls{$proc_name};
        #warn "LISP: ($proc_name (", $expr_list->kid, "))";

        my $prefix = "_${proc_name}_" . ++$c{$proc_name} . "_";
        my $retval = var->new( identifier->new($prefix . $proc_name) );
        #warn "Retval: ", $retval->kid;

        my $term = $call->parent->parent;
        $term->factor( factor->new($retval) );
        #warn "term: ", $term, ": ", $term->kid;
        die "$term is not a term" if ref $term ne 'term';

        my $expr = $term->parent;
        die "$expr is not an expression" if ref $expr ne 'expression';
        my $stmt = $expr->parent; # parent of expression
        while (ref $stmt ne 'statement') {
            $stmt = $stmt->parent;
            die "No statement found" if !$stmt;
        }
        my $parent = $stmt->parent;

        # $part1 is a statement_list containing argument passing assignments
        my $part1 = emit_args_pass($prefix, $proc_name, $expr_list);
        die if ref $part1 ne 'statement_list';
        my $stmt_prefix = $part1;

        # $part2 is a block coming from proc decl
        my $part2 = emit_proc_body($prefix, $proc_name);
        die if ref $part2 ne 'statement';
        $stmt_prefix = statement_list->new($stmt_prefix, $part2);

        my $block = block->new( statement_list->new($stmt_prefix, $stmt) );
        #warn "expand_proc: ", $block->kid();

        #warn "PARENT!!! $parent";
        $parent->statement( statement->new($block) );
    }
    $ast;
}

sub emit_args_pass {
    my ($prefix, $proc_name, $expr_list) = @_;
    my @exprs = $expr_list->get_all;
    my $proc_decl = $Kid::Proc::decls{$proc_name};
    my @ids = $proc_decl->identifier_list->get_all;
    map {
        undef $_->{__PARANT__};
        $_ = Clone::clone($_);
        $_->value($prefix . $_->value)
    } @ids;
    #warn "@exprs";
    #warn "@ids";
    my @vars = map { var->new($_) } @ids;
    my $stmt_list = nil->new;
    for my $i (0..$#vars) {
        my $assign = assignment->new( $vars[$i], $exprs[$i] );
        my $stmt = statement->new($assign);
        $stmt_list = statement_list->new($stmt_list, $stmt);
    }

    my $block = block->new( $stmt_list );
    #warn "emit_args_pass: ", $block->kid;
    $stmt_list;
}

sub emit_proc_body {
    my ($prefix, $proc_name) = @_;
    my $proc_decl = $Kid::Proc::decls{$proc_name};
    my $block = $proc_decl->block;
    #warn "emit_args_body 1: ", $block->kid;
    $block = rename_vars( $prefix, $block );
    #warn "emit_args_body 2: ", $block->kid;
    statement->new($block);
}

sub rename_vars {
    my ($prefix, $tree) = @_;
    #$tree = Clone::clone($tree);
    $Kid::Proc::rename_vars::p = $prefix;
    $RenameGrammar ||= new Language::AttributeGrammar <<'END_GRAMMAR';

statement_list: $/.ast = { statement_list->new( $<statement_list>.ast, $<statement>.ast ); }
statement:      $/.ast = { statement->new( $<child>.ast ); }

declaration:     $/.ast = { declaration->new( $<child>.ast ); }
proc_decl:       $/.ast = { nil->new; }

identifier_list: $/.ast = { identifier_list->new( $<identifier_list>.ast, $<identifier>.ast ) }

if_statement: $/.ast = { if_statement->new( $<condition>.ast, $<statement>.ast, $<else_statement>.ast ); }

condition:    $/.ast = { condition->new( $<expression>.ast, $<rel_op>.ast, $<rhs_expression>.ast ); }
rel_op:       $/.ast = { rel_op->new( $<__VALUE__> ); }
rhs_expression:  $/.ast = { rhs_expression->new( $<expression>.ast ); }

block:           $/.ast = { block->new( $<statement_list>.ast ); }
else_statement:  $/.ast = { else_statement->new( $<statement>.ast ); }

assignment: $/.ast = { assignment->new( $<var>.ast, $<expression>.ast ); }

var:        $/.ast = { Kid::Proc::emit_var( $<identifier>.ast ); }
identifier: $/.ast = { identifier->new( $<__VALUE__> ); }

expression: $/.ast = { expression->new( $<expression>.ast, $<op>, $<term>.ast ); }

expression_list: $/.ast = { expression_list->new( $<expression_list>.ast, $<expression>.ast ); }
term:       $/.ast = { term->new( $<neg>.ast, $<term>.ast, $<op>, $<factor>.ast ); }
neg:        $/.ast = { neg->new('-'); }
factor:     $/.ast = { factor->new( $<child>.ast ); }
number:     $/.ast = { number->new( $<__VALUE__> ); }
proc_call:  $/.ast = { Kid::Proc::emit_proc_call( $<identifier>.ast, $<expression_list>.ast ); }

nil:        $/.ast = { nil->new; }

END_GRAMMAR
    return $RenameGrammar->apply($tree, 'ast');
}

sub emit_var {
    my $id = shift;
    $id->value( $Kid::Proc::rename_vars::p . $id->value );
    var->new($id);
}

sub rm_proc_decls {
    my $ast = shift;
    for my $proc_decl (values %Kid::Proc::decls) {
        my $decl = $proc_decl->parent;
        die $decl if ref $decl ne 'declaration';
        my $stmt = $decl->parent;
        die $stmt if ref $stmt ne 'statement';
        #warn "12345 ", $stmt->kid, "12345";
        $stmt->parent->statement(nil->new);
    }
    %Kid::Proc::decls = ();
    $ast;
}

1;
__END__
