package UML::Class::Simple;

use 5.006001;
use strict;
use warnings;
no warnings 'redefine';

use Class::Inspector;
use IPC::Run3;
use Template;
use Carp qw(carp);
use File::Spec;
use List::MoreUtils 'any';

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(classes_from_runtime classes_from_files);

our $VERSION = '0.04';

my $tt = Template->new;
my $dot_template;

sub classes_from_runtime ($$) {
    my ($module, $pattern) = @_;
    $pattern = '' if !defined $pattern;
    if ($module) {
        eval "use $module;";
        if ($@) { carp $@; return (); }
    }
    grep { /$pattern/ } map { s/^::|::$//g; $_ } _runtime_packages();
}

sub _runtime_packages {
    no strict 'refs';
    my $pkg_name = shift || '::';
    my $cache = shift || {};
    return if $cache->{$pkg_name};
    $cache->{$pkg_name} = 1;
    for my $entry (keys %$pkg_name) {
        next if $entry !~ /\:\:$/ or $entry eq 'main::';
        my $subpkg_name = $pkg_name.$entry;
        #warn $subpkg_name;
        _runtime_packages($subpkg_name, $cache);
        $cache->{$subpkg_name} = 1;
    }
    keys %$cache;
}

sub classes_from_files ($@) {
    require PPI;
    my ($list, $pattern) = @_;
    my @classes;
    my $cache = {};
    for my $file (@$list) {
        _gen_paths($file, $cache);
        my $doc = PPI::Document->new( $file );
        if (!$doc) {
            carp "warning: Can't parse $file: ", PPI::Document->errstr;
            next;
        }
        my $res = $doc->find('PPI::Statement::Package');
        next if !$res;
        push @classes, map { $_->namespace } @$res;
        _load_file($file);
    }
    #@classes = sort @classes;
    wantarray ? @classes : \@classes;
}

sub _gen_paths {
    my ($file, $cache) = @_;
    $file =~ s{\\+}{/}g;
    my $dir;
    while ($file =~ m{(?x) \G .+? /+ }gc) {
        $dir .= $&;
        next if $cache->{$dir};
        $cache->{$dir} = 1;
        #warn "pushing ~~~ $dir\n";
        unshift @INC, $dir;
    }
}

sub new ($$) {
    my $class = ref $_[0] ? ref shift : shift;
    my $rclasses = shift || [];
    my $self = bless {
        class_names => $rclasses,
        node_color  => '#f1e1f4',
    }, $class;
    $self->_build_dom;
    $self;
}

sub size ($$$) {
    my $self = shift;
    if (@_) {
        my ($width, $height) = @_;
        if (!$width || !$height || ($width . $height) !~ /^[\.\d]+$/) {
            carp "invalid width and height";
            return undef;
        } else {
            $self->{width}  = $width;
            $self->{height} = $height;
            return 1;
        }
    } else {
        return ($self->{width}, $self->{height});
    }
}

sub node_color {
    my $self = shift;
    if (@_) {
        $self->{node_color} = shift;
    } else {
        $self->{node_color};
    }
}

sub public_only ($$) {
    my $self = shift;
    if (@_) {
        $self->{public_only} = shift;
        $self->_build_dom(1);
    } else {
        $self->{public_only};
    }
}

sub as_png ($@) {
    my $self = shift;
    $self->_as_image('png', @_);
}

sub as_gif ($@) {
    my $self = shift;
    $self->_as_image('gif', @_);
}

sub _as_image {
    my ($self, $type, $fname) = @_;
    my $dot = $self->as_dot;
    #if ($fname eq 'fast00.png') {
        #warn "==== $fname\n";
        #warn $dot;
        #use YAML::Syck;
        #$self->_build_dom(1);
        #warn Dump($self->as_dom);
    #}
    my @cmd = ('dot', '-T', $type);
    if ($fname) {
        push @cmd, '-o', $fname;
    }
    my ($img_data, $stderr);
    my $success = run3 \@cmd, \$dot, \$img_data, \$stderr;
    if ($stderr) {
        carp $stderr;
    }
    if (!$fname) {
        return $img_data;
    }
}

sub as_dom ($) {
    my $self = shift;
    $self->_build_dom;
    { classes => $self->{classes} };
}

sub set_dom ($$) {
    my $self = shift;
    $self->{classes} = shift->{classes};
    1;
}

sub _build_dom {
    my ($self, $force) = @_;
    # avoid unnecessary evaluation:
    return if $self->{classes} && !$force || !$self->{class_names};
    #warn "HERE";
    my @pkg = @{ $self->{class_names} };
    my @classes;
    $self->{classes} = \@classes;
    my $public_only = $self->{public_only};
    my %visited; # used to eliminate potential repetitions
    for my $pkg (@pkg) {
        #warn $pkg;
        $pkg =~ s/::::/::/g;
        if ($visited{$pkg}) { next; }
        $visited{$pkg} = 1;

        if (!Class::Inspector->loaded($pkg)) {
            #my $pmfile = Class::Inspector->filename($pkg);
            #warn $pmfile;
            #if ($pmfile) {
            #    if (! _load_file($pmfile)) {
            #        next;
            #    }
            #} else { next }
            next;
        }
        push @classes, {
            name => $pkg, methods => [],
            properties => [], subclasses => [],
        };
        my $func = Class::Inspector->functions($pkg);
        if ($func and @$func) {
            if ($public_only) {
                @$func = grep { /^[^_]/ } @$func;
            }
            $classes[-1]->{methods} = $func;
        }
        my $subclasses = Class::Inspector->subclasses($pkg);
        if ($subclasses) {
            no strict 'refs';
            my @child = grep {
                #warn "!!!! ", join ' ', @{"${_}::ISA"};
                any { $_ eq $pkg } @{"${_}::ISA"};
            } @$subclasses;
            if (@child) {
                $classes[-1]->{subclasses} = \@child;
            }
        }
    }
    #warn "@classes";
}

sub _load_file {
    my $file = shift;
    my $path = File::Spec->rel2abs($file);
    #warn "!!! >>>> $path\n";
    if ( any { 
                #warn "<<<<< ", File::Spec->rel2abs($_), "\n";
                $path eq File::Spec->rel2abs($_);
             } values %INC ) {
        #carp "!!! Caught duplicate module files: $file ($path)";
        return 1;
    }
    #my @a = values %INC;
    #warn "\n@a\n";
    #warn "!!! Loading $path...\n";
    eval {
        require $path;
    };
    carp $@ if $@;
    !$@;
}

sub as_dot ($@) {
    my ($self, $fname) = @_;
    $self->_build_dom;
    if ($fname) {
        $tt->process(\$dot_template, $self, $fname)
            || carp $tt->error();
    } else {
        my $dot;
        $tt->process(\$dot_template, $self, \$dot)
            || carp $tt->error();
        $dot;
    }
}

sub set_dot ($$) {
    my $self = shift;
    $self->{dot} = shift;
}

$dot_template = <<'_EOC_';
digraph uml_class_diagram {
  [%- IF width && height %]
    size="[% width %],[% height %]";
  [%- END %]
    node [shape=record, style="filled"];
    edge [color=red, dir=none];

[%- name2id = {} %]
[%- id = 1 %]
[%- FOREACH class = classes %]
    [%- name = class.name %]
    [%- name2id.$name = id %]
    class_[% id %] [shape=plaintext, style="", label=<
<table BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
  <tr><td port="title" bgcolor="[% node_color %]">[% name %]</td></tr>
  <tr>
    <td>
    <table border="0" cellborder="0" cellspacing="0" cellpadding="1">
      <tr>
    <td><font color="red">
    [%- FOREACH property = class.properties %]
      [%- property.match("^_") ? "-" : "+" %]<br align="left"/>

    [%- END %]</font></td>
    <td port="properties" bgcolor="white" align="left">
    [%- FOREACH property = class.properties %]
      [%- property %]<br align="left"/>

    [%- END %]</td>
      </tr>
    </table>
    </td>
  </tr>
  <tr>
    <td port="methods" >
    <table border="0" cellborder="0" cellspacing="0" cellpadding="0">
      <tr>
    <td><font color="red">
    [%- FOREACH method = class.methods %]
      [%- method.match("^_") ? "-" : "+" %]<br align="left"/>

    [%- END %]</font></td>
    <td bgcolor="white" align="left">
    [%- FOREACH method = class.methods %]
      [%- method %]<br align="left"/>

    [%- END %]</td>
      </tr>
    </table>
    </td>
  </tr>
</table>>];
  [%- id = id + 1 %]
[% END %]
[%- class_id = id %]

[%- first = 1 %]
[%- id = 0 %]
[%- FOREACH class = classes %]
  [%- id = id + 1 %]
  [%- super = class.name %]
  [%- NEXT IF !class.subclasses.size -%]

  [%- IF first -%]
     node [shape="triangle", fillcolor=yellow, height=0.3, width=0.3];
     [%- first = 0 %]
  [%- END -%]

     angle_[% id %] [label=""];


  [%- super_id = name2id.$super %]
     class_[% super_id %]:methods -> angle_[% id %]
  [%- FOREACH child = class.subclasses %]
    [%- child_id = name2id.$child %]
    [%- IF !child_id %]
     class_[% class_id %] [shape=record, label="[% child %]" fillcolor="#f1e1f4", style="filled"];
     angle_[% id %] -> class_[% class_id %]
        [%- class_id = class_id + 1 %]
      [%- ELSE %]
     angle_[% id %] -> class_[% child_id %]:title
    [%- END %]
  [%- END %]
[%- END %]

}
_EOC_

1;
__END__

=head1 NAME

UML::Class::Simple - Render simple UML class diagrams, by loading the code

=head1 VERSION

This document describes C<UML::Class::Simple> 0.04 released by Nov 1, 2006.

=head1 SYNOPSIS

    use UML::Class::Simple;

    # produce a class diagram for Alias's PPI
    # which has already installed to your perl:

    @classes = classes_from_runtime("PPI", qr/^PPI::/);
    $painter = UML::Class::Simple->new(\@classes);
    $painter->as_png('ppi.png');

    # produce a class diagram for your CPAN module on the disk

    @classes = classes_from_files(['lib/Foo.pm', 'lib/Foo/Bar.pm']);
    $painter = UML::Class::Simple->new(\@classes);
    
    # we can explicitly specify the image size
    $painter->size(5, 3.6); # in inches

    # ...and change the default title background color:
    $painter->node_color('#ffffff'); # defaults to '#f1e1f4'
    
    # only show public methods and properties
    $painter->public_only(1);

    $painter->as_png('my_module.png');

=head1 DESCRIPTION

C<UML::Class::Simple> is a Perl CPAN module that generates UML class
diagrams (PNG format, GIF format, or dot source) automatically
from Perl 5 source or Perl 5 runtime.

Perl developers can use this module to obtain pretty class diagrams
for arbitrary existing Perl class libraries (including modern perl OO
modules based on Moose.pm), by only a single command. Companies can
also use the resulting pictures to visualize the project hierarchy and
embed them into their documentation.

The users no longer need to drag a mouse on the screen so as to draw
figures themselves or provide any specs other than the source code of
their own libraries that they want to depict. This module does all the
jobs for them! :)

You know, I was really impressed by the outputs of L<UML::Sequence>, so I 
decided to find something to (automatically) get pretty class diagrams
too. The images from L<Autodia>'s Graphviz backend didn't quite fit my needs
when I was making some slides for my presentations.

I think most of the time you just want to use the command-line utility L<umlclass.pl>
offered by this module (just like me). See the documentation of L<umlclass.pl> for
details.

=head1 SAMPLE OUTPUTS

=over

=item PPI

L<http://perlcabal.org/agent/images/ppi_small.png>

=begin html

<img src="http://perlcabal.org/agent/images/ppi_small.png">

=end html

(See also F<samples/ppi_small.png> in the distribution.)

=item Moose

L<http://perlcabal.org/agent/images/moose_small.png>

=begin html

<img src="http://perlcabal.org/agent/images/moose_small.png">

=end html

(See also F<samples/moose_small.png> in the distribution.)

=item FAST

L<http://perlcabal.org/agent/images/fast.png>

=begin html

<img src="http://perlcabal.org/agent/images/fast.png">

=end html

(See also F<samples/fast.png> in the distribution.)

=back

=head1 SUBROUTINES

=over

=item classes_from_runtime($module_to_load, $regex)

Returns a list of class (or package) names by inspecting the perl runtime environment.
C<$module_to_load> is the I<main> module name to load while C<$regex> is
a perl regex used to filter out interesting package names.

The second argument can be omitted.

=item classes_from_files(\@pmfiles, $regex)

Returns a list of class (or package) names by scanning through the perl source files
given in the first argument. C<$regex> is used to filter out interesting package names.

The second argument can be omitted.

=back

These subroutines are imported by default.

=head1 METHODS

=over

=item C<< $obj->new( [@class_names] ) >>

Create a new C<UML::Class::Simple> instance with the specified class name list.
This list can either be constructed manually or by the utility functions
C<classes_from_runtime> and C<classes_from_files>.

=item C<< $obj->as_png($filename?) >>

Generate PNG image file when C<$filename> is given. It returns
binary data when C<$filename> is not given.

=item C<< $obj->as_gif($filename?) >>

Similar to C<as_png>, bug generate a GIF-format image.

=item C<< $obj->as_dom() >>

Return the internal DOM tree used to generate dot and png. The tree's structure
looks like this:

  {
    'classes' => [
                   {
                     'subclasses' => [],
                     'methods' => [],
                     'name' => 'PPI::Structure::List',
                     'properties' => []
                   },
                   {
                     'subclasses' => [
                                       'PPI::Structure::Block',
                                       'PPI::Structure::Condition',
                                       'PPI::Structure::Constructor',
                                       'PPI::Structure::ForLoop',
                                       'PPI::Structure::Unknown'
                                     ],
                     'methods' => [
                                    '_INSTANCE',
                                    '_set_finish',
                                    'braces',
                                    'content',
                                    'new',
                                    'refaddr',
                                    'start',
                                    'tokens'
                                  ],
                     'name' => 'PPI::Structure',
                     'properties' => []
                   },
                   ...
                ]
  }

You can adjust the data structure and feed it back to C<$obj> via
the C<set_dom> method.

=item C<< $obj->set_dom($dom) >>

Set the internal DOM structure to C<$obj>. This will be used to
generate the dot source and thus the PNG/GIF images.

=item C<< $obj->as_dot() >>

Return the Graphviz dot source code generated by C<$obj>.

=item C<< $obj->set_dot($dot) >>

Set the dot source code used by C<$obj>.

=back

=head1 PROPERTIES

=over

=item C<< $obj->size($width, $height) >>

=item C<< ($width, $height) = $obj->size >>

Set/get the size of the output images, in inches.

=item C<< $obj->public_only($bool) >>

=item C<< $bool = $obj->public_only >>

When the C<public_only> property is set to true, only public methods or properties
are shown. It defaults to false.

=item C<< $obj->node_color($color) >>

=item C<< $color = $obj->node_color >>

Set/get the background color for the class nodes. It defaults to C<'#f1e1f4'>.

=back

=head1 INSTALLATION

Please download and intall a recent Graphviz release from its home:

L<http://www.graphviz.org/>

C<UML::Class::Simple> requires the HTML label feature which is only
available on versions of Graphviz that are newer than mid-November 2003.
In particular, it is not part of release 1.10.

Add Graphviz's F<bin/> path to your PATH environment. This module needs its
F<dot> utility.

Grab this module from the CPAN mirror near you and run the following commands:

    perl Makefile.PL
    make
    make test
    make install

For windows users, use C<nmake> instead of C<make>.

Note that it's recommended to use the C<cpan> utility to install CPAN modules.

=head1 LIMITATIONS

=over

=item *

It's pretty hard to distinguish perl methods from properties (actually they're both
implemented by subs in perl). If you have any good thoughts on this issue,
please drop me a line.

=item *

Only the inheritance relationships are shown in the images. I believe other subtle 
relations may mess up the Graphviz layouter. Hence the "::Simple" suffix in
this module name.

=item *

Unlike L<Autodia>, at this moment only Graphviz backend is provided.

=item *

There's no way to recognize I<real> perl classes automatically. After all, Perl 5's 
classes are implemented by packages. I think Perl 6 will make my life much easier.

=item *

To prevent potential naming confusion. I'm using Perl's C<::> namespace separator
in the class diagrams instead of dot (C<.>) chosen by the UML standard. One can argue
that following UML standards are more important since people in the same team may
use different programming languages. But I think it's not the case for most people
(including me). ;-)

=back

=head1 TODO

=over

=item *

Suppress the "Subroutine XXX redefined at ..." warnings while loading user's
.pm files. Not sure how to do that. If you have a solution, please contact the
author ASAP.

=item *

Add more unit tests.

=item *

Add support for more image formats, such as as_ps, as_jpg, and etc.

=item *

Plot class relationships other than inheritance on the user's request.

=item *

Provide backends other than Graphviz.

=back

Please send me your wish list by emails or preferably via the CPAN RT site. I'll add
them here if I'm also interested in your crazy ideas. ;-)

=head1 BUGS

There must be some serious bugs lurking somewhere. so if you found one, please report
it to L<http://rt.cpan.org> or contact the author directly.

=head1 ACKNOWLEDGEMENT

I must thank Adam Kennedy (Alias) for writing the excellent L<PPI> and
L<Class::Inspector> modules. L<umlclass.pl> uses the former to extract package names 
from user's .pm files or the latter to retrieve the function list of a 
specific package.

I'm also grateful to Christopher Malon since he (unintentionally) motivated me to
turn the original hack into this CPAN module. ;-)

=head1 AUTHOR

Agent Zhang E<lt>agentzh@gmail.comE<gt>

=head1 COPYRIGHT

Copyright 2006 by Agent Zhang. All rights reserved.

This library is free software; you can redistribute it and/or modify it under
the same terms as perl itself.

=head1 SEE ALSO

L<umlclass.pl>, L<Autodia>, L<UML::Sequence>, L<PPI>, L<Class::Inspector>.
