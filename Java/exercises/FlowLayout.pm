package FlowLayout;

use strict;
use warnings;

use Carp;

#use Java::Swing::ActionListener;

use Inline Java      => 'STUDY',
           AUTOSTUDY => 1,
           STUDY     => ['java.awt.FlowLayout' ];

sub new {
    shift;
    FlowLayout::java::awt::FlowLayout->new(@_);
}

1;
