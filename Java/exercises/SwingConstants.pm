package SwingConstants;

use strict;
use warnings;

#use Java::Swing::ActionListener;

use Inline Java      => 'STUDY',
           AUTOSTUDY => 1,
           STUDY     => ['javax.swing.SwingConstants'];

sub new {
    shift;
    SwingConstants::javax::swing::SwingConstants->new();
}

1;
