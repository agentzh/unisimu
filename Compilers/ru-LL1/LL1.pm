#: LL1.pm
#: 2006-06-07 2006-06-07

package LL1;

use strict;
use warnings;

sub eof { '/\Z/' }

sub eps { "''" }

sub err { '/\S+/' }

1;
