use strict;
use warnings;

package Qooqle::Model::Session;
use Jifty::DBI::Schema;

use Qooqle::Record schema {
    column begin_time =>
        type is 'timestamp',
        label is 'Begin time',
        is mandatory;

    column end_time =>
        type is 'timestamp',
        label is 'End time',
        is mandatory;

    column message_count =>
        type is 'integer',
        label is 'Message count',
        is mandatory;
};

# Your model-specific methods go here.

1;
