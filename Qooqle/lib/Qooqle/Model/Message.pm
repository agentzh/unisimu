use strict;
use warnings;

package Qooqle::Model::Message;

use Jifty::DBI::Schema;
use Qooqle::Model::Session;
use Qooqle::Model::QQUser;

use Qooqle::Record schema {
    column sent =>
        type is 'timestamp',
        label is 'Sent time',
        is mandatory;

    column sender => 
        refers_to Qooqle::Model::QQUser by 'qq_number',
        is mandatory;

    column receiver =>
        refers_to Qooqle::Model::QQUser by 'qq_number',
        is mandatory;

    column content =>
        type is 'text',
        label is 'Content',
        render_as 'Textarea',
        is mandatory;

    column msg_session =>
        refers_to Qooqle::Model::Session by 'id',
        is mandatory;

    column session_offset =>
        type is 'integer',
        label is 'Offset in its group',
        validator is sub { $_[0] >= 0; },
        is mandatory;
};

1;
