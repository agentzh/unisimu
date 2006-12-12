use strict;
use warnings;

package Qooqle::Model::QQUser;
use Jifty::DBI::Schema;

use Qooqle::Record schema {
    column qq_number =>
        type is 'text',
        label is 'QQ number';

    column realname => 
        type is 'text',
        label is 'Real name';

    column nickname =>
        type is 'text',
        label is 'Nickname';

    column gender =>
        type is 'char',
        label is 'Gender',
        valid_values are qw/M F/;
};

1;
