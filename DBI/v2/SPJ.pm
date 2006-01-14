package SPJ::DBI;

use strict;
use warnings;
use base 'Class::DBI';
use Class::DBI::AbstractSearch;

__PACKAGE__->connection($ENV{DSN});

###################################################

package SPJ::Project;

use strict;
use warnings;
use base 'SPJ::DBI';

__PACKAGE__->table('J', 'jno');
__PACKAGE__->columns(All => qw/jno jname city/);
__PACKAGE__->has_many(vendors => [ 'SPJ::Supply' => 'sno' ]);
__PACKAGE__->has_many(parts => ['SPJ::Supply' => 'pno' ]);

####################################################

package SPJ::Vendor;

use strict;
use warnings;
use base 'SPJ::DBI';

__PACKAGE__->table('S', 'sno');
__PACKAGE__->columns(Primary => 'sno');
__PACKAGE__->columns(Others => qw/sname status city/);
__PACKAGE__->has_many(parts => ['SPJ::Supply' => 'pno' ]);
__PACKAGE__->has_many(projects => ['SPJ::Supply' => 'jno' ]);

###################################################

package SPJ::Part;

use strict;
use warnings;
use base 'SPJ::DBI';

__PACKAGE__->table('P', 'pno');
__PACKAGE__->columns(All => qw/pno pname color weight/);
__PACKAGE__->has_many(vendors => [ 'SPJ::Supply' => 'sno' ]);
__PACKAGE__->has_many(projects => ['SPJ::Supply' => 'jno' ]);

###################################################

package SPJ::Supply;
use base 'SPJ::DBI';
__PACKAGE__->table('SPJ');
__PACKAGE__->columns(Primary => qw/sno pno jno/);
__PACKAGE__->columns(Others  => qw/qty/);
__PACKAGE__->has_a(sno => 'SPJ::Vendor');
__PACKAGE__->has_a(pno => 'SPJ::Part');
__PACKAGE__->has_a(jno => 'SPJ::Project');

1;
