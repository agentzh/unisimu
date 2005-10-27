use warnings;
use strict;
use HTTP::Server::Simple;

my $server = MyServer->new();
$server->run();

package MyServer;
use warnings;
use strict;
use URI::Escape;

use base qw(HTTP::Server::Simple::CGI);
