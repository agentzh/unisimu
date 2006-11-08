use strict;
use warnings;
use IPC::Run3;

my ($stdout, $stderr);
run3(['plcon', @ARGV], \undef, \$stdout, \$stderr);
print $stdout;
$stderr =~ s/Warning: \([^\)]+\):\s+Clauses of .*? are not together in the source-file\n//gs;
$stderr =~ s/Warning: \([^\)]+\):\s+Singleton variables: \[\w+\]\n//gs;
warn $stderr if $stderr;
