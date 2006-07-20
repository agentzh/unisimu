use strict;
use warnings;
use Win32;

my $msg;
if (@ARGV) {
    $msg = eval $ARGV[0];
    if ($@) { $msg = $ARGV[0]; }
}
$msg = '' if !defined $msg;
Win32::MsgBox($msg, MB_ICONINFORMATION, Win32::LoginName);
0;
