use strict;
use warnings;

use Test::More tests => 25;
use Test::MockObject;
use Test::Deep;

my $pkg;
BEGIN {
    $pkg = 'Dynamic::Partition';
    use_ok($pkg);
};
my $req_tb  = "${pkg}::ReqTable";
my $free_tb = "${pkg}::FreeTable";

no strict 'refs';

@$req_tb = (
    { pid => 1, size => 20 },
    { pid => 2, size => 5  },
    { pid => 3, size => 2  },
    { pid => 4, size => 10 },
    { pid => 5, size => 30 },
    { pid => 6, size => 40 },
);

@$free_tb = (
    { addr => 0, size => 70 },
);

my $mock = Test::MockObject->new;
$mock->set_true( 'assign' );
$pkg->first_fit( sub{ $mock->assign(@_); } );
$mock->called_ok('assign');

my @args = $mock->call_args(1);
cmp_deeply (\@args, [ignore(), 1, 0, 20]);

@args = $mock->call_args(2);
cmp_deeply (\@args, [ignore(), 2, 20, 5]);

@args = $mock->call_args(3);
cmp_deeply (\@args, [ignore(), 3, 25, 2]);

@args = $mock->call_args(4);
cmp_deeply (\@args, [ignore(), 4, 27, 10]);

@args = $mock->call_args(5);
cmp_deeply (\@args, [ignore(), 5, 37, 30]);

ok !$mock->call_pos(6);

cmp_deeply(\@$free_tb, [{ addr=>67, size=>3 }]);

$mock->clear;
$pkg->recycle(3, 25, 2);

cmp_deeply(\@$free_tb, [
    { addr=>25, size=>2 },
    { addr=>67, size=>3 }
]);

$pkg->recycle(5, 37, 30);

cmp_deeply(\@$free_tb, [
    { addr=>25, size=>2 },
    { addr=>37, size=>33 }
]);

$pkg->recycle(2, 20, 5);

cmp_deeply(\@$free_tb, [
    { addr=>20, size=>7 },
    { addr=>37, size=>33 }
]);

$pkg->recycle(4, 27, 10);

cmp_deeply(\@$free_tb, [
    { addr=>20, size=>50 },
]);

### Test best_fit:

$mock->clear;

@$req_tb = (
    { pid => 1, size => 20 },
    { pid => 2, size => 5  },
    { pid => 3, size => 10 },
    { pid => 4, size => 30 },
    { pid => 5, size => 40 },
);

@$free_tb = (
    { addr => 0, size => 70 },
);

$pkg->best_fit( sub{ $mock->assign(@_); } );
$mock->called_ok('assign');

@args = $mock->call_args(1);
cmp_deeply (\@args, [ignore(), 1, 0, 20]);

@args = $mock->call_args(2);
cmp_deeply (\@args, [ignore(), 2, 20, 5]);

@args = $mock->call_args(3);
cmp_deeply (\@args, [ignore(), 3, 25, 10]);

@args = $mock->call_args(4);
cmp_deeply (\@args, [ignore(), 4, 35, 30]);

ok !$mock->call_pos(5);

### Test worst_fit:

$mock->clear;

@$req_tb = (
    { pid => 1, size => 20 },
    { pid => 2, size => 5  },
    { pid => 3, size => 10 },
    { pid => 4, size => 30 },
    { pid => 5, size => 40 },
);

@$free_tb = (
    { addr => 0, size => 70 },
);

$pkg->worst_fit( sub{ $mock->assign(@_); } );
$mock->called_ok('assign');

@args = $mock->call_args(1);
cmp_deeply (\@args, [ignore(), 1, 0, 20]);

@args = $mock->call_args(2);
cmp_deeply (\@args, [ignore(), 2, 20, 5]);

@args = $mock->call_args(3);
cmp_deeply (\@args, [ignore(), 3, 25, 10]);

@args = $mock->call_args(4);
cmp_deeply (\@args, [ignore(), 4, 35, 30]);

ok !$mock->call_pos(5);
