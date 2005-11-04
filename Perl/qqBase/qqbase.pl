#: qqbase.pl
#: Set up the QQ log database
#: 2005-11-02 2005-11-03

use strict;
use warnings;
use DBI;
use DBI::Const::GetInfoType;
use List::Util qw(first);
use File::Spec;
use POSIX 'mktime';
use Getopt::Std;

my %opts;
getopts('s:', \%opts);

my $BODY_SIZE = +$opts{s} || 255;

local $| = 1;

my %schema_sql = (
    'msgs' => <<_EOC_,
create table msgs
    (msg_time integer not null,
     msg_from varchar(20) not null,
     msg_to   varchar(20) not null,
     msg_body varchar($BODY_SIZE) not null,
     session_id integer not null,
     offset integer not null,
     primary key (msg_time, msg_from, msg_to),
     foreign key (msg_from) references users(user_id),
     foreign key (msg_to)   references users(user_id))
_EOC_

    'users' => <<'_EOC_',
create table users
    (user_id varchar(20) primary key,
     user_name varchar(32),
     user_sex char(1),
     user_nickname varchar(32))
_EOC_
);

my $dsn = $ENV{DSN};
die "No env DSN set.\n" unless $dsn;

my $dbh = DBI->connect($dsn, { PrintError => 1, RaiseError => 0 });

my @tables;
my $sth = $dbh->table_info();
if (ref $sth) {
    while (my $rowref = $sth->fetchrow_arrayref) {
        push @tables, $rowref->[2];
    }
} else {
    @tables = $dbh->tables();
}
my $sep = $dbh->get_info( $GetInfoType{SQL_IDENTIFIER_QUOTE_CHAR} );
map { s/$sep//g } @tables;
#die "@tables\n";

foreach my $table (qw(users msgs)) {
    if (not first { $_ eq $table } @tables) {
        print "info: Creating table $table...\n";
        create_table($table);
    } else {
        print "info: Table $table found.\n";
    }
}

my $msg_dup_sth = $dbh->prepare(
    "select msg_time from msgs where msg_time=? and msg_from=? and msg_to=?"
);

my $user_dup_sth = $dbh->prepare(
    "select user_id, user_name from users where user_id=?"
);

my @files = map glob, @ARGV;
@files = grep { -f $_ } @files;
@files = sort { $a cmp $b } @files;
#$" = "\n";
#die "@files\n";
for my $file (@files) {
    process_log($file);
}
$msg_dup_sth->finish;
$user_dup_sth->finish;
$dbh->disconnect();

sub create_table {
    my $table = shift;
    if (not $dbh->do($schema_sql{$table})) {
        warn "Create table $table failed\n";
    }
}

sub process_log {
    my $logfile = shift;
    my $s = $logfile;
    if ($logfile =~ m/.{29}$/) {
        $s = "...$&";
    } else {
        $s .= ' ' x (32 - length($s));
    }
    print "info: $s\n";
    open my $in, $logfile or
        die "Can't open $logfile for reading: $!\n";
    #sleep(1);
    my @dirs = File::Spec->splitdir($logfile);
    my $real_name = $dirs[-2] if @dirs >= 2;
    $real_name = ($real_name =~ /^[^\w]+$/) ? $real_name : undef;
    warn "Real Name: $real_name\n" if $real_name;
    my ($msg_from, $msg_to, $msg_time, $msg_body);
    my ($host, $host_name, $guest, $guest_name);
    my ($session_id, $offset);
    my $state = 'S_INIT';
    while (<$in>) {
        s/\r//g;
        if ($state eq 'S_INIT') {
            if (/^用户:(\d+)\((.+)\)/) {
                ($host, $host_name) = ($1, $2);
                check_user($host, $host_name, $real_name);
            }
            if (/^消息对象:(\d+)\((.+)\)/) {
                ($guest, $guest_name) = ($1, $2);
                check_user($guest, $guest_name, $real_name);
                #warn "$msg_from => $msg_to\n";
                $state = 'S_START';
            }
        }
        elsif ($state eq 'S_START' and
                /^(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2}):(\d{2}) (.+)/) {
            my ($year, $mon, $mday, $hour, $min, $sec) =
                ($1-1900, $2-1, $3, $4, $5, $6);
            my $name = $7;

            if ($msg_time and $msg_from and $msg_to and $msg_body) {
                insert_msg($msg_time, $msg_from, $msg_to, $msg_body, $session_id, $offset++);
                $msg_time = mktime($sec, $min, $hour, $mday, $mon, $year);
            } else { # for the first time:
                $msg_time = mktime($sec, $min, $hour, $mday, $mon, $year);
                $session_id = $msg_time;
                $offset = 0;
            }

            #my $date = localtime($msg_time);
            #chomp;
            #die "$_ => $date\n";
            #($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) =
            #                                    localtime(msg_time);
            $msg_from = $name eq $host_name ? $host : $guest;
            $msg_to   = $name eq $host_name ? $guest : $host;
            die "Internal assertion failed: $msg_from sent msg to itself"
                if $msg_from eq $msg_to;
            $msg_body = '';
        }
        elsif ($state eq 'S_START') {
            $msg_body .= $_;
        }
    }
    if ($msg_time and $msg_from and $msg_to and $msg_body) {
        insert_msg($msg_time, $msg_from, $msg_to, $msg_body, $session_id, $offset);
    }
    close $in;
}

sub check_user {
    my ($id, $nickname, $real_name) = @_;
    my $sth = $user_dup_sth;
    my ($user_id, $user_name);
    $sth->execute($id);
    $sth->bind_columns(\$user_id, \$user_name);
    if ($sth->fetch() and $user_id) {
        #warn "User id $id found!";
        if (!$user_name and $real_name) {
            my $sth = $dbh->prepare(
                'update users set user_name=? where user_id=?'
            );
            $sth->execute($real_name, $user_id);
        }
    } else {
        $sth = $dbh->prepare(
            'insert into users values (?,?,?,?)'
        );
        $sth->execute($id, $real_name, undef, $nickname);
    }
}

sub insert_msg {
    my ($msg_time, $msg_from, $msg_to, $msg_body, $session_id, $offset) = @_;
    #warn "\n+++++++++++++++++\n";
    #warn "@_";
    #warn "-----------------\n";
    my ($user_id, $user_name);
    my $sth = $msg_dup_sth;
    $sth->execute($msg_time, $msg_from, $msg_to);
    if ($sth->fetch) { return undef; }

    $sth = $dbh->prepare(
        'insert into msgs values (?,?,?,?,?,?)'
    );
    $msg_body =~ s/^[\s\n]*\n|[\s\n]+$//gs;
    my $raw = $msg_body;
    $msg_body = substr($raw, 0, $BODY_SIZE);
    if ($msg_body ne $raw) {
        warn "\nMessage trimmed to $BODY_SIZE chars long:\n".
             "$msg_body\n".
             "-----------------------------------------\n";
    }
    #$dbh->{PrintError} = 0;
    if (not $sth->execute($msg_time, $msg_from, $msg_to, $msg_body, $session_id, $offset)) {
        warn "$msg_time, $msg_from, $msg_to, $msg_body, $session_id, $offset";
    }
    #$dbh->{PrintError} = 1;
}
