use v6-alpha;

@*ARGS err
    die "Usage: $?FILE <num>+\n";
my @list;
for @*ARGS {
    ord_insert @list, $_;
}
say ~@list;

sub ord_insert(@list is rw, $elem) {
    my $i = 0;
    for @list {
        next if $_ < $elem;
        splice @list, $i, $elem;
        return;
        NEXT { $i++ }
    }
    push @list, $elem;
}
