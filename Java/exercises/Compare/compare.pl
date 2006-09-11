use v6-alpha;

@*ARGS.elems == 3 or
    die "Usage: $?FILE <num> <num> <num>\n";
my ($a, $b, $c) = @*ARGS;
say join ' ',
    reverse sort { $^a <=> $^b }, $a, $b, $c;
