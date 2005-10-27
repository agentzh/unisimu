#: Dynamic/Partition.pm
#: Simulation for dynamic partition
#: v0.01
#: Copyright (c) 2005 Agent Zhang
#: 2005-10-23 2005-10-23

package Dynamic::Partition;

use strict;
use warnings;
use List::Util qw(first);

our @ReqTable;
our @FreeTable;

our $VERSION = '0.01';

sub recycle {
    shift;
    my ($pid, $addr, $size) = @_;
    my $bottom = $addr + $size;
    for (my $i = 0; $i < @FreeTable; $i++) {
        my $block = $FreeTable[$i];
        my ($blk_sz, $blk_addr) =
            ($block->{size}, $block->{addr});
        if ($blk_addr + $blk_sz == $addr) {
            $addr = $blk_addr;
            $size += $blk_sz;
            splice @FreeTable, $i, 1;
            redo;
        }
        if ($blk_addr == $bottom) {
            $block->{addr} = $addr;
            $block->{size} += $size;
            return;
        }
        if ($block->{addr} > $bottom) {
            splice @FreeTable, $i, 0,
                { addr => $addr, size => $size };
            return;
        }
    }
    push @FreeTable, { addr => $addr, size => $size };
}

sub first_fit {
    shift;
    my $assign = shift;
    my $i = 0;
    while (@ReqTable) {
        my $req = $ReqTable[$i];
        my $req_sz = $req->{size};
        my $j = -1;
        my $block = first { $j++; $_->{size} >= $req_sz; } @FreeTable;
        if (!defined $block) {
            last if $i >= @ReqTable - 1;
            $i++;
            next;
        }
        &$assign($req->{pid}, $block->{addr}, $req_sz)
            if $assign and ref $assign;
        if ($block->{size} == $req_sz) {
            splice @FreeTable, $j, 1;
        } else {
            $block->{addr} += $req_sz;
            $block->{size} -= $req_sz;
        }
        splice @ReqTable, $i, 1;
        last if $i >= @ReqTable - 1;
        warn "Deleting $i...\n";
        #warn "\@ReqTable = ", join(':',@ReqTable), "\n";
    }
}

sub best_fit {
    shift;
    my $assign = shift;
    my $i = 0;
    while (@ReqTable) {
        my $req = $ReqTable[$i];
        my ($addr, $req_sz) = ($req->{addr}, $req->{size});
        my ($j_best, $sz_best);
        my $j = 0;
        for my $block (@FreeTable) {
            my $blk_sz = $block->{size};
            if ($blk_sz < $req_sz) {
                last if @ReqTable == 1;
                $i++;
                next;
            } elsif ($blk_sz == $req_sz) {
                $j_best  = $j;
                $sz_best = $blk_sz;
                last;
            } else {
                if (!defined $j_best or $blk_sz < $sz_best) {
                    $j_best  = $j;
                    $sz_best = $blk_sz;
                }
            }
            $j++;
        }
        if (defined $j_best) {
            my $best = $FreeTable[$j_best];
            &$assign($req->{pid}, $best->{addr}, $req_sz)
                if $assign and ref $assign;
            if ($sz_best == $req_sz) {
                splice @FreeTable, $j, 1;
            } else {
                $best->{addr} += $req_sz;
                $best->{size} -= $req_sz;
            }
        }
        splice @ReqTable, $i, 1;
    }
}

sub worst_fit {
    shift;
    my $assign = shift;
    my $i = 0;
    while (@ReqTable) {
        my $req = $ReqTable[$i];
        my ($addr, $req_sz) = ($req->{addr}, $req->{size});
        my ($j_worst, $sz_worst);
        my $j = 0;
        for my $block (@FreeTable) {
            my $blk_sz = $block->{size};
            if ($blk_sz < $req_sz) {
                last if @ReqTable == 1;
                $i++;
                next;
            } elsif ($blk_sz == $req_sz) {
                $j_worst  = $j;
                $sz_worst = $blk_sz;
                last;
            } else {
                if (!defined $j_worst or $blk_sz > $sz_worst) {
                    $j_worst  = $j;
                    $sz_worst = $blk_sz;
                }
            }
            $j++;
        }
        if (defined $j_worst) {
            my $worst = $FreeTable[$j_worst];
            &$assign($req->{pid}, $worst->{addr}, $req_sz)
                if $assign and ref $assign;
            if ($sz_worst == $req_sz) {
                splice @FreeTable, $j, 1;
            } else {
                $worst->{addr} += $req_sz;
                $worst->{size} -= $req_sz;
            }
        }
        splice @ReqTable, $i, 1;
    }
}

1;
