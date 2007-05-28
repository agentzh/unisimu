#!/usr/bin/env perl

=begin problem

三个进程(线程)通信采用共享缓冲区机制，缓冲区应至少能存放10组数据，
每组数据是一个不超过31字节的字符串。
数据处理进程（M）的职责是：
将读入的一组数据中的
1.所有大写字母转换为小写字母（但每个句子的第一个字母应该大写）
2.去掉单词与单词之间、句子与句子之间多余的空格
3.如果某个标点既不是句号，也不是逗号，则将其改为句号

=cut

use strict;
use warnings;

#use Smart::Comments;
use threads;
use threads::shared;
use PV;

my $in;

my @buffer: shared = ();
my $K = 10;

my $R_i = 0;
my $M_i = 0;
my $P_i = 0;

semas(
    R_enter => $K,
    M_enter => 0,
    P_enter => 0,
);

async { #-# R
    while (1) {
        P('R_enter');
        my $s = read_data();
        warn "R: Reading [$s]...\n" if defined $s;

        threads->yield;

        $buffer[$R_i] = $s;
        $R_i = ($R_i + 1) % $K;
        V('M_enter');
        last if not defined $s;
    }
    #-#
}

#sleep(3);

async { #-# M
    while (1) {
        P('M_enter');
        my $s = $buffer[$M_i];
        my $new;
        if (defined $s) {
            $new = lc($s);
            $new = ucfirst($new);
            $new =~ s/ {2,}/ /g;
            $new =~ s/[^A-Za-z\s,.]/./g;
            warn "M: Converting [$s] to [$new]...\n";
        }

        threads->yield;

        $buffer[$M_i] = $new;
        $M_i = ($M_i + 1) % $K;
        V('P_enter');
        last if not defined $s;
    }
    #-#
}

#sleep(3);

async { #-# P
    while (1) {
        P('P_enter');
        my $s = $buffer[$P_i];
        ### P (s): $s
        warn "P: Printing [$s]...\n" if defined $s;
        if (defined $s) { print "$s\n"; }
        else { last; }
        $P_i = ($P_i + 1) % $K;
        V('R_enter');
    }
    #-#
}

my $thr;
foreach $thr (threads->list) {
    if ($thr->tid && !threads::equal($thr, threads->self)) {
        $thr->join;
    }
}

sub read_data {
    if (not $in) {
        my $file = 'lcc.data.txt';
        open $in, $file or
            die "Can't open $file for reading: $!\n";
    }
    my $s = <$in>;
    chomp $s if $s;
    my $retval = defined $s && $s ne '' ? $s : undef;
    ### $retval
    $retval
}

