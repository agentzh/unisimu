@rem = '--*-Perl-*--
@echo off
if "%OS%" == "Windows_NT" goto WinNT
D:\perl\bin\perl -x -S "%0" %1 %2 %3 %4 %5 %6 %7 %8 %9
goto endofperl
:WinNT
D:\perl\bin\perl -x -S %0 %*
if NOT "%COMSPEC%" == "%SystemRoot%\system32\cmd.exe" goto endofperl
if %errorlevel% == 9009 echo You do not have Perl in your PATH.
if errorlevel 1 goto script_failed_so_exit_with_non_zero_val 2>nul
goto endofperl
@rem ';
#!perl -w
#line 1 "getcnn.pl"
#: getcnn.pl
#: extract news articles from CNN.com by catalogs
#: v0.07
#: Agent2002. All rights reserved.
#: 2004-07-09 2005-03-12

use strict;
use warnings;

use threads;
use threads::shared;
use Thread::Semaphore;
use Thread::Queue;

use LWP::UserAgent;
use HTML::TreeBuilder;
use URI::URL;
use Time::HiRes;

our $VERSION = '0.07';
my $MAX_THREADS = 10;
my $nthrs : shared = 0;

# always flush the STDOUT immediately:
$| = 1;


my ($ua, $start, $HOME, $proxy, %URLs);

{
    $start = time();

    $proxy = shift;
    if ($proxy) {
        $proxy = "http://$proxy"
            unless $proxy =~ m/^http:/o;
    }
    $ua = LWP::UserAgent->new;
    $ua->proxy( 'http', $proxy ) if $proxy;

    warn << "_EOC_";
GetCNN version $VERSION
Agent2002. All rights reserved.

_EOC_

    $HOME = "http://www.cnn.com";

    my $ua = LWP::UserAgent->new;
    $ua->proxy( 'http', $proxy ) if $proxy;

    my $html = get_page($HOME);
    unless ($html){
        die << "_EOC_";
Bad luck this time.
Please try next time.
_EOC_
    }

    %URLs = get_url_list( $html );
}

my $fsema = Thread::Semaphore->new(1);
my $queue = new Thread::Queue;

my %outputs : shared;
foreach (1..$MAX_THREADS) {
    threads->new(\&worker);
}

foreach my $url( keys %URLs ){
    #$sema->down;
    $queue->enqueue($url);
}

foreach (1..$MAX_THREADS) {
    $queue->enqueue(undef);
}

# Loop through all the threads 
#foreach my $thr (threads->list) { 
    # Don't join the main thread or ourselves 
    #if ($thr->tid && !threads::equal($thr, threads->self)) { 
    #    $thr->join; 
    #} 
#}

while ($nthrs != 0) {
    threads->yield;
}

foreach my $catalog (keys %outputs){
    write_file($catalog, $outputs{$catalog});
}

my $elapsed = time() - $start;
print "\nFor total $elapsed sec elapsed.\n";

sub worker {
    { lock($nthrs); $nthrs++; }
    #warn;
    while (1) {
        #warn;
        my $url = $queue->dequeue;
        last unless defined $url;
        #warn;
        process_url($url);
        #warn;
    }
    { lock($nthrs); $nthrs--; }
}

sub get_page{
    my $url = shift;

    my $tid = threads->tid();
    warn "Thread $tid: Accessing '$url'...\n";
    
    $url = URI::URL->new($url, $HOME)->abs;
    # warn "\$url = $url";
    my $res = $ua->get($url);
    unless ($res->is_success) {
        print "Thread $tid: Failed: ".$res->status_line."\n";
        return;
    }
    my $page = $res->content;
    print "Thread $tid: Done!\n" if $page;
    return $page;
}

sub get_url_list{
    my $html = shift;
    my %urls;
    my $tree = HTML::TreeBuilder->new;
    $tree->parse($html);
    my @ans = $tree->look_down( '_tag', 'a',
        sub {
            my $link = $_[0]->attr('href') or return;
            if ($link =~ m/index\.html?/io) {
                $urls{$link} = 1;
            }
        }
    );
    $tree = $tree->delete;
    return %urls;
}

sub process_url{
    #warn;
    my $url = shift;

    if( $url =~ m#/\d{4}/([A-Z]+)/#o ){
        my $catalog = $1;

        my $html = get_page($url);

        #print "Uping...", threads->tid, "\n";

        return unless $html;
        my $news = extract_news($html);
        # print "\$news = $news\n";
        defined( $news ) or
            warn "warning: there may be errors ".
                 "on CNN's page '$url'.\n\n";

        lock(%outputs);
        unless( exists $outputs{$catalog} ){
            $outputs{$catalog} = "\n<!--$url-->\n$news";
        }else{
            $outputs{$catalog} .= 
                "\n\n<!--$url-->\n$news";
        }
    }
    #sema->up;
}

sub extract_news{
    local $_ = shift;

    my $SI = qr/<!--startclickprintinclude-->/;
    my $EI = qr/<!--endclickprintinclude-->/;
    my $SE = qr/<!--startclickprintexclude-->/;
    my $EE = qr/<!--endclickprintexclude-->/;

    # collect contents between $SI and $EI:
    my $news = '';
    while( m/ $SI $EI |
              $SI ( (?:.(?!$EI))* . ) $EI
            /goixs ){
        $news .= $1 if defined $1;
    }

    # delete contents between $SE and $EE (if any):
    $news =~ s/ $SE $EE |
                $SE (?:.(?!$EE))* . $EE
              //goixs;

    return unless $news;
    
    $news =~ s/<img \s+ [^>]+>//giox;
    # die "images occured.\n" if( $news =~ m/<img/i );
   
    return $news;
}

sub get_date{
    my @tm_info = localtime();

    my $day = sprintf( "%02d", $tm_info[3] );
    my $mon = sprintf( "%02d", $tm_info[4] + 1 );
    my $year = sprintf( "%02d", $tm_info[5] % 100 );

    return "$year-$mon-$day";
}

sub write_file{
    my $catalog = shift;
    my $date = get_date();
    my $fname = "$date-$catalog.html";
    my $content = shift;

    open( my $fh, ">$fname" );
    if( !defined $fh ){
        die "cannot open '$fname' for writing\n";
    }

    my $timestr = scalar(localtime);
    my $html = << "_EOC_";
<!-- This file was generated by GetCNN $VERSION at $timestr -->
<html>

<head>
<title>CNN News -- $catalog -- $date</title>
</head>

<body>

$content

</body>

</html>
_EOC_

    $html =~ s/\n{3,}/\n\n/gs;
    $fsema->down;
    print $fh $html;
    $fsema->up;
    close( $fh );
    print "$fname generated.\n";
}

0;

__END__
:endofperl
