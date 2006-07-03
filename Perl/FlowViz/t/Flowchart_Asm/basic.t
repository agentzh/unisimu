#basic.t - a script for testing function :
#GraphViz::Flowchart::Asm->graphviz();

#2006-07-02 2006-7-2

use GraphViz::Flowchart::Asm;
use File::Compare;
use Test::Base;
plan tests => 1 * blocks;

run {
	my $obj = GraphViz::Flowchart::Asm->new();

	my $block = shift;
	
	open F, ">tmp1";		
	print F $block->input;	
	close F;
	$obj->compile('tmp1');

	my $outfile = $block->outfile;
	my $tempfile;
	($tempfile = $outfile) =~ s/~//;
	open OF, ">$tempfile";
	my $gv = $obj->graphviz();
	print OF $gv->as_debug();
	close OF;
	is compare($tempfile, $outfile), 0, "debug file $outfile;";
	$gv->as_png("$outfile.png");
	

}


__DATA__

=== test 1
--- input
    encoding gb2312
    font     simsun.ttc

    start 开始
    do    来到十字路口
    test  绿灯是否亮着？
    jyes  L1
    do    等绿灯亮起
L1:
    do    穿过十字路口
    end   结束
--- outfile chomp
~tmp1

=== test 2
--- input
    encoding gb2312
    font     simsun.ttc

    test 有调用者吗？
    jyes L1
    do   入等待队列 
L1:
    do   从 FIFO 中取第一个
    test 有输入参数吗？
    jno  L2
    do   取输入参数
L2:
    test 有语句序列？
    jno  L3
    jyes L5
L5:
    do   执行之
L3:
    test 有输出参数吗？
    jno  L4
    do   送输出参数
L4:
    do   唤醒调用者

--- outfile chomp
~tmp2

=== test 3
--- input
    encoding gb2312
    font     simsun.ttc

    start 开始
    do    来到十字路口
    test  绿灯是否亮着？
    jyes  L1
    do    等绿灯亮起
L1:
    do    穿过十字路口
    end   结束
--- outfile chomp
~tmp3

=== test 4 : invalid string - top syntax error
--- input
    encoding gb2312
    font     simsun.ttc
    width    2
    height   6

    start 开始运行
L1:
    io    输入
    do    求解
    test  结束？
    jno   L1
    end   退出
--- outfile chomp
~tmp4
