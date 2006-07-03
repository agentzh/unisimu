#compile.t - a script for testing function compile of 
#GraphViz::Flowchart::Asm->compile
#2006-07-02 2006-07-02

use GraphViz::Flowchart::Asm;
use Test::Base;
plan tests => 2 * blocks();

sub filter {
	my $old = shift;
	$old =~ s/\n.*$//;
	return $old
}

run {
	my $block = shift;

	open F, ">tmp";		
	print F $block->input;	
	close F;
	
	my $stdout;
	
	tie_output(*STDERR, $stdout);
	$stdout ||= '';
	my $asm = GraphViz::Flowchart::Asm->new();
	
	is $asm->compile('tmp'), $block->expected;
	is filter($stdout), $block->tip, "error tip!";
};


__DATA__

=== test 1 : valid string 
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
--- expected chomp
1
--- tip

=== test 2 : valid string
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
--- expected chomp
1
--- tip

=== test 3 : invalid string - body syntax error
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
    结束

--- expected chomp
0
--- tip
syntax error: tmp: line 11: unknown instruction "结束"

=== test 4 : invalid string - top syntax error
--- input
    encoding
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
--- expected chomp
0
--- tip
syntax error: tmp: line 1: unknown instruction "encoding"

=== test 5 : invalid string - top syntax error
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
    end
--- expected chomp
1
--- tip
