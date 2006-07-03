use GraphViz::Flowchart::C;
use Test::Base;
plan tests => 1 * blocks();

run {
	my $block = shift;
	my $obj = GraphViz::Flowchart::C->new();
	$obj->compile_string($block->input);
	is $obj->as_asm, $block->expected, "compare .asm code!";
	#print $obj->as_asm, "\n", '_' x 50, "\n\n";
}

__DATA__

=== test 1
--- input 
font cour.ttf

if (a > b) {
    io a is greater than b!
} else if (a == b) {
    io a is equal to b!
} else {
    io a is less than b!
}

if (a < 0)
    do a = -a

if (b <= 0) {
    do b = -b
    io output b
}

--- expected 
    font cour.ttf
    # line 9: if-else statement:
    test a > b ?
    jno _FV_L3
    io a is greater than b!

    jmp _FV_L2
_FV_L3:
    # line 9: if-else statement:
    test a == b ?
    jno _FV_L1
    io a is equal to b!

    jmp _FV_L2
_FV_L1:
    io a is less than b!

_FV_L2:

    # line 12: if statement:
    test a < 0 ?
    jno _FV_L5
    do a = -a

_FV_L5:
    # line 17: if statement:
    test b <= 0 ?
    jno _FV_L7
    do b = -b
    io output b

_FV_L7:

    end End

=== test 2
--- input
font cour.ttf

if 'a > 5'
    if <a < 10>
        io "a is between 5 and 10!"
    else
        io "a is greater than 5!"

if "b == 0"
    do b = -1

while (i > 0) {
    do Do some calculations
    do i = i + 1
}

while (*p != '\0')
    do p++

if <p == '\0'>
    io print "Reached the end\n of the input"
else
    io print "blah blah blah..."

--- expected
    font cour.ttf
    # line 7: if statement:
    test a > 5 ?
    jno _FV_L12
    # line 7: if-else statement:
    test a < 10 ?
    jno _FV_L11
    io a is between 5 and 10!

    jmp _FV_L12
_FV_L11:
    io a is greater than 5!

_FV_L12:

    # line 10: if statement:
    test b == 0 ?
    jno _FV_L15
    do b = -1

_FV_L15:
    # line 15: while statement:
    test i > 0 ?
    jno _FV_L19
    do Do some calculations
    do i = i + 1

    jmp _FV_L15
_FV_L19:
    # line 18: while statement:
    test *p != '\0' ?
    jno _FV_L22
    do p++

    jmp _FV_L19
_FV_L22:
    # line 23: if-else statement:
    test p == '\0' ?
    jno _FV_L23
    io print "Reached the end\n of the input"

    jmp _FV_L24
_FV_L23:
    io print "blah blah blah..."

_FV_L24:

    end End

=== test 3
--- input
    encoding gb2312
    font     simsun.ttc

    start 开始
    do    取一条指令
L1:
    do    取指令中访问的页号 --> L
L2:
    do 查页表
    if (该页标志 = 1) {
        do 形成绝对地址
        if (是存指令)
            do 置 L 页修改标志为"1"
        io 输出绝对地址
		if (有后继指令) {
			do 取下一条指令
			jmp L1
		} else
			end 结束
	} else {
		do j := P[k]
		if (j 页修改标志为1)
			io 输出"out j"
		io 输出"in L"
		do P[k] := L, k := (k+1) mod m
		do 修改页表
		jmp L2
	}
--- expected
    encoding gb2312
    font simsun.ttc
    start 开始
    do 取一条指令
L1:
        do 取指令中访问的页号 --> L

L2:
        do 查页表

    # line 28: if-else statement:
    test 该页标志 = 1 ?
    jno _FV_L31
    do 形成绝对地址
    # line 13: if statement:
    test 是存指令 ?
    jno _FV_L25
    do 置 L 页修改标志为"1"

_FV_L25:
    io 输出绝对地址
    # line 19: if-else statement:
    test 有后继指令 ?
    jno _FV_L27
    do 取下一条指令
    jmp L1

    jmp _FV_L28
_FV_L27:
    end 结束

_FV_L28:

    jmp _FV_L32
_FV_L31:
    do j := P[k]
    # line 23: if statement:
    test j 页修改标志为1 ?
    jno _FV_L29
    io 输出"out j"

_FV_L29:
    io 输出"in L"
    do P[k] := L, k := (k+1) mod m
    do 修改页表
    jmp L2

_FV_L32:

    end End
