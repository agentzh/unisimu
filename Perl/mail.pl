#: mail.pl
#: Send email via SMTP server automatically
#: Copyright (c) 2005 Agent Zhang
#: 2005-11-25 2005-11-25

use strict;
use warnings;
use Net::SMTP;
use Authen::SASL qw(Perl);

my ($user, $password) = ('agent2002', '840424');

my $sasl = Authen::SASL->new(
    mechanism => 'LOGIN',
    callback  => {
      user => $user,
      pass => $password,
    },
);

my $smtp = Net::SMTP->new(
    'smtp.126.com',
    Timeout => 60,
    Debug => 1,
);
$smtp->auth($sasl);
$smtp->mail('agent2002@126.com');
#$smtp->to('zhongxiang721@163.com');
$smtp->to('real_agent2002@163.com');
#$smtp->to('agent2002@126.com');
$smtp->data();
$smtp->datasend("From: 章亦春 [agent2002\@126.com]\n");
my $time = localtime;
$smtp->datasend("Sent: $time\n");
$smtp->datasend("To: 仲伟祥\n");
$smtp->datasend("Subject:  通过 Perl 脚本自动发送的电子邮件\n");
$smtp->datasend("\n");
my $text = <<'_EOC_';
祥子~~

我终于成功了！这封电子邮件就是通过 CPAN 模块 Net::SMTP_auth 自
动发送的。

原先我们试尝的 Net::STMP 不支持我们国内 SMTP 的 LOGIN、PLAIN
和 NTLM 认证方式，所以我在测试的时候，和你一样，总是收到下面这条
出错提示：

    535 Error: authentication failed

后来我在 CPAN 中进行搜索，一下子便找到了可爱的 Net::STMP_auth
模块。它继承自 Net::STMP，但是提供了有效的 authentication 服务，
所以我最终取得了成功。

我的程序的源代码如下所示：

    use Net::SMTP_auth;

    my ($user, $password) = ('agent2002', '123456');

    my $smtp = Net::SMTP_auth->new(
        'smtp.126.com',
        Timeout => 60,
        Debug => 1,
    );
    $smtp->auth('LOGIN', $user, $password);
    $smtp->mail('agent2002@126.com');
    $smtp->to('zhongxiang721@163.com');
    $smtp->data();
    $smtp->datasend("From: 章亦春 [agent2002\@126.com]\n");
    my $time = localtime;
    $smtp->datasend("Sent: $time\n");
    $smtp->datasend("To: 仲伟祥\n");
    $smtp->datasend("Subject:  通过 Perl 脚本自动发送的电子邮件\n");
    $smtp->datasend("\n");
    my $text = <<'_EOC_';
        祥子~~

        我终于成功了！这封电子邮件就是通过 CPAN 模块 Net::SMTP_auth 自
        动发送的。

        ...

            章亦春
    _EOC_

    open my $in, '<', \$text;
    while (<$in>) {
        $smtp->datasend($_);
    }
    close $in;

    $smtp->dataend();
    $smtp->quit;

很简单，不是么？下面是程序运行时产生的调试输出：

    <<< 220 126.com Coremail SMTP(Anti Spam) System
    >>> EHLO localhost.localdomain
    <<< 250-mail
    <<< 250-PIPELINING
    <<< 250-AUTH LOGIN PLAIN NTLM
    <<< 250-AUTH=LOGIN PLAIN NTLM
    <<< 250 8BITMIME
    >>> AUTH LOGIN
    <<< 334 dXNlcm5hbWU6
    >>> YWdlbnQyMDAy
    <<< 334 UGFzc3dvcmQ6
    >>> ODQwNDI0
    <<< 235 Authentication successful
    >>> MAIL FROM:<agent2002@126.com>
    <<< 250 Mail OK
    >>> RCPT TO:<zhongxiang721@163.com>
    <<< 250 Mail OK
    >>> DATA
    <<< 354 End data with <CR><LF>.<CR><LF>
    >>> From: 章亦春 [agent2002@126.com]
    >>> Sent: Fri Nov 25 16:06:02 2005
    >>> To: 仲伟祥
    >>> Subject:  通过 Perl 脚本自动发送的电子邮件
    >>> 祥子~~
    >>> 我终于成功了！这封电子邮件就是通过 CPAN 模块 Net::SMTP_auth
    ...
    >>> 究了。
    >>> 好好干！
    >>>     章亦春
    >>> .
    <<< 250 Mail OK queued as smtp4,agBJE9W4hkO4_MkB.2

    >>> QUIT
    <<< 221 Bye

呵呵，126 的 SMTP 服务器最后还跟我说 Bye 呢，真是够人性化的。:=)

有一点非常重要的问题必须予以强调，那就是 163 信箱的反病毒引擎强制要求
电子邮件的From 字段中包含发件人的电子信箱地址，否则 163 会自动退信。
如果我在这里使用的是

    From: 章亦春

而不是

    From: 章亦春 [agent2002@126.com]

163 信箱会自动退信。这必须要注意。经测试，126 信箱没有这种“无理要求”。

不过我现在不大清楚如何通过该模块发送含有附件的电子邮件以及是否可以发送
HTML 代码。这些课题还是交给你去做吧。我是没有更多的时间来研究了。

好好干吧！

    章亦春
_EOC_

open my $in, '<', \$text;
while (<$in>) {
    $smtp->datasend($_);
}
close $in;

$smtp->dataend();
$smtp->quit;
