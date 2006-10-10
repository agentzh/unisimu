use strict;
use warnings;

use Java_Swing_Extend;
use Java::Swing;

my $frame  = new JFrame;
$frame->setTitle("Testing JLabel");
my $container = $frame->getContentPane;
$container->setLayout(FlowLayout->new);

my $label1 = new JLabel('Label with text');
$label1->setToolTipText('This is label1');
$container->add($label1);

my $icon = new ImageIcon("cherry.jpg");
my $label2 = JLabel->new(
    "Label with text and icon",
    $icon,
    SwingConstants->LEFT,
);
$label2->setToolTipText("This is label2");
$container->add($label2);

my $label3 = new JLabel({
    text                   => "Label with icon and text at bottom",
    icon                   => $icon,
    horizontalTextPosition => SwingConstants->CENTER,
    verticalTextPosition   => SwingConstants->BOTTOM,
    toolTipText            => "This is label3",
});
$container->add($label3);

$frame->setSize(280, 280);
$frame->show;

my $swinger = new Java::Swing;
$swinger->connect(
    "WindowListener", $frame,
    { windowClosing => sub { $swinger->stop } }
);

$swinger->start;
