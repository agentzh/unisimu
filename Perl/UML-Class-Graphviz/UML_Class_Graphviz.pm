package UML::Class::GraphViz;

use strict;
use warnings;
use Template;
#use File::Slurp 'write_file';

my $tt = Template->new;

my $dot_template = <<'_EOC_';
digraph uml {
    node [shape=record, fillcolor="#f1e1f4", style="filled"];
    edge [color=red, dir=none];

[%- name2id = {} %]
[%- id = 1 %]
[%- FOREACH class = classes %]
    [%- name = class.name %]
    [%- name2id.$name = id %]
    class_[% id %] [shape=plaintext, style="", label=<
<table BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
  <tr><td port="title" bgcolor="#f1e1f4">[% name %]</td></tr>
  <tr>
    <td>
    <table border="0" cellborder="0" cellspacing="0" cellpadding="1">
      <tr>
    <td><font color="red">
    [%- FOREACH method = class.methods %]
      [%- method.match("^_") ? "-" : "+" %]<br align="left"/>

    [%- END %]</font></td>
    <td port="methods" bgcolor="white" align="left">
    [%- FOREACH method = class.methods %]
      [%- method %]<br align="left"/>

    [%- END %]</td>
      </tr>
    </table>
    </td>
  </tr>
  <tr>
    <td port="properties" >
    <table border="0" cellborder="0" cellspacing="0" cellpadding="0">
      <tr>
    <td><font color="red">
    [%- FOREACH property = class.properties %]
      [%- property.match("^_") ? "-" : "+" %]<br align="left"/>

    [%- END %]</font></td>
    <td bgcolor="white" align="left">
    [%- FOREACH property = class.properties %]
      [%- property %]<br align="left"/>

    [%- END %]</td>
      </tr>
    </table>
    </td>
  </tr>
</table>>];
  [%- id = id + 1 %]
[% END %]
[%- class_id = id %]

    node [shape="triangle", fillcolor=yellow, height=0.3, width=0.3];
[%- id = 0 %]
[%- FOREACH class = classes %]
  [%- id = id + 1 %]
  [%- super = class.name %]
  [%- NEXT IF !class.subclasses %]
     angle_[% id %] [label=""];

  [%- super_id = name2id.$super %]
     class_[% super_id %]:properties -> angle_[% id %]
  [%- FOREACH child = class.subclasses %]
    [%- child_id = name2id.$child %]
    [%- IF !child_id %]
     class_[% class_id %] [shape=record, label="[% child %]" fillcolor="#f1e1f4", style="filled"];
     angle_[% id %] -> class_[% class_id %]
        [%- class_id = class_id + 1 %]
      [%- ELSE %]
     angle_[% id %] -> class_[% child_id %]:title
    [%- END %]
  [%- END %]
[%- END %]

}
_EOC_

sub as_dot {
    my ($class, $data) = @_;
    my $dot;
    $tt->process(\$dot_template, $data, \$dot)
        || die $tt->error(), "\n";
    $dot
}

1;
