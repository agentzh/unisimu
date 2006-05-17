# basic.t

use Test::Base;
use re::XML;

plan tests => 1 * blocks();

sub unindent {
    my $s = shift;
    $s =~ s/^\s+//mgs;
    $s;
}

filters {
    out => [qw< unindent >],
};

run {
    my $block = shift;
    if (defined $block->out) {
        my $re = $block->re;
        chomp $re;
        is re::XML::translate($re), $block->out, $block->name;
    } elsif ($block->error) {
        ok !defined translate($block->re), $block->name;
    }
};

__DATA__

=== TEST 2:
--- re
abc
--- out
<expression>
  <alternation>
    <concat>
      <modified_atom>
        <atom>
          <char>a</char>
        </atom>
      </modified_atom>
      <modified_atom>
        <atom>
          <char>b</char>
        </atom>
      </modified_atom>
      <modified_atom>
        <atom>
          <char>c</char>
        </atom>
      </modified_atom>
    </concat>
  </alternation>
</expression>



=== TEST 3:
--- re
a|b|c
--- out
<expression>
  <alternation>
    <concat>
      <modified_atom>
        <atom>
          <char>a</char>
        </atom>
      </modified_atom>
    </concat>
    <concat>
      <modified_atom>
        <atom>
          <char>b</char>
        </atom>
      </modified_atom>
    </concat>
    <concat>
      <modified_atom>
        <atom>
          <char>c</char>
        </atom>
      </modified_atom>
    </concat>
  </alternation>
</expression>



=== TEST 4:
--- re
((aa))*
--- out
<expression>
  <alternation>
    <concat>
      <modified_atom>
        <atom>
          <expression>
            <alternation>
              <concat>
                <modified_atom>
                  <atom>
                    <expression>
                      <alternation>
                        <concat>
                          <modified_atom>
                            <atom>
                              <char>a</char>
                            </atom>
                          </modified_atom>
                          <modified_atom>
                            <atom>
                              <char>a</char>
                            </atom>
                          </modified_atom>
                        </concat>
                      </alternation>
                    </expression>
                  </atom>
                </modified_atom>
              </concat>
            </alternation>
          </expression>
        </atom>
        <modifier>*</modifier>
      </modified_atom>
    </concat>
  </alternation>
</expression>



=== TEST 5:
--- re:
--- out
<expression>
<alternation>
<concat>
</concat>
</alternation>
</expression>



=== TEST 6;
--- re
ab*c
--- out
<expression>
<alternation>
<concat>
<modified_atom>
<atom>
<char>a</char>
</atom>
</modified_atom>
<modified_atom>
<atom>
<char>b</char>
</atom>
<modifier>*</modifier>
</modified_atom>
<modified_atom>
<atom>
<char>c</char>
</atom>
</modified_atom>
</concat>
</alternation>
</expression>



=== TEST 7:
--- re
a  (b| )*
--- out
<expression>
<alternation>
<concat>
<modified_atom>
<atom>
<char>a</char>
</atom>
</modified_atom>
<modified_atom>
<atom>
<char> </char>
</atom>
</modified_atom>
<modified_atom>
<atom>
<char> </char>
</atom>
</modified_atom>
<modified_atom>
<atom>
<expression>
<alternation>
<concat>
<modified_atom>
<atom>
<char>b</char>
</atom>
</modified_atom>
</concat>
<concat>
<modified_atom>
<atom>
<char> </char>
</atom>
</modified_atom>
</concat>
</alternation>
</expression>
</atom>
<modifier>*</modifier>
</modified_atom>
</concat>
</alternation>
</expression>
