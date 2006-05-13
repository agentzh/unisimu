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
  <branch>
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
  </branch>
</expression>



=== TEST 3:
--- re
a|b|c
--- out
<expression>
  <branch>
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
  </branch>
</expression>



=== TEST 4:
--- re
((aa))*
--- out
<expression>
  <branch>
    <concat>
      <modified_atom>
        <atom>
          <expression>
            <branch>
              <concat>
                <modified_atom>
                  <atom>
                    <expression>
                      <branch>
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
                      </branch>
                    </expression>
                  </atom>
                </modified_atom>
              </concat>
            </branch>
          </expression>
        </atom>
        <modifier>*</modifier>
      </modified_atom>
    </concat>
  </branch>
</expression>
--- LAST


=== TEST 5;
--- re
ab*c
--- out
ab*c



=== TEST 6:
--- re
a  (b| )*
--- out
a  (b| )*
