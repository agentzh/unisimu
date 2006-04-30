#: proc.t

use t::Kid_XML;

plan tests => 1 * blocks();

filters {
    xml => 'unindent',
};

run_tests;

__DATA__

=== TEST 1
--- kid
b:=foo(x);

proc foo(a) { foo:=a+1; }
--- xml
<?xml version="1.0"?>
<program>
  <statement_list>
    <statement>
      <assignment>
        <var>
        <identifier>b</identifier>
        </var>
        <expression>
          <term>
            <factor>
              <proc_call>
                <identifier>foo</identifier>
                <expression_list>
                  <expression>
                    <term>
                      <factor>
                        <var>
                          <identifier>x</identifier>
                        </var>
                      </factor>
                    </term>
                  </expression>
                </expression_list>
              </proc_call>
            </factor>
          </term>
        </expression>
      </assignment>
    </statement>
    <statement>
      <declaration>
        <proc_decl>
          <identifier>foo</identifier>
          <identifier_list>
            <identifier>a</identifier>
          </identifier_list>
          <block>
            <statement_list>
              <statement>
                <assignment>
                  <var>
                    <identifier>foo</identifier>
                  </var>
                  <expression>
                    <term>
                      <factor>
                        <var>
                          <identifier>a</identifier>
                        </var>
                      </factor>
                    </term>
                    <op>+</op>
                    <term>
                      <factor>
                        <number>1</number>
                      </factor>
                    </term>
                  </expression>
                </assignment>
              </statement>
            </statement_list>
          </block>
        </proc_decl>
      </declaration>
    </statement>
  </statement_list>
</program>
