#: assign.t

use t::Kid_XML;

plan tests => 1 * blocks();

filters {
    xml => 'unindent',
};

run_tests;

__DATA__

=== TEST 1
--- kid
a := 3 + 5*2
--- xml
<?xml version="1.0"?>
<program>
  <statement_list>
    <statement>
      <assignment>
        <var>
          <identifier>a</identifier>
        </var>
        <expression>
          <term>
            <factor>
              <number>3</number>
            </factor>
          </term>
            <op>+</op>
          <term>
            <factor>
              <number>5</number>
            </factor>
            <op>*</op>
            <factor>
              <number>2</number>
            </factor>
          </term>
        </expression>
      </assignment>
    </statement>
  </statement_list>
</program>



=== TEST 2
--- kid
a:=1+3-     2 + 5- 6- 8
--- xml
<?xml version="1.0"?>
<program>
  <statement_list>
    <statement>
      <assignment>
        <var>
          <identifier>a</identifier>
        </var>
        <expression>
          <term>
            <factor>
              <number>1</number>
            </factor>
          </term>
          <op>+</op>
          <term>
            <factor>
              <number>3</number>
            </factor>
          </term>
          <op>-</op>
          <term>
            <factor>
              <number>2</number>
            </factor>
          </term>
          <op>+</op>
          <term>
            <factor>
              <number>5</number>
            </factor>
          </term>
          <op>-</op>
          <term>
            <factor>
              <number>6</number>
            </factor>
          </term>
          <op>-</op>
          <term>
            <factor>
              <number>8</number>
            </factor>
          </term>
        </expression>
      </assignment>
    </statement>
  </statement_list>
</program>



=== TEST 3
--- kid
d := 5
--- xml
<?xml version="1.0"?>
<program>
  <statement_list>
    <statement>
      <assignment>
        <var>
          <identifier>d</identifier>
        </var>
        <expression>
          <term>
            <factor>
              <number>5</number>
            </factor>
          </term>
        </expression>
      </assignment>
    </statement>
  </statement_list>
</program>



=== TEST 4
--- kid
ebc := (25)
--- xml
<?xml version="1.0"?>
<program>
  <statement_list>
    <statement>
      <assignment>
        <var>
          <identifier>ebc</identifier>
        </var>
        <expression>
          <term>
            <factor>
              <expression>
                <term>
                  <factor>
                    <number>25</number>
                  </factor>
                </term>
              </expression>
            </factor>
          </term>
        </expression>
      </assignment>
    </statement>
  </statement_list>
</program>



=== TEST 5
--- kid
a_b := ((25+5123))
--- xml
<?xml version="1.0"?>
<program>
  <statement_list>
    <statement>
      <assignment>
        <var>
          <identifier>a_b</identifier>
        </var>
        <expression>
          <term>
            <factor>
              <expression>
                <term>
                  <factor>
                    <expression>
                    <term>
                      <factor>
                        <number>25</number>
                      </factor>
                    </term>
                    <op>+</op>
                    <term>
                      <factor>
                        <number>5123</number>
                      </factor>
                    </term>
                    </expression>
                  </factor>
                </term>
              </expression>
            </factor>
          </term>
        </expression>
      </assignment>
    </statement>
  </statement_list>
</program>



=== TEST 6
--- kid
e := 3/5
--- xml
<?xml version="1.0"?>
<program>
  <statement_list>
    <statement>
      <assignment>
        <var>
          <identifier>e</identifier>
        </var>
      <expression>
        <term>
          <factor>
            <number>3</number>
          </factor>
          <op>/</op>
          <factor>
            <number>5</number>
          </factor>
        </term>
      </expression>
      </assignment>
    </statement>
  </statement_list>
</program>



=== TEST 7
--- kid
e := 3/(a+5)
--- xml
<?xml version="1.0"?>
<program>
<statement_list>
<statement>
<assignment>
<var>
<identifier>e</identifier>
</var>
<expression>
<term>
<factor>
<number>3</number>
</factor>
<op>/</op>
<factor>
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
<number>5</number>
</factor>
</term>
</expression>
</factor>
</term>
</expression>
</assignment>
</statement>
</statement_list>
</program>



=== TEST 8
--- kid
e := 5+(3*6+3/5 ) * bb-3*c
--- xml
<?xml version="1.0"?>
<program>
<statement_list>
<statement>
<assignment>
<var>
<identifier>e</identifier>
</var>
<expression>
<term>
<factor>
<number>5</number>
</factor>
</term>
<op>+</op>
<term>
<factor>
<expression>
<term>
<factor>
<number>3</number>
</factor>
<op>*</op>
<factor>
<number>6</number>
</factor>
</term>
<op>+</op>
<term>
<factor>
<number>3</number>
</factor>
<op>/</op>
<factor>
<number>5</number>
</factor>
</term>
</expression>
</factor>
<op>*</op>
<factor>
<var>
<identifier>bb</identifier>
</var>
</factor>
</term>
<op>-</op>
<term>
<factor>
<number>3</number>
</factor>
<op>*</op>
<factor>
<var>
<identifier>c</identifier>
</var>
</factor>
</term>
</expression>
</assignment>
</statement>
</statement_list>
</program>



=== TEST 9
--- kid
e := 5*(3-5-(7+3))
--- xml
<?xml version="1.0"?>
<program>
<statement_list>
<statement>
<assignment>
<var>
<identifier>e</identifier>
</var>
<expression>
<term>
<factor>
<number>5</number>
</factor>
<op>*</op>
<factor>
<expression>
<term>
<factor>
<number>3</number>
</factor>
</term>
<op>-</op>
<term>
<factor>
<number>5</number>
</factor>
</term>
<op>-</op>
<term>
<factor>
<expression>
<term>
<factor>
<number>7</number>
</factor>
</term>
<op>+</op>
<term>
<factor>
<number>3</number>
</factor>
</term>
</expression>
</factor>
</term>
</expression>
</factor>
</term>
</expression>
</assignment>
</statement>
</statement_list>
</program>



=== TEST 10
--- kid
a:=3+2; b:=5*6  ;c:=1*2
--- xml
<?xml version="1.0"?>
<program>
  <statement_list>
    <statement>
      <assignment>
        <var>
          <identifier>a</identifier>
        </var>
        <expression>
          <term>
            <factor>
              <number>3</number>
            </factor>
          </term>
          <op>+</op>
          <term>
            <factor>
              <number>2</number>
            </factor>
          </term>
        </expression>
      </assignment>
    </statement>
    <statement>
      <assignment>
        <var>
          <identifier>b</identifier>
        </var>
        <expression>
          <term>
            <factor>
              <number>5</number>
            </factor>
            <op>*</op>
            <factor>
              <number>6</number>
            </factor>
          </term>
        </expression>
      </assignment>
    </statement>
    <statement>
      <assignment>
        <var>
          <identifier>c</identifier>
        </var>
        <expression>
          <term>
            <factor>
              <number>1</number>
            </factor>
            <op>*</op>
            <factor>
              <number>2</number>
            </factor>
          </term>
        </expression>
      </assignment>
    </statement>
  </statement_list>
</program>



=== TEST 11
--- kid
x:=-y;
--- xml
<?xml version="1.0"?>
<program>
  <statement_list>
    <statement>
      <assignment>
        <var>
          <identifier>x</identifier>
        </var>
        <expression>
          <term>
            <negative/>
            <factor>
              <var>
                <identifier>y</identifier>
              </var>
            </factor>
          </term>
        </expression>
      </assignment>
    </statement>
  </statement_list>
</program>
