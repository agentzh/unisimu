use strict;
use warnings;
my @terminal = ('(', 'number', ')', '+', '-', '*', , '<', 'if', 'read', 'write', 'identifier',
				'repeat', 'until', 'end', 'then', 'else', ';','#');
my $VAR1 = {
          'comparison-op' => {
                               '=' => '=',
                               '<' => '<'
                             },
          'read-stmt' => {
                           'read' => 'read identifier'
                         },
          'stmt-sequence' => {
                               'identifier' => 'statement stmt-sequence@',
                               'read' => 'statement stmt-sequence@',
                               'repeat' => 'statement stmt-sequence@',
                               'write' => 'statement stmt-sequence@',
                               'if' => 'statement stmt-sequence@'
                             },
          'assign-stmt' => {
                             'identifier' => 'identifier := exp'
                           },
          'factor' => {
                        'identifier' => 'identifier',
                        'number' => 'number',
                        '(' => '( exp )'
                      },
          'mulop' => {
                       '/' => '/',
                       '*' => '*'
                     },
          'term' => {
                      'identifier' => 'factor term@',
                      'number' => 'factor term@',
                      '(' => 'factor term@'
                    },
          'program' => {
                         'identifier' => 'stmt-sequence',
                         'read' => 'stmt-sequence',
                         'repeat' => 'stmt-sequence',
                         'write' => 'stmt-sequence',
                         'if' => 'stmt-sequence'
                       },
          'simple-exp' => {
                            'identifier' => 'term simple-exp@',
                            'number' => 'term simple-exp@',
                            '(' => 'term simple-exp@'
                          },
          'term@' => {
                       '/' => 'mulop factor term@',
                       '=' => '@',
                       '#' => '@',
                       'else' => '@',
                       '*' => 'mulop factor term@',
                       '+' => '@',
                       'end' => '@',
                       '-' => '@',
                       'until' => '@',
                       ')' => '@',
                       'then' => '@',
                       ';' => '@',
                       '<' => '@'
                     },
          'statement' => {
                           'identifier' => 'assign-stmt',
                           'read' => 'read-stmt',
                           'repeat' => 'repeat-stmt',
                           'write' => 'write-stmt',
                           'if' => 'if-stmt'
                         },
          'simple-exp@' => {
                             '=' => '@',
                             '#' => '@',
                             'else' => '@',
                             'end' => '@',
                             '+' => 'addop term simple-exp@',
                             '-' => 'addop term simple-exp@',
                             'until' => '@',
                             ')' => '@',
                             'then' => '@',
                             ';' => '@',
                             '<' => '@'
                           },
          'exp' => {
                     'identifier' => 'simple-exp exp@',
                     'number' => 'simple-exp exp@',
                     '(' => 'simple-exp exp@'
                   },
          'addop' => {
                       '-' => '-',
                       '+' => '+'
                     },
          'write-stmt' => {
                            'write' => 'write exp'
                          },
          'exp@' => {
                      '=' => 'comparison-op simple-exp exp@',
                      '#' => '@',
                      'else' => '@',
                      'end' => '@',
                      'until' => '@',
                      ')' => '@',
                      'then' => '@',
                      '<' => 'comparison-op simple-exp exp@',
                      ';' => '@'
                    },
          'if-stmt' => {
                         'if' => 'if exp then stmt-sequence if-stmt@'
                       },
          'repeat-stmt' => {
                             'repeat' => 'repeat stmt-sequence until exp'
                           },
          'if-stmt@' => {
                          'else' => 'else stmt-sequence end',
                          'end' => 'end'
                        },
          'stmt-sequence@' => {
                                'until' => '@',
                                '#' => '@',
                                'else' => '@',
                                ';' => '; statement stmt-sequence@',
                                'end' => '@'
                              }
        };

print '<table width = 1800 height = 200 border = 1 align = center>
	<tr>
		<td> M[N, T]</td>
';

my $string1 = "";
for(@terminal) {
	$string1 .= '		<td align=center ><font size = 2>'.$_.'</font></td>'."\n";
}
$string1 .= '	</tr>';

print $string1;

for my $key(keys %$VAR1) {
	print "\t<tr>\n\t\t<td>".$key."</td>\n";
	
	for(@terminal) {
		if(exists $VAR1->{$key}{$_}) {
			print "\t\t<td align=center ><font size = 1>$key -> ".$VAR1->{$key}{$_}."</font></td>\n";
		} else {
			print "\t\t<td>&nbsp </td>\n";
		}		
	}
	print "\t</tr>\n";
}

print "</table>";
		