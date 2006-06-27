use lex;

my $file = shift || 'tiny';
print "step 1 - scanning words in $file\n\n",'-'x 50, "\n", "value\t\ttype\t\tline\n";
for(get_words($file)) {
	print join "\t\t", @$_, "\n";
}