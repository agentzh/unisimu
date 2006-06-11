// lexer.js
// 2006-05-19 2006-06-11

var infile = arguments[0];
if (infile == undefined)
    die('usage: lexer.js <infile>\n');
var src = readFile(infile);
print(src);
