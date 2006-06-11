var num = arguments[0];
if (num == undefined || !num.match(/^\d+$/)) {
    die( "usage: factorial.js N" )
}

say( factorial(num) );

function factorial (n) {
    if (n == undefined || n <= 0) { return 1 }
    return n * factorial(n-1);
}
