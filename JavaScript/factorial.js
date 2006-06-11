if (ARGV[0] == undefined || !ARGV[0].match(/^\d+$/)) {
    die( "usage: factorial.js N" )
}

say( factorial(ARGV[0]) );

function factorial (n) {
    if (n == undefined || n <= 0) { return 1 }
    return n * factorial(n-1);
}
