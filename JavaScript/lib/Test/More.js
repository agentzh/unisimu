Test = {};

Test.More = {};

Test.More.CurrTest = 0;

plan = function (arg) {
    say ("1..", arg.tests);
};

function is (got, expect, desc) {
    Test.More.CurrTest++;
	if (got == expect) {
        say( "ok ", Test.More.CurrTest );
	} else {
        say( "not ok ", Test.More.CurrTest );
    }
}

function like (got, regex, desc) {
    Test.More.CurrTest++;
    if (got.match(regex)) {
        say( "ok ", Test.More.CurrTest );
    } else {
        say( "not ok ", Test.More.CurrTest );
    }
}

function ok (value, desc) {
    Test.More.CurrTest++;
    if (value) {
        say( "ok ", Test.More.CurrTest );
    } else {
        say( "not ok ", Test.More.CurrTest );
    }
}
