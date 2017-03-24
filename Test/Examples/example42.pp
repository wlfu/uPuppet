# @FEATURES classes, class_variables
# @EXPECT_FAIL
#% It is not possible to assign to out-of-scope variables.

class main () {
	$var = "value"
	include main::sub
}

class main::sub {
	$main::var = "new value"
}
