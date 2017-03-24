# @FEATURES classes, class_parameters
# @EXPECT_FAIL 
#% Parameters cannot be reassigned

class main ( $p = "default" ) {
	$p = "the password"
	user { "alice": password => $p }
}
