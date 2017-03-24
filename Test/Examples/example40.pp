# @FEATURES classes, class_parameters
# @EXPECT_FAIL
#% When a class has parameters, you can't include it more than once (compare figure \ref{exfig:example7})
#% because there may be different values for the parameter.

class main {
	file { "file": }
	class { main::sub: owner => "alice" }
	class { main::sub: owner => "bob" }
}

class main::sub ( $owner = "carol" ) {
	file { "file1": owner => $owner }
}
