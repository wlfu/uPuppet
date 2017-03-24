# @FEATURES resources, variables
# @EXPECT_FAIL
#% Variables cannot be re-assigned.

class main {
	$owner = "alice"
	file { "somefile": owner => $owner }
	$owner = "bob"
}
