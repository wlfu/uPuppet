# @FEATURES resources, variables
# @OPTIONS --strict_variables @EXPECT_FAIL
#% Forward references to variables are not supported (unlike resource references).
#% Undefined variables have the special value {\tt undef}. If you assign this value
#% to an attribute, the attribute doesn't appear in the catalog.

class main {
	file { "file":
		group => $grp,
 		mode => "0755"
	 }
 	$grp = "the group"

}
