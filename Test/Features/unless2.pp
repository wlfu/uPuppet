#@FEATURES statements, unless
# @EXPECT_FAIL
# "If an elsif clause is included in an “unless” statement,
# it is a syntax error and will cause compilation to fail"
# https://docs.puppet.com/puppet/latest/lang_conditional.html#unless-statements

class main {
	$os = "Darwin"
	unless $os == "Darwin" {
		file { afile: }
	} elseif $os != "Darwin" {
		file { bfile: }
	}
 }
