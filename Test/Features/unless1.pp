#@FEATURES statements, unless
# basic unless statement
# https://docs.puppet.com/puppet/latest/lang_conditional.html#unless-statements

class main {
	$os = "Darwin"
	unless $os == "Darwin" {
		file { afile: }
	}
	unless $os != "Darwin" {
		file { bfile: }
	} else {
		file { cfile: }
	}
 }

