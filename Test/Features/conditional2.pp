#@FEATURES statements, if
# elsif
# https://docs.puppet.com/puppet/latest/lang_conditional.html#if-statements

class main {
	$os = "Darwin"
	if $os == "Darwin" {
		file { afile: }
	} elsif $os != "Darwin" {
		file { bfile: }
	}
 }
