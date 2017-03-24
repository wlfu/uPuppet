#@FEATURES statements, if
# basic conditional statement
# https://docs.puppet.com/puppet/latest/lang_conditional.html#if-statements

class main {
	$os = "Darwin"
	if $os == "Darwin" {
		file { afile: }
	}
	if $os != "Darwin" {
		file { bfile: }
	}
 }
