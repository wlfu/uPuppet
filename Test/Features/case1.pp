#@FEATURES statements, case
# basic case statement
# https://docs.puppet.com/puppet/latest/lang_conditional.html#case-statements

class main {
	$os = "Darwin"
	case $os {
		"Solaris":	{ file { afile: owner => "wheel" } }
		"Darwin":	{ file { afile: owner => "wheel" } }
		default	:	{ file { afile: owner => "root" } }
	}
 }
