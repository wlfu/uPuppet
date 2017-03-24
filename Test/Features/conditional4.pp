#@FEATURES statements, if

# "The condition(s) of an "if" statement may be any expression that
# resolves to a boolean value.""
# https://docs.puppet.com/puppet/latest/lang_conditional.html#if-statements

class main {
	$x = 1
	$y = 2
	$z = 1
	if $x==1 and $y==3 {
		file { afile: path => "a" }
	}
	if $z==1 or $x==1 and $y==2 {
		file { bfile: path => "b" }
	}
 }
