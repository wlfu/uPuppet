#@FEATURES statements, unless
# "The condition(s) of an “unless” statement may be any expression that
# resolves to a boolean value.""
# https://docs.puppet.com/puppet/latest/lang_conditional.html#unless-statements

class main {
	$x = 1
	$y = 2
	$z = 1
	unless $x==1 and $y==3 {
		file { afile: path => "a" }
	}
	unless $z==1 or $x==1 and $y==2 {
		file { bfile: path => "b" }
	}
 }
