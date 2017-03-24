#@FEATURES expressions, expr_if
# @UNSUPPORTED
# "The value of an unless expression is the value of the last expression in
# the executed block, or undef if no block was executed.""
# https://docs.puppet.com/puppet/latest/lang_conditional.html#if-statements

class main {
	$os = "Darwin"
	$f = if $os == "Darwin" { "f1" }
	else { "f2" }
	file { bfile: path => $f }
 }
