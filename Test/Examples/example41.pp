# @FEATURES classes, class_variables
#% Qualified names can be used to reference variables from other classes, providing
#% that they have been defined (lexically earlier).

class main () {
	$c0 = "main"
	include main::sub1
	include main::sub2
}

class main::sub1 {
	$c0 = $::main::c0
	$c1 = "sub1"
	user { "b1": comment => $c0 }
	user { "b2": comment => $c1 }
}

class main::sub2 {
	$c0 = $::main::c0
	$c1 = $::main::sub1::c1
	$c2 = "sub2"
	user { "c1": comment => $c0 }
	user { "c2": comment => $c1 }
	user { "c3": comment => $c2 }
}
