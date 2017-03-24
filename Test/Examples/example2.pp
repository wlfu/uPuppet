# @FEATURES classes, class_parameters
#% Parameter values override inherited values.

class main ( $p = "default" ) inherits main::parent {
	user { "alice": password => $p }
}

class main::parent {
	$p = "the password"
}
