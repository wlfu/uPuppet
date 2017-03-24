# @FEATURES classes, class_inheritance
#% Inheritance composes as expected.

class main inherits main::parent {
}

class main::parent inherits main::grandparent {
}

class main::grandparent {
	file { "file": owner => "alice" }
}
