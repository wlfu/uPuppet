# @FEATURES classes, class_parameters
#% It is possible to declare a parent class with a parameter.
#% But there is no way to provide this parameter when instantiating the child class!

class main inherits main::parent {
	file { "file1": owner => "alice" }
}

class main::parent ( $owner = "bob" ) {
	file { "file2": owner => $owner }
}
