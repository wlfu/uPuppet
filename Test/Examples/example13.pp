# @FEATURES classes, class_variables
#% Variables inherit from the parent class.

class main () inherits main::parent {
	user { "alice": comment => $comment }
}

class main::parent {
	$comment = "the comment"
}
