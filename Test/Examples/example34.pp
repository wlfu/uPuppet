# @FEATURES classes, class_variables
#% This is rather suprising (but good) - the {\tt \$comment} on the LHS in the parent
#% class references the local copy, and the one on the RHS references
#% the parent copy.

class main inherits main::parent {
	user { "alice": comment => $comment }
}

class main::parent inherits main::grandparent {
	$comment = $comment
}

class main::grandparent {
	$comment = "grandparent"
}
