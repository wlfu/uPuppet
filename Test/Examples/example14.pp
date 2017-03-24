# @FEATURES classes, class_variables
#% Local variables override inherited ones.

class main () inherits main::parent {
	$comment = "the comment"
	user { "alice": comment => $comment }
}

class main::parent {
	$comment = "another comment"
}
