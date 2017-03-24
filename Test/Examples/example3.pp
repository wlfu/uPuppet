# @FEATURES classes, class_inheritance
#% Classes can (instance)
#% \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_classes.html#inheritance}{inherit}
#% other classes, and extend the parent resources.
#% The class inheritance mechanism actually seems to be fairly straightforward (compared to some
#% other features!) but the manual says: ``{\em Class inheritance should be used very sparingly}''
#% and ``{\em .... in nearly all other cases, inheritance is unnecessary complexity}''.

class main inherits main::parent {
	file { "file1": owner => "alice" }
}

class main::parent {
	file { "file2": owner => "bob" }
}
