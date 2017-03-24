# @FEATURES classes, class_overriding
# @UNSUPPORTED
#% The $+>$ operator allows items to be appended to an existing list value.
#% If you append an item to a scalar value, it gets automatically converted to a list.
#% As an aside ... would this fail if we tried to deploy it because the owner
#% is a list, and not a simple string ?

class main inherits main::parent {
	File["file"] { owner +> "alice" }
}

class main::parent {
	file { "file": owner => "bob" }
}
