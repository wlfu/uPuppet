# @FEATURES classes, class_overriding
# @UNSUPPORTED
#% Subclasses can add attributes to inherited resources.

class main inherits main::parent {
	File["file"] { mode => 0644 }
}

class main::parent {
	file { "file": owner => "alice" }
}
