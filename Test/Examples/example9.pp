# @FEATURES classes, class_variables
#% Variables are scoped to the class.

class main {
	$owner = "alice"
	file { "file": owner => $owner }
	include main::sub
}

class main::sub {
        $owner = "bob"
	file { "file1": owner => $owner, mode => "0644" }
}
