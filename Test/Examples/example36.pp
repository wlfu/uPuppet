# @FEATURES collectors, collector_variables
# @UNSUPPORTED
#% Variables are evaluated in the resource collector when it is defined.
#% (which we would probably expect).

class main {
	$fname = "file1"
	include main::sub
	file {"file1": owner => "alice" }
	file {"file2": owner => "alice" }
}

class main::sub {
	$fname = "file2"
	File <| title == $fname |> {
		owner => "bob",
	}
}
