# @FEATURES collectors, collector_variables
# @UNSUPPORTED
#% This seems to confirm that the resource collector is applied late.

class main inherits main::parent {
	File["file1"] { mode => 0755 }
}

class main::parent {
	file { "file1": owner => "alice" }
	File <| mode == 0755 |> {
		mode => 0700
	}
}
