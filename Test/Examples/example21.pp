# @FEATURES collectors, collector_variables
# @UNSUPPORTED
#% The spaceship in the class {\tt spaceship2::sub2} seems to affect
#% the resources in the enclosing class as well as the (unrelated) {\tt sub1},
#% as we woudl expect if the resource collection is a separate pass.

class main {
	file { "file": owner => "alice" }
	include main::sub1
	include main::sub2
}

class main::sub1 {
	file { "file1": owner => "alice" }
}

class main::sub2 {
	file { "file2": owner => "alice" }
	File <| owner == "alice" |> {
		owner => "bob"
	}  
}
