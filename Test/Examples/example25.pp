# @FEATURES collectors, collector_order
# @UNSUPPORTED
#% Spaceship application order is not based on lexical order of classnames.
#% So I don't know what is is based on!

class main {
	file { "file": owner => "alice" }
	include main::subx2
	include main::suby1
}

class main::subx2 {
	file { "file2": owner => "alice" }
	File <| owner == "person1" |> {
		owner => "carol"
	}  
}

class main::suby1 {
	file { "file1": owner => "alice" }
	File <| owner == "alice" |> {
		owner => "bob"
	}  
}
