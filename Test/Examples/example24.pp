# @FEATURES collectors, collector_order
# @UNSUPPORTED
#% Spaceship application order is not inclusion order.
#% This is probably expected from Figure \ref{exfig:example23}.

class main {
	file { "file": owner => "alice" }
	include main::sub2
	include main::sub1
}

class main::sub2 {
	file { "file2": owner => "alice" }
	File <| owner == "bob" |> {
		owner => "carol"
	}  
}

class main::sub1 {
	file { "file1": owner => "alice" }
	File <| owner == "alice" |> {
		owner => "bob"
	}  
}
