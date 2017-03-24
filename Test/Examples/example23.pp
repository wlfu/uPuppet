# @FEATURES collectors, collector_order
# @UNSUPPORTED
#% The application order of multiple spaceship operators isn't clear.
#% If the resource collectors had been applied in the order they appear in the source
#% then, I would have expected all the owners to be {\tt person1}.
#% So Spaceship application order is not source order.

class main {
	file { "file": owner => "alice" }
	include main::sub1
	include main::sub2
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
