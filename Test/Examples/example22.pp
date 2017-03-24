# @FEATURES collectors, collector_order
# @UNSUPPORTED
#% Multiple spaceship operators all get applied ...

class main {
	file { "file": owner => "alice" }
	include main::sub1
	include main::sub2
}

class main::sub1 {
	file { "file1": owner => "alice" }
	File <| owner == "alice" |> {
		owner => "bob"
	}  
}

class main::sub2{
	file { "file2": owner => "alice" }
	File <| owner == "alice" |> {
		owner => "bob"
	}  
}
