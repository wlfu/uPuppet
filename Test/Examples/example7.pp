# @FEATURES classes, class_basics
#% Including the same class multiple times is allowed (it only appears once in the catalog).
#% But see figure \ref{exfig:example30}.

class main {
	file { "file": }
	include main::sub
	include main::sub
}

class main::sub {
	file { "file1": }
}
