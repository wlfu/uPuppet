# @FEATURES resources, variables
#% Conditionals \& other operators are implemented in fairly obvious way.

$operatingsystem = "darwin"

class main {
      
	if $::operatingsystem == "centos" { $grp = "wheel" }
        elsif $::operatingsystem == "debian" { $grp = "admin" }
        else { $grp = "unknown" }
        file { "file": group => $grp }
 }
