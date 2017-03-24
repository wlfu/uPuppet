#@FEATURES expressions, selector
# basic selector
# https://docs.puppet.com/puppet/latest/lang_conditional.html#selectors

class main {
	$os = "Darwin"
	$owner = $os ? {
		"Solaris"	=> "wheel",
		"Darwin"	=> "wheel",
		default		=> "root"
	}
	file { afile:
	    owner => $owner
  	}
}
