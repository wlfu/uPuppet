# @FEATURES resources, userdef
#% \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_defined_types.html}{Defined resource types}
#% are like macros. They are declared in the same way as normal resources, but the the body is expanded with
#% the parameters.
#% \danger{There are probably quite a lot of questions that we could ask about the semantics of these
#% defined resources types and how they interact with the other elements of the language ...}

class main {
	main::myuser { one: id => "alice", n => 1 }
	main::myuser { two: id => "bob", n => 2 }
}

define main::myuser ( $id = 0, $n = 0 ) {
       user { $id : uid => $n }
}
