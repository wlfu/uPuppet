# @FEATURES collectors, collector_basics
# @UNSUPPORTED
#% This ``spaceship'' construct $|>$ {\em query} $<|$ is a
#% ``\href{https://docs.puppetlabs.com/puppet/latest/reference/lang_collectors.html}{resource collector}''.
#% It allows you to override a resource value ({\tt owner}, in this case).
#% The manu says ``{\em Note that this idiom must be used carefully, if at all}''.
#% But I think this this is the only way (apart from inheritance) of overriding a value.
#% \danger{I think that the spaceship operator gets applied in a separate pass after
#% the initial catalog has been generated, which gives the query global scope. In fact 
#% it feels very much like a database query running on the generated resources. But I'm
#% not clear on the semantics - for example, I don't understand the application order.}

class main {
	file { "file": owner => "alice" }
	File <| title == "file" |> {
		owner => "bob",
		group => "the group",
	}  
}
