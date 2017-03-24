# @FEATURES resources, defaults
# @UNSUPPORTED
#% \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_defaults.html}{Defaults}
#% can be set for an entire type (use the capitalized resource name).
#% Attributes without an explicit value, or a default don't appear in the catalog.
#% The semantics of this needs a bit of investigation ...
#% \danger{
#% {\em Puppet still uses dynamic scope for resource defaults, even though it no longer uses
#% dynamic variable lookup. This means that if you use a resource default statement in a class,
#% it has the potential to affect any classes or defined types that class declares.}}

class main () {
	File { owner => "alice" }
	file { "file1": }
	file { "file2": owner => "bob" }
}
