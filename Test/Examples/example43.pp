# @FEATURES resources, virtual_resources
# @UNSUPPORTED
#% \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_virtual.html}{Virtual resources} 
#% allow a resource to be declared in one place without the resource being included
#% in the catalog. The resource can then be {\em realised} in one or more other places to include
#% it in the catalog. Notice that you can realize virtual resources before declaring them.

class main {
	realize User["alice"]
	@user { "alice": uid => 100 }
	@user { "bob": uid => 101 }
	realize User["alice"]
}
