# @FEATURES resources, resource_basics
# @EXPECT_FAIL
#% A resource cannot be redefined with the same name.
#% The title/type combination must be globally unique - there is
#% \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_resources.html#scope-independence}{no local scoping}.

class main {
  file { "file1": owner => "alice" }
  file { "file1": owner => "bob" }
}
