# @FEATURES resources, ordering
#% Don't be mislead by the attributes specifying
#% \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_relationships.html}{"ordering"}.
#% This refers to deployment order and passed into the catalog without
#% affecting the compilation, or the order of the items in the catalog.
#% Likewise {\em chaining arrows} ($\sim>$) and
#% \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_run_stages.html}{\em Run stages}
#% and \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_containment.html}{\em Containment}.
#% {\em None of this is relevant to the compilation.}

class main {
  file { "file1": owner => "alice" }
  file { "file2":
  	owner => "bob",
  	before => File["file1"]
  }
}
