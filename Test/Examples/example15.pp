# @FEATURES resources, resource_basics
#% A \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_resources.html}{resource {\em declaration}}
#% is the only thing which produces output in the catalog (I think).
#% Each \href{https://docs.puppetlabs.com/references/4.1.latest/type.html}{\em type}
#% supports a fixed set of
#% \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_resources.html#attributes}{\em attributes}
#% (sometimes called {\em parameters} or {\em properties}).
#% The \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_resources.html#title}{\em title}
#% must be unique (per type) and is used to reference the resource.

class main {
  file { "file1": owner => "alice" }
  file { "file2": content => "the content" }
  package { "webservice": source => "the source" }
}
