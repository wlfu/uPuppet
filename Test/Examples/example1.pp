# @FEATURES classes, class_basics
#% The name {\tt Class} is a bit misleading.
#% A \href{https://docs.puppetlabs.com/puppet/latest/reference/lang_classes.html}{Puppet class}
#% is just a way of grouping together a collection of resources, and you can then 
#% {\em declare} the class to include all of its content in the catalog.
#% All of the resources in the catalog appear in a flat list and there is no record of any class hierarchy.

class main {
  include main::sub
  file { "file1": owner => "alice" }
}

class main::sub  {
  file { "file2": owner => "bob" }
}
