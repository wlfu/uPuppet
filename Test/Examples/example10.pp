# @FEATURES classes, class_parameters
#% Classes can take parameters which appear as variables inside the class.
#% To declare a class with parameters, you need to use the {\tt class}
#% statement (instead of {\tt include}). The included class can specify a default
#% value for the arguments.

class main {
  class { main::sub : p => "alice" }
  file { "file1": owner => "bob" }
}

class main::sub( $p = "default" ) {
  file { "file2": owner => $p }
}
