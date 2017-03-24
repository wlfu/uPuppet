# @FEATURES classes, class_parameters, resources, userdef
# @UNORDERED

class main {
  foo { "foo" : x => "abcd"}
  class { bar : x => "efgh"}

  baz { "baz" : }
  class { xyzzy : }
}

define foo ($x = "wwww") {
  file { "define1" : owner => $x }
}

class bar ($x = "xxxx") {
  file { "class2" : owner => $x}
}



define baz ($x = "yyyy") {
  file { "define3": owner => $x }
}

class xyzzy ($x = "zzzz") {
  file { "class4" : owner => $x}
}


