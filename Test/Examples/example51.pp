# @FEATURES classes, class_scope
# @UNORDERED

class main {
  foo { "foo" :}
  include bar
  $y = 17
  baz { "baz" :}
  include xyzzy
}

define foo ($x = $y) {
  file { "define1": owner => $x}
}

class bar ($x = $y) {
  file { "include2" : owner => $x}
}

$y = 42


define baz ($x = $y) {
  file { "define3": owner => $x}
}

class xyzzy ($x = $y) {
  file { "include4" : owner => $x}
}


