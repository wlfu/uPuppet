# @FEATURES classes, class_nesting
# @UNSUPPORTED
#% More stupid class tricks: multiple definitions of a class are OK, 
#% and contribute to its contents

class main {
  include a
  file { "a" : owner => $::a::x }
  file { "b" : owner => $::a::y  }
}

class a {
  $x = "alice"
}

class a {
  $y = "bob"
}