# @FEATURES classes, class_nesting
# @UNSUPPORTED
#% Stupid class tricks: multiple definitions of a class from different nesting levels are OK, 
#% and contribute to the same class provided no names are redefined

class main {
  include a
  include a::b
  user { $a::x1 :}
  user { $a::b::y1 :}
  user { $a::b::y2 :}
}

class a {
  $x1 = "a"
  class b {
    $y1 = "b"
  }
}

class a::b {
  $y2 = "ab"
}