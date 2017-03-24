# @FEATURES classes, class_nesting
# @EXPECT_FAIL
#% Stupid class tricks: multiple definitions of a class are OK, 
#% ergo multiple definitions of variables in the same class are forbidden

class main {
  include a
  user { $::a::x : }
}

class a {
  $x = "alice"
}

class a {
  $x = "bob"
}
