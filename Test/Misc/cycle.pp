#@FEATURES classes, class_inheritance
# @EXPECT_FAIL - non-terminating

class a inherits b {
  $x = 1
}
class b inherits a {
  $y = 2
}

class main {
  include a
}
