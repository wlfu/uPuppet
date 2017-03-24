# @EXPECT_FAIL

define foo ($x = 1, $x = 2) {
}

class main {
  foo {"asdf": x => 3}
}

