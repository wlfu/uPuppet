# @FEATURES resources, userdef
# @EXPECT_FAIL
#% Apparently arbitrary recursion among defined resources is allowed...

define a ($x = 1) {
  b {"foo$x": y => $x + 1}
}

define b($y = 2) {
  a {"bar$y":  x => $y + 1}

}

class main {
  a {"xyz": x => 1 }
}

