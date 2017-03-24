#@FEATURES resources, userdef
# @EXPECT_FAIL 

define foo ($x = 1) {
  bar {$title: y => $x}
}

define bar ($y = 1) {
  foo {$title: x => $y}
}
class main {

  foo {"test" : x => 0}
}

