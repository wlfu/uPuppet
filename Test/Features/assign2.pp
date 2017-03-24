#@FEATURES statements, assignment
# @OPTIONS --strict_variables @EXPECT_FAIL
class main {
  file {"foo" : owner => $y}
  $y = $x
}
  $x = "alice"
