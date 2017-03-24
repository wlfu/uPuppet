#@FEATURES statements, assignment

class main {
  $x = "alice"
  $y = $x
  file {"foo" : owner => $y}
}
