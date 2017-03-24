
define foo ($x="test",$y) {
  file {$x : owner => $y}
}


class main {
  foo { "bar" : x => "asdf", y => "alice"}
}
 
