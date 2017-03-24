define foo ($x = "alice") {
  file { $title : owner => $x}
}

class main {
  foo {"XXX" : x => "bob"}
}

