class main::sub {
  $x = "alice"
}

class main inherits main::sub {
  file {"foo" : owner => $::main::sub::x }
}

