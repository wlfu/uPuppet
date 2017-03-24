# @FEATURES classes, class_nesting
# @UNSUPPORTED
#% I'm not sure what this might mean ... but it seems to be illegal anyway ...

class main {
  class main::sub {
  	file { 'file1': owner => "alice" }
  }
  file { 'file2': owner => "bob" }
}
