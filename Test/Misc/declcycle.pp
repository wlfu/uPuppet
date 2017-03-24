#@FEATURES classes, class_basics
class a {
  include b
}

class b {
  include a
}

class main {
  include a
}