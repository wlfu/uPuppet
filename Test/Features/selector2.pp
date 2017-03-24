#@FEATURES expressions, selector

class main {
  $x = 1 + (2 ? {2 => 10, 3 => 20, default => 30})
  file {"test" : owner => $x}
}
