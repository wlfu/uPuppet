class main {
  file {"foo.txt" : owner => "alice"}
  $y = "foo.txt"
  $x = File[$y]
  file {"bar.txt" : owner => $x["owner"]}
}

include main
