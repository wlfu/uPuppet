class main {

  $x = {a => "alice", b => "bob"}

  file { "file1" : owner => $x["a"]}
  file { "file2" : owner => $x["b"]}

}