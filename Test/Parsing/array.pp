class main {

  $x = ["alice","bob"]
  file { "file1" : owner => $x[0]}
  file { "file2" : owner => $x[1]}
  }