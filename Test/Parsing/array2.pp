class main {

    $x = {1=>"alice", 2=>"bob"}
    $y = [2, 4, 5, $x] 
    file { "file1" : owner => $y[1]}
    file { "file2" : owner => $y[3][2]}

}
