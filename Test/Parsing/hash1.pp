class main {
 
    $x = [2, 4, 5, 6] 
    $y = {1=>"alice", 2=>$x}
    $z = {a=>"sun", b=>$y, c=>$y[1]}
    file { "file1" : owner => $z[c]}
    file { "file2" : owner => $z[b][2]}

}


