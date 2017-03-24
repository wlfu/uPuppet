define test($a = { b => "c" }) {
    file {
        $a["b"]:
            ensure => "file", content => "content"
    }
}

class main {
$hash = { file => "/tmp/myhashfile1" }

file {
    $hash["file"]:
        ensure => "file", content => "content"
}

$hash2 = { a => { key => "/tmp/myhashfile2" }}

file {
    $hash2["a"][key]:
        ensure => "file", content => "content"
}


$hash3 = { mykey => "/tmp/myhashfile4" }
$key = "mykey"

file {
    $hash3[$key]: ensure => "file", content => "content"
}

test {
    "test":
        a => { b => "/tmp/myhashfile3" }
}


}