$mode = "640"

define thing {
    file { $name: ensure => "file", mode => $mode }
}

class testing {
    $mode = "755"
    thing {scopetest: }
}

class main {

include testing

}