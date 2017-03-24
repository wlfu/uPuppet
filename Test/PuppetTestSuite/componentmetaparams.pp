
define thing () {
    file { $name: ensure => "file" }
}

class main {

file { "/tmp/component1":
    ensure => "file"
}

thing { "/tmp/component2":
    require => File["/tmp/component1"]
}

}