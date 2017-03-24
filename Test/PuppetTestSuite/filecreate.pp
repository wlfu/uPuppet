# $Id$

class main {
file {
    "/tmp/createatest": ensure => "file", mode => "0755"
    }
file {
    "/tmp/createbtest": ensure => "file", mode => "0755"
}

file {
    "/tmp/createctest": ensure => "file"
    }
file {"/tmp/createdtest": ensure => "file"
}
}