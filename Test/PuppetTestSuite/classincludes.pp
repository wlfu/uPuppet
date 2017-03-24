# $Id$

class base {
    file { "/tmp/classincludes1": ensure => "file", mode => "0755" }
}

class sub1 inherits base {
    file { "/tmp/classincludes2": ensure => "file", mode => "0755" }
}

class sub2 inherits base {
    file { "/tmp/classincludes3": ensure => "file", mode => "0755" }
}

# $sub = "sub2"

class main {
include sub1
include sub2
}
