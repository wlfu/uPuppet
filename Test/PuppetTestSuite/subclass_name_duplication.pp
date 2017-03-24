#!/usr/bin/env puppet

class one::fake {
    file { "/tmp/subclass_name_duplication1": ensure => "present" }
}

class two::fake {
    file { "/tmp/subclass_name_duplication2": ensure => "present" }
}

class main {
include one::fake
include two::fake
}