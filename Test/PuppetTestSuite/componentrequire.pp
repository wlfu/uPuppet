define testfile($mode) {
    file { $name: mode => $mode, ensure => "present" }
}

class main {

file { "/tmp/testing_component_requires1": mode => "0755", ensure => "present",
    require => Testfile["/tmp/testing_component_requires2"] }

testfile { "/tmp/testing_component_requires2": mode => "0755" }
}