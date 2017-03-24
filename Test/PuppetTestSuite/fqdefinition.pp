define one::two($ensure) {
    file { "/tmp/fqdefinition": ensure => $ensure }
}

class main {
one::two { "/tmp/fqdefinition": ensure => "file" }
}