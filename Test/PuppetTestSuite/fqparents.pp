#@UNSUPPORTED
class base {
    class one {
        file { "/tmp/fqparent1": ensure => file }
    }
}

class two::three inherits base::one {
    file { "/tmp/fqparent2": ensure => file }
}

class main {
include two::three
}