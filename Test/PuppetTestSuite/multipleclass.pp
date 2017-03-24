#@UNSUPPORTED
class one {
    file { "/tmp/multipleclassone": content => "one" }
}

class one {
    file { "/tmp/multipleclasstwo": content => "two" }
}

class main {
include one

}
