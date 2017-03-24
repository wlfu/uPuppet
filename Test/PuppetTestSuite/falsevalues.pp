#@UNSUPPORTED
class main {

$value = false

file { "/tmp/falsevalues$value": ensure => "file" }
}