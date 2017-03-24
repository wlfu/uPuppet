#@UNSUPPORTED
# $Id$

class main {
File { mode => "0755" }

file { "/tmp/defaulttest": ensure => "file" }

}