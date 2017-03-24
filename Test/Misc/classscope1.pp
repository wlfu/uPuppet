#@OPTIONS --strict_variables
#@FEATURES classes, class_scope

class ssh::params {
 $val = "val"
}

class ssh {
 $params = "hello"
}

#% Whether $ssh::params denotes a variable or a class depends on context?
class blah {
 file {"foo" : owner => $::ssh::params }
 file {"bar" : owner => $::ssh::params::val }
}
class main {
}

node default {
 include ssh
 include ssh::params
 include blah
}