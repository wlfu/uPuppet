# @FEATURES classes, class_nesting
# @UNSUPPORTED
#% \href{http://docs.puppetlabs.com/puppet/4.2/reference/lang_classes.html#other-locations}{Nested
#% classes} are allowed, and variables defined in a nested class can be
#% accessed using the scope qualifier `::'.

#% \danger{According to the Puppet style guide, class nesting is
#% discouraged; the same effect is supposedly obtainable by using explicit
#% namespaces on class definitions.}


class main {
  include a
  include a::b::c
  file {"ax" : 
    mode => $a::x
  }

  file {"abx":
    mode =>  $a::b::x
  }

  file {"abcx": 
    mode => $a::b::c::x
  }
}


class a {
  $x = 0700
  class b {
    $x = 0770
    class c {
      $x = 0777
    }
  }
}


