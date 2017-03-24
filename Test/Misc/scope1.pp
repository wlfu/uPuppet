#@FEATURES resources, userdef
# @OPTIONS --strict_variables @EXPECT_FAIL

define foo ($x = "alice") {
  file { "foo.txt" :
       	 owner => $x,
         source => $source
  }
}

class main  {
  $source = "asdf"
  foo { "xyz" : x => "bob" }
}
