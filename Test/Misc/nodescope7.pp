#@FEATURES nodes
# @OPTIONS --strict_variables @EXPECT_FAIL
#% If a name is not found in a class scope with an inheritance edge, lookup
#% proceeds into node scope.


define foo ( $x = 1)  {
  user { "alice": comment => $comment }
}


node default {
  $comment = "the comment"
}

foo {"asdf" : x => 2}

class main {

}