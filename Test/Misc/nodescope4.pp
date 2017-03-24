#@FEATURES nodes
# @OPTIONS --strict_variables @EXPECT_FAIL

class baseb {
  include b
}

class b {
  user { "bob": comment => $comment }
}

class main {
}

node default {
  $comment = "the comment"
}

include baseb
