#@FEATURES nodes
# @OPTIONS --strict_variables @EXPECT_FAIL


define bar ($x = 1) {
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
bar{"asdf" : x => 3}
