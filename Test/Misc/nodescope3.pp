#@FEATURES nodes
# @OPTIONS --strict_variables


define foo ($x = 1) {
  include a
}
class a {
  user { "alice": comment => $comment }
}
class main {
}

node default {
  $comment = "the comment"
  foo {"asdf": x => 2}
}
