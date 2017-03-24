#@FEATURES nodes
# @OPTIONS --strict_variables 

class basea {
  include a
}
class a {
  user { "alice": comment => $comment }
}
class main {
}

node default {
  $comment = "the comment"
  include basea 
}
