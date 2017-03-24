# @FEATURES collectors, collector_order
# @UNSUPPORTED
#% If you have multiple spaceship operators, the last one wins ....
#% Or maybe this is just coincidence in this example. It still isn't clear to me what 
#% order these get applied in.

class main {
  file { "file": owner => "alice" }
  File <| title == "file" |> {
    owner => "bob"
  }  
  File <| title == "file" |> {
    owner => "carol"
  }  
}
