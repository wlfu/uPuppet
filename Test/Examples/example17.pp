# @FEATURES resources, resource_extension
# @UNSUPPORTED
# @EXPECT_FAIL
#% Attributes cannot (normally) be redefined using a reference. But see figure \ref{exfig:example6}.

class main {
  file { "file": owner => "alice" }
  File["file"] { owner => "bob" }
}
