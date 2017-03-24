#@EXPECT_FAIL
class main {
exec { "touch /tmp/emptyexectest":
    path => "/usr/bin:/bin"
}
}