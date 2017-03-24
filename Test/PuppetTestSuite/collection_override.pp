#@UNSUPPORTED

class main {

@file {
    "/tmp/collection":
        content => "whatever"
}

File<| |> {
    mode => '0600'
}

}