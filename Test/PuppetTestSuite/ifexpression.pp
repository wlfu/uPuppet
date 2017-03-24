$one = 1
$two = 2

class main {
if ($one < $two) and (($two < 3) or ($two == 2)) {
    file{"True!" :}
}
}

#if "test regex" =~ /(.*) regex/ {
#    file {
#        "/tmp/${1}iftest": ensure => file, mode => '0755'
#    }
#}
