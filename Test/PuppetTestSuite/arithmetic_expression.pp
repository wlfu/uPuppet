#@UNSUPPORTED
$one = 1.30
$two = 2.034e-2

$result = ((( $two + 2) / $one) + 4 * 5.45) - (6 << 7) + (0x800 + -9)

class main {
notice("result is $result == 1295.87692307692")
}