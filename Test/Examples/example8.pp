# @FEATURES classes, class_basics
#% Because the classes are just included in the flat catalog, this kind of 
#% apparently mutual recursion compiles fine .. (slightly suprising).

class main {
	file { "file": }
	include main::sub1
}

class main::sub1 {
	file { "file1": }
	include main::sub2
}

class main::sub2 {
	file { "file2": }
	include main::sub1
}
