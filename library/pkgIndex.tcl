# Provide hand-crafted package loading script as
# the T3 package is not detectable by the pkg_mkIndex
package ifneeded T3 0.1.0 [list source [file join $dir t3.tcl]]