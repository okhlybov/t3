# ... and now the True Test (tm) Hold fast or expire ...


package provide T3 0.1.0

puts $tcl_interactive
if {$tcl_interactive} {exit}

# Capture current source file name as it is set for the toplevel interpreter only
catch {
  set source $argv0
  # The code below is executed by the toplevel interpreter only
  puts stderr "--- # T3 report stream"
  puts stderr {}
}

package require Tcl 8.6
package require Thread

if {[catch {set |boxed|}]} {

  # The code below is executed by the master interpreter

  # https://wiki.tcl-lang.org/page/lshift
  proc lshift {args} {
      lassign $args listVar count
      upvar 1 $listVar var
      if { ! [info exists var] } {
          return -level 1 -code error [subst {can't read "$listVar": no such variable}]
      }
      switch -exact -- [llength $args] {
        1 { set var [lassign $var value] }
        2 {
            set value [lrange $var 0 $count-1]
            set var [lrange $var $count end]
        }
        default {
          return -level 1 -code error [subst {wrong # args: should be "lshift listVar ?count?"}]
        }
      }
      return $value
    }

      # Output stream collector
      oo::class create Collector {
        variable stream
        method initialize {handle mode} {
          set stream {}
          return {initialize finalize write}
        }
        method write {handle buffer} {
          append stream $buffer
          return
        }
        method stream {} {
          return $stream
        }
      }

  interp create playground

  try {

    # Flag the playground interpreter to skip configuration and proceed to the user's code
    playground eval set |boxed| 1

    # List of submitted job units
    set jobs [list]

    set pool [tpool::create -initcmd {

      package require TclOO

      # Output stream collector
      oo::class create Collector {
        variable stream
        constructor {} {my reset}
        method reset {} {set stream {}}
        method stream {} {return $stream}
        method initialize {handle mode} {return {initialize finalize write}}
        method write {handle buffer} {
          append stream $buffer
          return
        }
      }

      Collector create |stdout|
      chan push stdout |stdout|

      Collector create |stderr|
      chan push stderr |stderr|

    }]

    # Entities shared among the Unit's instances
    namespace eval Unit {
      # Counter used to generate default unit names
      set index 0
      # Unit constuctor's creation error message
      set usage {usage: ?-name ...? ?-tags {...}? ?-setup {code}? ?-cleanup {code}? code}
      # List of created units
      set units [list]
    }

    #
    oo::class create Unit {
      #
      constructor {args} {
        my variable name tags setup cleanup code
        set name #[incr Unit::index]
        set tags [list]
        set setup {}
        set cleanup {}
        set params [list]
        while {[llength $args] > 0} {
          switch -glob [lindex $args 0] {
            -name {lassign [lshift args 2] _ name}
            -tags {lassign [lshift args 2] _ tags}
            -setup {lassign [lshift args 2] _ setup}
            -cleanup {lassign [lshift args 2] _ cleanup}
            -* {error $Unit::usage}
            default {lappend params [lshift args]}
          }
        }
        if {[llength $params] != 1} {error $Unit::usage}
        set code [lindex $params 0]
        lappend Unit::units [self]
      }
      # Render the script to be executed in the pooled thread context
      method script {} {
        my variable name tags setup cleanup code
        set unit [self]
        subst -nocommands {
          # Standard channel redirectors are reused across the pool's threads therefore
          # they should be cleared of the previous invokation contents
          |stdout| reset
          |stderr| reset
          # Run the user-supplied code
          set outcome [catch {
            $setup
            try {
              $code
            } finally {
              $cleanup
            }
          } result options]
          # Yield the run report
          return [dict merge \$options [dict create -unit $unit -stdout [|stdout| stream] -stderr [|stderr| stream] -result \$result]]
        }
      }
      # Process the outcome of the script execution
      method report {outcome} {
        variable name
        puts stderr "  - $name"
        dict with outcome {
          if {${-code}} {
            puts stderr "    outcome: FAILURE"
          } else {
            puts stderr "    outcome: SUCCESS"
          }
        }
        flush stderr
      }
    }

    # Proc exposed to the custom script which defines new units
    proc unit {args} {
      upvar pool pool
      upvar jobs jobs
      lappend jobs [tpool::post $pool [[Unit new {*}$args] script]]
    }

    playground alias unit unit

    try {

      # Re-evaluate current source on the playground

      if {[catch {interp eval playground [subst -nocommands {source [set source {$source}]}]} result options]} {
        puts stderr [dict get $options -errorinfo]
      } else {
        # Process the results of completed jobs
        while {[llength $jobs] > 0} {
          foreach job [tpool::wait $pool $jobs jobs] {
            set outcome [tpool::get $pool $job]
            [dict get $outcome -unit] report $outcome
          }
        }
      }

      puts stderr {}

    } finally {
      tpool::release $pool
    }

    # Handle subprojects
    foreach source [glob -nocomplain -type f -tails -directory [file dirname $source] [file join * [file tail $source]]] {
      set wd [pwd]
      try {
        interp create descent
        try {
          interp eval descent [subst -nocommands { source [set source {$source}] }]
        } finally {
          interp delete descent
        }
      } finally {
        cd $wd
      }
    }

  } finally {
    interp delete playground
  }

  exit

} else {

  # This part precedes the user-supplied code evaluated on the playground

  cd [file dirname $source]

  puts stderr "- [pwd]"
  puts stderr "  source: [file tail $source]"
}